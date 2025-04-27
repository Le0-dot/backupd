from typing import Annotated, Any, Literal, Self, TypedDict
from collections.abc import Awaitable, Callable
from contextlib import asynccontextmanager
from functools import wraps
from typing import Annotated, Any, Concatenate, Literal, Self

from aiodocker import Docker, DockerError
from fastapi import Depends
from pydantic import AfterValidator, BaseModel


async def make_client():
    client = Docker()
    try:
        yield client
    finally:
        await client.close()


Client = Annotated[Docker, Depends(make_client)]


def with_client[**P, T](
    callee: Callable[Concatenate[Docker, P], Awaitable[T]],
) -> Callable[P, Awaitable[T]]:
    @wraps(callee)
    async def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
        async with asynccontextmanager(make_client)() as client:
            return await callee(client, *args, **kwargs)

    return wrapper


def log_result[**P](
    callee: Callable[P, Awaitable[dict[str, Any]]],
) -> Callable[P, Awaitable[None]]:
    @wraps(callee)
    async def wrapper(*args: P.args, **kwargs: P.kwargs) -> None:
        result = await callee(*args, **kwargs)
        for key, value in result.items():
            print(f"{key} = {value!r}")

    return wrapper


class Mount(BaseModel):
    Target: str
    Source: str
    Type: Literal["bind", "volume", "tmpfs"]
    ReadOnly: bool


class HostConfig(BaseModel):
    Mounts: list[Mount]


class ContainerCreate(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerCreate
    """

    Image: str
    Entrypoint: str
    Cmd: list[str]
    Env: list[str]
    HostConfig: HostConfig

    @classmethod
    def shell(
        cls, *, image: str, cmd: str, env: dict[str, str], mounts: list[Mount | None]
    ) -> Self:
        return cls(
            Image=image,
            Entrypoint="sh",
            Cmd=["-c", cmd],
            Env=[f"{key}={value}" for key, value in env.items()],
            HostConfig=HostConfig(Mounts=list(filter(None, mounts))),
        )


class Container(BaseModel):
    name: Annotated[str, AfterValidator(lambda s: s.removeprefix("/"))]
    volumes: list[str]

    @classmethod
    def from_attrs(cls, attrs: dict[str, Any]) -> Self:
        name = attrs["Name"]
        mounts = attrs["Mounts"]
        volumes = [mount["Name"] for mount in mounts if mount["Type"] == "volume"]

        return cls(name=name, volumes=volumes)


async def container_by_name(client: Docker, name: str) -> Container | None:
    try:
        container = await client.containers.get(name)
    except DockerError:
        return None

    attrs = await container.show()
    return Container.from_attrs(attrs)


async def list_containers(client: Docker) -> list[Container]:
    containers = await client.containers.list()
    attrs_list = [await container.show() for container in containers]
    return [Container.from_attrs(attrs) for attrs in attrs_list]


@log_result
@with_client
async def run_container(
    client: Docker,
    config: ContainerCreate,
    name: str,
) -> dict[str, Any]:
    container = await client.containers.run(config.model_dump(), name=name)

    # See https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerWait
    result = await container.wait()
    exit_code = result["StatusCode"]

    stdout = await container.log(stdout=True)
    stderr = await container.log(stderr=True)

    await container.delete()

    return {
        "success": exit_code == 0,
        "stdout": "".join(stdout),
        "stderr": "".join(stderr),
    }
