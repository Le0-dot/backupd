from collections.abc import Awaitable, Callable, Iterable
from contextlib import asynccontextmanager
from functools import wraps
from itertools import repeat
from typing import Annotated, Any, Concatenate, Literal, NamedTuple, Self

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


class ContainerWait(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerWait
    """

    StatusCode: int


class Container(BaseModel):
    name: Annotated[str, AfterValidator(lambda s: s.removeprefix("/"))]
    volumes: list[str]

    def iter_volumes(self) -> Iterable[tuple[str, str]]:
        return zip(repeat(self.name), self.volumes)

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


class ContainerRunResult(NamedTuple):
    success: bool
    stdout: str
    stderr: str


@with_client
async def run_container(
    client: Docker,
    config: ContainerCreate,
    name: str,
) -> ContainerRunResult:
    container = await client.containers.run(config.model_dump(), name=name)

    result = ContainerWait.model_validate(await container.wait())
    exit_code = result.StatusCode

    stdout = await container.log(stdout=True)
    stderr = await container.log(stderr=True)

    await container.delete()

    return ContainerRunResult(exit_code == 0, "".join(stdout), "".join(stderr))
