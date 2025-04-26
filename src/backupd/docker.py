from typing import Annotated, Any, Literal, Self, TypedDict

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


class Mount(TypedDict):
    Target: str
    Source: str
    Type: Literal["bind", "volume", "tmpfs"]
    ReadOnly: bool


class Container(BaseModel):
    name: Annotated[str, AfterValidator(lambda s: s.removeprefix("/"))]
    volumes: list[str]

    @classmethod
    def from_attrs(cls, attrs: dict[str, Any]) -> Self:
        name = attrs["Name"]
        mounts = attrs["Mounts"]
        volumes = [mount["Name"] for mount in mounts if mount["Type"] == "volume"]

        return cls(name=name, volumes=volumes)


async def container_by_name(name: str, client: Docker) -> Container | None:
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


def configure_run(
    image: str, mounts: list[Mount], env: dict[str, str], preexec: str
) -> dict[str, Any]:
    # mounts = [Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True)]
    return {
        "Image": image,
        "Entrypoint": "sh",
        "Cmd": ["-c", f"{preexec}; restic check"],  # f"backup /data --tag {tag}"
        "Env": [f"{key}={value}" for key, value in env.items()],
        "HostConfig": {"Mounts": mounts},
    }


async def run_backup(
    config: dict[str, Any],
    client: Docker,
) -> tuple[bool, str, str]:
    container = await client.containers.run(config, name="backupd-worker")
    result = await container.wait()

    return (
        result["StatusCode"] == 0,
        "".join(await container.log(stdout=True)),
        "".join(await container.log(stderr=True)),
    )
