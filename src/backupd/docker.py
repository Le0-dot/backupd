import asyncio
from collections.abc import Awaitable, Callable, Iterable
from itertools import filterfalse
from typing import Annotated, Literal, NamedTuple, Self

from aiodocker import Docker, DockerError
from aiodocker.docker import DockerContainer, DockerVolume
from fastapi import Depends
from pydantic import BaseModel, Field

from backupd.settings import Settings


async def asyncmap[T, U](
    func: Callable[[T], Awaitable[U]], values: Iterable[T]
) -> Iterable[U]:
    return [await func(value) for value in values]


class Volume(BaseModel):
    Name: str
    Driver: str
    Labels: dict[str, str] | None = None

    @classmethod
    async def from_wrapped(cls, wrapper: DockerVolume) -> Self:
        raw = await wrapper.show()
        return cls.model_validate(raw)

    def is_anonymous(self) -> bool:
        return "com.docker.volume.anonymous" in (self.Labels or {})


class VolumeList(BaseModel):
    Volumes: list[Volume]
    Warnings: list[str] | None = None


class VolumeMountPoint(BaseModel):
    Type: Literal["volume"]
    Name: str


class OtherMountPoint(BaseModel):
    Type: Literal["bind", "image", "tmpfs", "npipe", "cluster"]


class Container(BaseModel):
    Id: str
    Name: str
    Mounts: list[
        Annotated[VolumeMountPoint | OtherMountPoint, Field(discriminator="Type")]
    ]

    @classmethod
    async def from_wrapped(cls, wrapper: DockerContainer) -> Self:
        raw = await wrapper.show()
        return cls.model_validate(raw)

    def volumes(self) -> Iterable[str]:
        for mount in self.Mounts:
            if isinstance(mount, VolumeMountPoint):
                yield mount.Name


class Client:
    def __init__(self, docker: Docker | None = None) -> None:
        self.docker: Docker = docker or Docker()

    async def close(self) -> None:
        await self.docker.close()

    async def volume_exists(self, volume_name: str) -> bool:
        try:
            volume = await self.docker.volumes.get(volume_name)
            model = await Volume.from_wrapped(volume)
        except DockerError:
            return False

        return not model.is_anonymous()

    async def volumes(self) -> list[str]:
        response = await self.docker.volumes.list()
        volume_list = VolumeList.model_validate(response)
        named = filterfalse(Volume.is_anonymous, volume_list.Volumes)
        names = map(lambda volume: volume.Name, named)
        return list(names)

    async def volumes_from(self, container_name: str) -> list[str] | None:
        try:
            container_wrapped = await self.docker.containers.get(container_name)
            container_model = await Container.from_wrapped(container_wrapped)
        except DockerError:
            return None

        volumes_wrapped = await asyncmap(
            self.docker.volumes.get, container_model.volumes()
        )
        volume_models = await asyncmap(Volume.from_wrapped, volumes_wrapped)
        volumes_named = filterfalse(Volume.is_anonymous, volume_models)

        return [volume.Name for volume in volumes_named]

    async def container_exists(self, container_name: str) -> bool:
        try:
            _ = await self.docker.containers.get(container_name)
        except DockerError:
            return False

        return True

    async def containers(self) -> list[str]:
        containers = await self.docker.containers.list()
        models = await asyncmap(Container.from_wrapped, containers)
        names = map(lambda container: container.Name, models)
        return [name.removeprefix("/") for name in names]


async def make_client():
    client = Client()
    try:
        yield client
    finally:
        await client.close()


DockerClient = Annotated[Client, Depends(make_client)]


class Mount(BaseModel):
    Target: str
    Source: str
    Type: Literal["bind", "volume", "tmpfs", "npipe", "cluster"]
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
        cls, *, image: str, cmd: str, env: list[str], mounts: list[Mount | None]
    ) -> Self:
        return cls(
            Image=image,
            Entrypoint="sh",
            Cmd=["-c", cmd],
            Env=env,
            HostConfig=HostConfig(Mounts=list(filter(None, mounts))),
        )


class ContainerWait(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerWait
    """

    StatusCode: int


class MountPoint(BaseModel):
    Type: Literal["bind", "volume", "image", "tmpfs", "npipe", "cluster"]
    Name: str | None = None
    Destination: str


class ContainerInspect(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerInspect
    """

    Id: str
    Name: str
    Mounts: list[MountPoint]

    @property
    def name(self) -> str:
        return self.Name.removeprefix("/")

    async def volumes(self, client: Client) -> Iterable[VolumeInspect]:
        volumes = filter(lambda m: m.Type == "volume", self.Mounts)
        names = filter(None, map(lambda m: m.Name, volumes))
        inspect = [await VolumeInspect.by_name(client, name) for name in names]
        filtered = filter(None, inspect)
        return filter(lambda v: not v.anonymous, filtered)

    @classmethod
    async def all(cls, client: Docker) -> list[Self]:
        containers = await client.containers.list()
        raw = [await container.show() for container in containers]
        return [cls.model_validate(item) for item in raw]

    @classmethod
    async def by_name(cls, client: Docker, name: str) -> Self | None:
        try:
            container = await client.containers.get(name)
        except DockerError:
            return None

        raw = await container.show()
        return cls.model_validate(raw)


class ContainerRunResult(NamedTuple):
    success: bool
    stdout: str
    stderr: str


async def run_container(
    client: Docker,
    config: ContainerCreate,
    name: str,
) -> ContainerRunResult:
    container = await client.containers.run(config.model_dump(), name=name)

    settings = Settings()
    try:
        result_json = await asyncio.wait_for(
            container.wait(), timeout=settings.timeout_seconds
        )
        result = ContainerWait.model_validate(result_json)
        success = result.StatusCode == 0
    except asyncio.TimeoutError:
        success = False
        await container.stop()

    stdout = await container.log(stdout=True)
    stderr = await container.log(stderr=True)

    await container.delete()

    return ContainerRunResult(success, "".join(stdout), "".join(stderr))
