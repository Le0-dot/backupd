import asyncio
from collections.abc import Iterable
import json
from typing import Annotated, Literal, NamedTuple, Self

from aiodocker import Docker, DockerError
from fastapi import Depends
from pydantic import BaseModel, model_validator

from backupd.settings import Settings


async def make_client():
    client = Docker()
    try:
        yield client
    finally:
        await client.close()


Client = Annotated[Docker, Depends(make_client)]


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
    Name: str
    Destination: str


class ContainerInspect(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerInspect
    """

    Id: str
    Name: str
    Mounts: list[MountPoint]

    @property
    def volumes(self) -> Iterable[MountPoint]:
        return filter(lambda m: m.Type == "volume", self.Mounts)

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


class VolumeInspect(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.43/#tag/Volume/operation/VolumeInspect
    """

    Name: str
    Driver: str
    Mountpoint: str
    Labels: dict[str, str] | None = None

    @property
    def anonymous(self) -> bool:
        return "com.docker.volume.anonymous" in (self.Labels or {})

    @classmethod
    async def all(cls, client: Docker) -> list[Self]:
        volumes = await client.volumes.list()
        return [cls.model_validate(volume) for volume in volumes["Volumes"]]

    @classmethod
    async def by_name(cls, client: Docker, name: str) -> Self | None:
        try:
            volume = await client.volumes.get(name)
        except DockerError:
            return None

        raw = await volume.show()
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
