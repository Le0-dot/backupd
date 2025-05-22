from collections.abc import Iterable
from typing import Annotated, Literal, Self

from aiodocker.docker import DockerContainer, DockerVolume
from pydantic import BaseModel, Field


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


class Mount(BaseModel):
    Target: str
    Source: str
    Type: Literal["bind", "volume", "tmpfs", "npipe", "cluster"]
    ReadOnly: bool


class HostConfig(BaseModel):
    Mounts: list[Mount] | None


class ContainerCreate(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerCreate
    """

    Image: str
    Entrypoint: str | None
    Cmd: list[str] | None
    Env: list[str] | None
    HostConfig: HostConfig | None


class ContainerWait(BaseModel):
    """
    https://docs.docker.com/reference/api/engine/version/v1.49/#tag/Container/operation/ContainerWait
    """

    StatusCode: int
