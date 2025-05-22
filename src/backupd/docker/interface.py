import asyncio
from collections.abc import Awaitable, Callable, Iterable
from itertools import filterfalse
from typing import Annotated, NamedTuple

from aiodocker import Docker, DockerError
from fastapi import Depends

from backupd.docker.models import (
    Container,
    ContainerCreate,
    ContainerWait,
    HostConfig,
    Mount,
    Volume,
    VolumeList,
)
from backupd.settings import Settings


async def asyncmap[T, U](
    func: Callable[[T], Awaitable[U]], values: Iterable[T]
) -> list[U]:
    return [await func(value) for value in values]


class ContainerRunResult(NamedTuple):
    success: bool
    stdout: str
    stderr: str


class ContainerCreator:
    def __init__(self, docker: Docker, configuration: ContainerCreate) -> None:
        self.docker: Docker = docker
        self.configuration: ContainerCreate = configuration

    async def run_and_wait(self, name: str) -> ContainerRunResult:
        container = await self.docker.containers.run(
            self.configuration.model_dump(exclude_none=True), name=name
        )

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

    def configure(
        self,
        *,
        image: str,
        entrypoint: str | None = None,
        cmd: list[str] | None = None,
        env: list[str] | None = None,
        volumes: dict[str, str] | None = None,
        binds: dict[str, str] | None = None,
    ) -> ContainerCreator:
        volume_mounts = [
            Mount(Source=volume, Target=container_path, Type="volume", ReadOnly=False)
            for volume, container_path in (volumes or {}).items()
        ]
        bind_mounts = [
            Mount(Source=host_path, Target=container_path, Type="bind", ReadOnly=False)
            for host_path, container_path in (binds or {}).items()
        ]
        configuration = ContainerCreate(
            Image=image,
            Entrypoint=entrypoint,
            Cmd=cmd,
            Env=env,
            HostConfig=HostConfig(Mounts=volume_mounts + bind_mounts),
        )
        return ContainerCreator(self.docker, configuration)


async def make_client():
    client = Client()
    try:
        yield client
    finally:
        await client.close()


DockerClient = Annotated[Client, Depends(make_client)]
