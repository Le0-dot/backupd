import asyncio
from collections.abc import Awaitable, Callable, Iterable
from itertools import filterfalse
from pathlib import Path
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


async def make_docker():
    docker = Docker()
    try:
        yield docker
    finally:
        await docker.close()


DockerClient = Annotated[Docker, Depends(make_docker)]


async def asyncmap[T, U](
    func: Callable[[T], Awaitable[U]], values: Iterable[T]
) -> list[U]:
    return [await func(value) for value in values]


async def volume_exists(docker: Docker, volume_name: str) -> bool:
    try:
        volume = await docker.volumes.get(volume_name)
        model = await Volume.from_wrapped(volume)
    except DockerError:
        return False

    return not model.is_anonymous()


async def volumes(docker: Docker) -> list[str]:
    response = await docker.volumes.list(docker)
    volume_list = VolumeList.model_validate(response)
    named = filterfalse(Volume.is_anonymous, volume_list.Volumes)
    names = map(lambda volume: volume.Name, named)
    return list(names)


async def volumes_from(docker: Docker, container_name: str) -> list[str] | None:
    try:
        container_wrapped = await docker.containers.get(container_name)
        container_model = await Container.from_wrapped(container_wrapped)
    except DockerError:
        return None

    volumes_wrapped = await asyncmap(docker.volumes.get, container_model.volumes())
    volume_models = await asyncmap(Volume.from_wrapped, volumes_wrapped)
    volumes_named = filterfalse(Volume.is_anonymous, volume_models)

    return [volume.Name for volume in volumes_named]


async def container_exists(docker: Docker, container_name: str) -> bool:
    try:
        _ = await docker.containers.get(container_name)
    except DockerError:
        return False

    return True


async def containers(docker: Docker) -> list[str]:
    containers = await docker.containers.list()
    models = await asyncmap(Container.from_wrapped, containers)
    names = map(lambda container: container.Name, models)
    return [name.removeprefix("/") for name in names]


async def configure_container(
    docker: Docker,
    /,
    *,
    image: str,
    entrypoint: str | None = None,
    cmd: list[str] | None = None,
    env: list[str] | None = None,
    volumes: dict[str, Path] | None = None,
    binds: dict[Path, Path] | None = None,
) -> ContainerCreate:
    # TODO: Validate volumes
    volume_mounts = [
        Mount(Source=volume, Target=str(container_path), Type="volume", ReadOnly=False)
        for volume, container_path in (volumes or {}).items()
    ]
    bind_mounts = [
        Mount(
            Source=str(host_path),
            Target=str(container_path),
            Type="bind",
            ReadOnly=False,
        )
        for host_path, container_path in (binds or {}).items()
    ]
    return ContainerCreate(
        Image=image,
        Entrypoint=entrypoint,
        Cmd=cmd,
        Env=env,
        HostConfig=HostConfig(Mounts=volume_mounts + bind_mounts),
    )


async def start_and_wait(
    docker: Docker, /, *, name: str, config: ContainerCreate, timeout: float
) -> tuple[bool, str]:
    container = await docker.containers.run(
        config.model_dump(exclude_none=True), name=name
    )

    try:
        result_json = await asyncio.wait_for(container.wait(), timeout=timeout)
        result = ContainerWait.model_validate(result_json)
        success = result.StatusCode == 0
    except asyncio.TimeoutError:
        success = False
        await container.stop()

    logs = await container.log(stdout=True, stderr=True)

    await container.delete()

    return success, "".join(logs)
