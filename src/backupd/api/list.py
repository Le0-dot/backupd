import logging
from http import HTTPStatus
from pathlib import Path

from aiodocker import Docker
from fastapi import APIRouter, Response
from pydantic import TypeAdapter

from backupd.docker.interface import (
    DockerClient,
    configure_container,
    containers,
    start_and_wait,
    volume_exists,
    volumes,
    volumes_from,
)
from backupd.docker.models import ContainerCreate
from backupd.restic.commands import snapshots
from backupd.restic.flags import GroupFlag, TagFlag
from backupd.restic.models import Snapshot, SnapshotGroupping
from backupd.settings import RepositorySettings, Settings

router = APIRouter(prefix="/list")


async def configure_snapshots(docker: Docker, cmd: list[str]) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()
    return await configure_container(
        docker,
        image=settings.runner_image,
        entrypoint=settings.runner_entrypoint,
        cmd=cmd,
        env=repository.env,
        binds=None
        if repository.restic.backend != "local"
        else {Path(repository.restic.location): Path(repository.restic.location)},
    )


@router.get("/volume")
async def list_all_volumes(client: DockerClient) -> list[str]:
    return await volumes(client)


@router.get("/volume/{name}")
async def list_volume(
    name: str, response: Response, client: DockerClient
) -> str | None:
    if await volume_exists(client, name):
        return name

    response.status_code = HTTPStatus.NOT_FOUND
    return None


@router.get("/container")
async def list_all_containers(client: DockerClient) -> dict[str, list[str]]:
    container_list = await containers(client)
    models = {name: await volumes_from(client, name) or [] for name in container_list}
    return models


@router.get("/container/{name}")
async def list_container(
    name: str, response: Response, client: DockerClient
) -> dict[str, list[str]] | None:
    if (volumes := await volumes_from(client, name)) is not None:
        return {name: volumes}

    response.status_code = HTTPStatus.NOT_FOUND
    return None


@router.get("/snapshot")
async def list_all_snapshots(
    response: Response, client: DockerClient
) -> list[SnapshotGroupping] | None:
    cmd = snapshots(groupping=GroupFlag.tags())
    logging.debug("retrieving snapshots", extra={"cmd": " ".join(cmd)})

    settings = Settings()
    config = await configure_snapshots(client, cmd)
    result = await start_and_wait(
        client, name="backupd-retrieve", config=config, timeout=settings.timeout_seconds
    )

    status = ["failure", "success"][result.success]
    level = [logging.ERROR, logging.INFO][result.success]

    logging.log(level, "finished retrieving snapshots", extra={"status": status})
    logging.debug(result.stdout, extra={"status": status, "stream": "stdout"})
    logging.debug(result.stderr, extra={"status": status, "stream": "stderr"})

    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[SnapshotGroupping]).validate_json(result.stdout)
    return model


@router.get("/snapshot/volume/{name}")
async def list_snapshots_for_volume(
    name: str, response: Response, client: DockerClient
) -> list[Snapshot] | None:
    cmd = snapshots(tags=[TagFlag.for_volume(name)])
    logging.debug("retrieving snapshots", extra={"cmd": " ".join(cmd)})

    settings = Settings()
    config = await configure_snapshots(client, cmd)
    result = await start_and_wait(
        client, name="backupd-retrieve", config=config, timeout=settings.timeout_seconds
    )

    status = ["failure", "success"][result.success]
    level = [logging.ERROR, logging.INFO][result.success]

    logging.log(level, "finished retrieving snapshots", extra={"status": status})
    logging.debug(result.stdout, extra={"status": status, "stream": "stdout"})
    logging.debug(result.stderr, extra={"status": status, "stream": "stderr"})

    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[Snapshot]).validate_json(result.stdout)
    return model


@router.get("/snapshot/container/{name}")
async def list_snapshots_for_container(
    name: str, response: Response, client: DockerClient
) -> list[SnapshotGroupping] | None:
    volumes = await volumes_from(client, name)
    if volumes is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    cmd = snapshots(tags=map(TagFlag.for_volume, volumes), groupping=GroupFlag.tags())
    logging.debug("retrieving snapshots", extra={"cmd": " ".join(cmd)})

    settings = Settings()
    config = await configure_snapshots(client, cmd)
    result = await start_and_wait(
        client, name="backupd-retrieve", config=config, timeout=settings.timeout_seconds
    )

    status = ["failure", "success"][result.success]
    level = [logging.ERROR, logging.INFO][result.success]

    logging.log(level, "finished retrieving snapshots", extra={"status": status})
    logging.debug(result.stdout, extra={"status": status, "stream": "stdout"})
    logging.debug(result.stderr, extra={"status": status, "stream": "stderr"})

    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[SnapshotGroupping]).validate_json(result.stdout)
    return model
