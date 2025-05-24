import logging
from http import HTTPStatus
from pathlib import Path
from typing import Literal, cast

from fastapi import APIRouter, Response
from pydantic import TypeAdapter

from backupd.docker.interface import (
    DockerClient,
    configure_container,
    start_and_wait,
    volume_exists,
    volumes_from,
)
from backupd.metrics import restore_result
from backupd.restic.commands import restore, snapshots
from backupd.restic.models import RestoreMessage, Snapshot, parse_messages
from backupd.settings import RepositorySettings, Settings

router = APIRouter(prefix="/restore")

type VolumeRestore = list[RestoreMessage]
type ContainerRestore = dict[str, VolumeRestore]


async def run_restore(
    client: DockerClient, volume: str, snapshot_id: str | Literal["latest"]
) -> tuple[bool, VolumeRestore]:
    logging.debug("staring volume restoreation", extra={"volume": volume})

    settings = Settings()
    repository = RepositorySettings()
    mountpoint = Path("/data")
    cmd = restore(volume=volume, mountpoint=mountpoint, snapshot_id=snapshot_id)
    config = await configure_container(
        client,
        image=settings.runner_image,
        entrypoint=settings.runner_entrypoint,
        cmd=cmd,
        env=repository.env,
        volumes={volume: mountpoint},
        binds=None
        if repository.restic.backend != "local"
        else {Path(repository.restic.location): Path(repository.restic.location)},
    )
    result = await start_and_wait(
        client, name="backupd-restore", config=config, timeout=settings.timeout_seconds
    )

    messages = parse_messages(
        cast(type[RestoreMessage], RestoreMessage), result.stdout, result.stderr
    )

    status = ["failure", "success"][result.success]
    restore_result.labels(volume, status).inc()

    level = [logging.ERROR, logging.INFO][result.success]
    logging.log(
        level, "finished volume restoration", extra={"volume": volume, "status": status}
    )

    logging.debug(
        result.stdout, extra={"volume": volume, "status": status, "stream": "stdout"}
    )
    logging.debug(
        result.stderr, extra={"volume": volume, "status": status, "stream": "stderr"}
    )

    return result.success, messages


@router.post("/volume/{name}")
async def restore_latest_volume(
    name: str, response: Response, client: DockerClient
) -> VolumeRestore | None:
    if not await volume_exists(client, name):
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    success, messages = await run_restore(client, name, "latest")

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/volume/{name}/{snapshot_id}")
async def restore_volume(
    name: str, snapshot_id: str, response: Response, client: DockerClient
) -> VolumeRestore | None:
    if not await volume_exists(client, name):
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    settings = Settings()
    repository = RepositorySettings()
    cmd = snapshots(snapshot_id=snapshot_id)
    config = await configure_container(
        client,
        image=settings.runner_image,
        entrypoint=settings.runner_entrypoint,
        cmd=cmd,
        env=repository.env,
        binds=None
        if repository.restic.backend != "local"
        else {Path(repository.restic.location): Path(repository.restic.location)},
    )
    result = await start_and_wait(
        client, name="backupd-retrieve", config=config, timeout=settings.timeout_seconds
    )

    if not result.success:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    [snapshot] = TypeAdapter(list[Snapshot]).validate_json(result.stdout)
    if not snapshot.is_for(name):
        response.status_code = HTTPStatus.BAD_REQUEST
        return None

    # Run restoration process
    success, messages = await run_restore(client, name, snapshot_id)

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/container/{name}")
async def restore_container(
    name: str, response: Response, client: DockerClient
) -> ContainerRestore | None:
    settings = Settings()

    volumes = await volumes_from(client, name)

    if volumes is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    messages: dict[str, VolumeRestore] = {}

    for volume in volumes:
        success, restore_messages = await run_restore(client, volume, "latest")

        messages[volume] = restore_messages

        if not success:
            response.status_code = HTTPStatus.FAILED_DEPENDENCY

        if not success and settings.abort_on_failure:
            break

    return messages
