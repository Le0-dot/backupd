import logging
from http import HTTPStatus
from typing import Literal, cast

from fastapi import APIRouter, Response
from pydantic import TypeAdapter

from backupd.docker import DockerClient, run_container
from backupd.metrics import restore_result
from backupd.restic.common import parse_messages
from backupd.restic.restore import RestoreMessage, restore
from backupd.restic.snapshots import Snapshot, snapshot_by_id
from backupd.settings import Settings

router = APIRouter(prefix="/restore")

type VolumeRestore = list[RestoreMessage]
type ContainerRestore = dict[str, VolumeRestore]


async def run_restore(
    client: DockerClient, volume: str, snapshot_id: str | Literal["latest"]
) -> tuple[bool, VolumeRestore]:
    logging.debug("staring volume restoreation", extra={"volume": volume})

    configuration = restore(volume, snapshot_id)
    result = await run_container(client.docker, configuration, "backupd-restore")

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
    # Check if volume exists
    if not await client.volume_exists(name):
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    # Run restoration process
    success, messages = await run_restore(client, name, "latest")

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/volume/{name}/{snapshot_id}")
async def restore_volume(
    name: str, snapshot_id: str, response: Response, client: DockerClient
) -> VolumeRestore | None:
    # Check if volume exists
    if not await client.volume_exists(name):
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    # Check if snapshot exists and is for the volume
    configuration = snapshot_by_id(snapshot_id)
    result = await run_container(client.docker, configuration, "backupd-retrieve")

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

    volumes = await client.volumes_from(name)

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
