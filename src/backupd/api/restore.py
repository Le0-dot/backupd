from http import HTTPStatus
from fastapi import APIRouter, Response
from pydantic import TypeAdapter

from backupd.docker import Client, ContainerInspect, VolumeInspect, run_container
from backupd.restic.flags import Tag
from backupd.restic.restore import RestoreMessage, restore, restore_latest
from backupd.restic.snapshots import Snapshot, snapshot_by_id
from backupd.settings import Settings

router = APIRouter(prefix="/restore")

type VolumeRestore = list[RestoreMessage]
type ContainerRestore = dict[str, VolumeRestore]


async def run_restore_latest(client: Client, volume: str) -> tuple[bool, VolumeRestore]:
    configuration = restore_latest(volume)
    result = await run_container(client, configuration, "backupd-restore")

    lines = result.stdout.splitlines() + result.stderr.splitlines()
    adapter: TypeAdapter[RestoreMessage] = TypeAdapter(RestoreMessage)
    messages = list(map(adapter.validate_json, lines))

    return result.success, messages


@router.post("/restore/{snapshot}")
async def restore_snapshot(
    snapshot_id: str, response: Response, client: Client
) -> VolumeRestore | None:
    configuration = snapshot_by_id(snapshot_id)
    result = await run_container(client, configuration, "backupd-retrieve")

    if not result.success:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    try:
        [snapshot] = TypeAdapter(list[Snapshot]).validate_json(result.stdout)
    except ValueError:
        response.status_code = HTTPStatus.BAD_REQUEST
        return None

    tags = map(Tag, snapshot.tags or [])
    volumes = map(lambda t: t.volume, tags)

    try:
        [volume] = list(filter(None, volumes))
    except ValueError:
        response.status_code = HTTPStatus.BAD_REQUEST
        return None

    configuration = restore(snapshot_id, volume)
    result = await run_container(client, configuration, "backupd-restore")

    lines = result.stdout.splitlines() + result.stderr.splitlines()
    adapter: TypeAdapter[RestoreMessage] = TypeAdapter(RestoreMessage)
    messages = list(map(adapter.validate_json, lines))

    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/restore/volume/{name}")
async def restore_volume(
    name: str, response: Response, client: Client
) -> VolumeRestore | None:
    volume = await VolumeInspect.by_name(client, name)

    if volume is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    success, messages = await run_restore_latest(client, name)

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/restore/container/{name}")
async def restore_container(
    name: str, response: Response, client: Client
) -> ContainerRestore | None:
    settings = Settings()

    container = await ContainerInspect.by_name(client, name)

    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    messages: dict[str, VolumeRestore] = {}
    for volume in container.volumes:
        success, restore_messages = await run_restore_latest(client, volume.Name)

        messages[volume.Name] = restore_messages

        if not success:
            response.status_code = HTTPStatus.FAILED_DEPENDENCY

        if not success and settings.abort_on_failure:
            break

    return messages
