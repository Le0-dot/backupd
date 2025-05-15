from collections.abc import Iterable
from http import HTTPStatus
from typing import cast
from fastapi import APIRouter, Response
from pydantic import TypeAdapter

from backupd.docker import Client, ContainerInspect, VolumeInspect, run_container
from backupd.metrics import backup_result, backup_duration
from backupd.restic.backup import BackupMessage, BackupSummary, backup
from backupd.settings import Settings

router = APIRouter(prefix="/backup")

type VolumeBackup = list[BackupMessage]
type BulkBackup = dict[str, VolumeBackup]
type ConatinersBackup = dict[str, BulkBackup]


async def run_backup(client: Client, volume: str) -> tuple[bool, VolumeBackup]:
    configuration = backup(volume)
    result = await run_container(client, configuration, "backupd-backup")

    lines = result.stdout.splitlines() + result.stderr.splitlines()
    adapter: TypeAdapter[BackupMessage] = TypeAdapter(BackupMessage)
    messages = list(map(adapter.validate_json, lines))

    status = ["failure", "success"][result.success]
    backup_result.labels(volume, status).inc()

    if result.success:
        summary = next(filter(lambda m: isinstance(m, BackupSummary), messages))
        summary = cast(BackupSummary, summary)
        backup_duration.labels(volume).observe(summary.total_duration)

    return result.success, messages


async def run_bulk_backup(
    client: Client, volumes: Iterable[str], abort_on_failure: bool
) -> tuple[bool, BulkBackup]:
    success = True
    messages: BulkBackup = {}
    for volume in volumes:
        backup_success, backup_messages = await run_backup(client, volume)

        success &= backup_success
        messages[volume] = backup_messages

        if not success and abort_on_failure:
            break

    return success, messages


@router.post("/backup/volume")
async def backup_all_volumes(response: Response, client: Client) -> BulkBackup:
    settings = Settings()

    volumes = await VolumeInspect.all(client)
    named = filter(lambda v: not v.anonymous, volumes)
    names = map(lambda v: v.Name, named)
    success, messages = await run_bulk_backup(client, names, settings.abort_on_failure)

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/backup/volume/{name}")
async def backup_volume(
    name: str, response: Response, client: Client
) -> VolumeBackup | None:
    volume = await VolumeInspect.by_name(client, name)

    if volume is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    success, messages = await run_backup(client, name)

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/backup/container")
async def backup_all_conatiner(response: Response, client: Client) -> ConatinersBackup:
    settings = Settings()

    containers = await ContainerInspect.all(client)

    messages: ConatinersBackup = {}
    for container in containers:
        names = map(lambda v: v.Name, container.volumes)
        success, backup_messages = await run_bulk_backup(
            client, names, settings.abort_on_failure
        )

        messages[container.name] = backup_messages

        if not success:
            response.status_code = HTTPStatus.FAILED_DEPENDENCY

        if not success and settings.abort_on_failure:
            break

    return messages


@router.post("/backup/container/{name}")
async def backup_container(
    name: str, response: Response, client: Client
) -> BulkBackup | None:
    settings = Settings()

    container = await ContainerInspect.by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    names = map(lambda v: v.Name, container.volumes)
    success, messages = await run_bulk_backup(client, names, settings.abort_on_failure)

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages
