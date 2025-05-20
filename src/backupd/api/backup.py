import logging
from collections.abc import Iterable
from http import HTTPStatus
from typing import cast

from fastapi import APIRouter, Response

from backupd.docker import DockerClient, ContainerInspect, VolumeInspect, run_container
from backupd.metrics import backup_duration, backup_result
from backupd.restic.backup import BackupMessage, BackupSummary, backup
from backupd.restic.common import parse_messages
from backupd.settings import Settings

router = APIRouter(prefix="/backup")

type VolumeBackup = list[BackupMessage]
type BulkBackup = dict[str, VolumeBackup]
type ConatinersBackup = dict[str, BulkBackup]


async def run_backup(client: DockerClient, volume: str) -> tuple[bool, VolumeBackup]:
    logging.debug("starting volume backup", extra={"volume": volume})

    configuration = backup(volume)
    result = await run_container(client, configuration, "backupd-backup")

    messages = parse_messages(
        cast(type[BackupMessage], BackupMessage), result.stdout, result.stderr
    )

    status = ["failure", "success"][result.success]
    backup_result.labels(volume, status).inc()

    extra: dict[str, float] = {}
    if result.success:
        summary = next(filter(lambda m: isinstance(m, BackupSummary), messages))
        summary = cast(BackupSummary, summary)
        backup_duration.labels(volume).observe(summary.total_duration)
        extra["duration"] = summary.total_duration

    level = [logging.ERROR, logging.INFO][result.success]
    logging.log(
        level,
        "finished volume backup",
        extra={"volume": volume, "status": status} | extra,
    )

    logging.debug(
        result.stdout, extra={"volume": volume, "status": status, "stream": "stdout"}
    )
    logging.debug(
        result.stderr, extra={"volume": volume, "status": status, "stream": "stderr"}
    )

    return result.success, messages


async def run_bulk_backup(
    client: DockerClient, volumes: Iterable[str], abort_on_failure: bool
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
async def backup_all_volumes(response: Response, client: DockerClient) -> BulkBackup:
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
    name: str, response: Response, client: DockerClient
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
async def backup_all_conatiner(
    response: Response, client: DockerClient
) -> ConatinersBackup:
    settings = Settings()

    containers = await ContainerInspect.all(client)

    messages: ConatinersBackup = {}
    for container in containers:
        volumes = await container.volumes(client)
        names = [volume.Name for volume in volumes]
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
    name: str, response: Response, client: DockerClient
) -> BulkBackup | None:
    settings = Settings()

    container = await ContainerInspect.by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    volumes = await container.volumes(client)
    names = [volume.Name for volume in volumes]
    success, messages = await run_bulk_backup(client, names, settings.abort_on_failure)

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages
