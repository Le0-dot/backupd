import logging
from collections.abc import Iterable
from http import HTTPStatus
from typing import cast

from fastapi import APIRouter, Response

from backupd.docker import DockerClient, run_container
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
    result = await run_container(client.docker, configuration, "backupd-backup")

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

    volumes = await client.volumes()
    success, messages = await run_bulk_backup(
        client, volumes, settings.abort_on_failure
    )

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/backup/volume/{name}")
async def backup_volume(
    name: str, response: Response, client: DockerClient
) -> VolumeBackup | None:
    if not await client.volume_exists(name):
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

    containers = await client.containers()

    messages: ConatinersBackup = {}
    for container in containers:
        volumes = await client.volumes_from(container)
        assert volumes is not None

        success, backup_messages = await run_bulk_backup(
            client, volumes, settings.abort_on_failure
        )

        messages[container] = backup_messages

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

    volumes = await client.volumes_from(name)
    if volumes is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    success, messages = await run_bulk_backup(
        client, volumes, settings.abort_on_failure
    )

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages
