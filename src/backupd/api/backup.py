import logging
from collections.abc import Iterable
from http import HTTPStatus
from pathlib import Path
from typing import cast

from fastapi import APIRouter, Response

from backupd.docker.interface import (
    DockerClient,
    configure_container,
    containers,
    start_and_wait,
    volume_exists,
    volumes,
    volumes_from,
)
from backupd.metrics import backup_duration, backup_result
from backupd.restic.commands import backup
from backupd.restic.models import BackupMessage, BackupSummary, parse_messages
from backupd.settings import RepositorySettings, Settings

router = APIRouter(prefix="/backup")

type VolumeBackup = list[BackupMessage]
type BulkBackup = dict[str, VolumeBackup]
type ConatinersBackup = dict[str, BulkBackup]


async def run_backup(client: DockerClient, volume: str) -> tuple[bool, VolumeBackup]:
    settings = Settings()
    repository = RepositorySettings()
    mountpoint = Path("/data")

    cmd = backup(volume=volume, mountpoint=mountpoint)
    logging.debug(
        "starting volume backup", extra={"volume": volume, "cmd": " ".join(cmd)}
    )

    config = configure_container(
        image=settings.runner_image,
        entrypoint=settings.runner_entrypoint,
        cmd=cmd,
        env=repository.env,
        volume_mounts={volume: mountpoint},
        bind_mounts=None
        if repository.restic.backend != "local"
        else {Path(repository.restic.location): Path(repository.restic.location)},
    )
    success, logs = await start_and_wait(
        client, name="backupd-backup", config=config, timeout=settings.timeout_seconds
    )

    messages = parse_messages(cast(type[BackupMessage], BackupMessage), logs)

    status = ["failure", "success"][success]
    backup_result.labels(volume, status).inc()

    extra: dict[str, float] = {}
    if success:
        summary = next(filter(lambda m: isinstance(m, BackupSummary), messages))
        summary = cast(BackupSummary, summary)
        backup_duration.labels(volume).observe(summary.total_duration)
        extra["duration"] = summary.total_duration

    level = [logging.ERROR, logging.INFO][success]
    logging.log(
        level,
        "finished volume backup",
        extra={"volume": volume, "status": status} | extra,
    )

    logging.debug(logs, extra={"volume": volume, "status": status})

    return success, messages


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

    volume_list = await volumes(client)
    success, messages = await run_bulk_backup(
        client, volume_list, settings.abort_on_failure
    )

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages


@router.post("/backup/volume/{name}")
async def backup_volume(
    name: str, response: Response, client: DockerClient
) -> VolumeBackup | None:
    if not await volume_exists(client, name):
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

    container_list = await containers(client)

    messages: ConatinersBackup = {}
    for container in container_list:
        volumes = await volumes_from(client, container)
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

    volumes = await volumes_from(client, name)
    if volumes is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    success, messages = await run_bulk_backup(
        client, volumes, settings.abort_on_failure
    )

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY

    return messages
