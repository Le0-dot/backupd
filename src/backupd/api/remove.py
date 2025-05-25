from http import HTTPStatus
import logging
from pathlib import Path

from fastapi import APIRouter, Response

from backupd.docker.interface import DockerClient, configure_container, start_and_wait
from backupd.restic.commands import forget
from backupd.restic.models import ForgetGroup, parse_messages
from backupd.settings import RepositorySettings, Settings

router = APIRouter(prefix="/remove")


@router.delete("/snapshot")
async def remove_old_snapshots(
    response: Response, client: DockerClient
) -> list[ForgetGroup] | None:
    settings = Settings()
    repository = RepositorySettings()

    cmd = forget(forget_policy=repository.restic.keep)
    logging.debug("removing old snapshots", extra={"cmd": " ".join(cmd)})

    config = configure_container(
        image=settings.runner_image,
        entrypoint=settings.runner_entrypoint,
        cmd=cmd,
        env=repository.env,
        bind_mounts=None
        if repository.restic.backend != "local"
        else {Path(repository.restic.location): Path(repository.restic.location)},
    )

    success, logs = await start_and_wait(
        client,
        name="backupd-remove",
        config=config,
        timeout=settings.backup_timeout_seconds,
    )

    status = ["failure", "success"][success]
    level = [logging.ERROR, logging.INFO][success]

    logging.log(level, "finished removing old snapshots", extra={"status": status})
    logging.debug(logs, extra={"status": status})

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    [model] = parse_messages(list[ForgetGroup], logs)
    return model


@router.delete("/snapshot/{snapshot_id}")
async def remove_snapshot(
    snapshot_id: str, response: Response, client: DockerClient
) -> None:
    settings = Settings()
    repository = RepositorySettings()

    cmd = forget(snapshot_id=snapshot_id)
    logging.debug(f"removing snapshot {snapshot_id}", extra={"cmd": " ".join(cmd)})

    config = configure_container(
        image=settings.runner_image,
        entrypoint=settings.runner_entrypoint,
        cmd=cmd,
        env=repository.env,
        bind_mounts=None
        if repository.restic.backend != "local"
        else {Path(repository.restic.location): Path(repository.restic.location)},
    )

    success, logs = await start_and_wait(
        client,
        name="backupd-remove",
        config=config,
        timeout=settings.backup_timeout_seconds,
    )

    status = ["failure", "success"][success]
    level = [logging.ERROR, logging.INFO][success]

    logging.log(
        level, f"finished removing snapshot {snapshot_id}", extra={"status": status}
    )
    logging.debug(logs, extra={"status": status})

    if not success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
