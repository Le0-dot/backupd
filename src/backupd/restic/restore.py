from typing import Literal
from pydantic import BaseModel

from backupd.docker import ContainerCreate, Mount
from backupd.settings import RepositorySettings, Settings


class RestoreSummary(BaseModel):
    message_type: Literal["summary"]
    seconds_elapsed: int
    total_files: int
    files_restored: int
    files_skipped: int
    files_deleted: int
    total_bytes: int
    bytes_restored: int
    bytes_skipped: int


def restore(snapshot: str, volume: str) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    repo_mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        repo_mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)

    data_mount = Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True)
    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --json restore {snapshot} --target /",
        env=repository.env,
        mounts=[repo_mount, data_mount],
    )


def restore_latest(volume: str) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    repo_mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        repo_mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)

    data_mount = Mount(Target="/data", Source=volume, Type="volume", ReadOnly=False)
    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --json restore latest --tag backupd --tag volume:{volume} --target /",
        env=repository.env,
        mounts=[repo_mount, data_mount],
    )
