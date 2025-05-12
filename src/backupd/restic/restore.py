from typing import Annotated, Literal
from pydantic import BaseModel, Field

from backupd.docker import ContainerCreate, Mount
from backupd.settings import RepositorySettings, Settings


class RestoreStatus(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id4
    """

    message_type: Literal["status"]
    seconds_elapsed: int
    percent_done: float
    total_files: int
    files_restored: int
    files_skipped: int
    files_deleted: int
    total_bytes: int
    bytes_restored: int
    bytes_skipped: int


class RestoreVerboseStatus(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id6
    """

    message_type: Literal["verbose_status"]
    action: Literal["restored", "updated", "updated", "updated"]
    item: str
    size: int


class RestoreError(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id5
    """

    message_type: Literal["error"]
    # error.message: str
    during: str
    item: int


class RestoreSummary(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id7
    """

    message_type: Literal["summary"]
    seconds_elapsed: int
    total_files: int
    files_restored: int
    files_skipped: int
    files_deleted: int
    total_bytes: int
    bytes_restored: int
    bytes_skipped: int


RestoreMessage = Annotated[
    RestoreStatus | RestoreVerboseStatus | RestoreError | RestoreSummary,
    Field(discriminator="message_type"),
]


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
