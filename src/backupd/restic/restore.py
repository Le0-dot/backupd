from typing import Annotated, Literal
from pydantic import BaseModel, Field

from backupd.docker import ContainerCreate, Mount
from backupd.restic.error import Error, ExitError
from backupd.restic.flags import TagFlag
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
    action: Literal["restored", "updated", "unchanged", "deleted"]
    item: str
    size: int


class RestoreSummary(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id7
    """

    message_type: Literal["summary"]
    seconds_elapsed: int | None = None
    total_files: int | None = None
    files_restored: int | None = None
    files_skipped: int | None = None
    files_deleted: int | None = None
    total_bytes: int | None = None
    bytes_restored: int | None = None
    bytes_skipped: int | None = None


RestoreMessage = Annotated[
    RestoreStatus | RestoreVerboseStatus | RestoreSummary | Error | ExitError,
    Field(discriminator="message_type"),
]


def restore(snapshot: str, volume: str) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    repo_mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        repo_mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)

    data_mount = Mount(Target="/data", Source=volume, Type="volume", ReadOnly=False)

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --verbose=2 --json restore {snapshot}:data --target /data",
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

    tag = TagFlag.for_volume(volume)

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --verbose=2 --json restore latest:data {tag} --target /data",
        env=repository.env,
        mounts=[repo_mount, data_mount],
    )
