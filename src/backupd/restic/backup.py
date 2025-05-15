from datetime import datetime
from typing import Annotated, Literal

from pydantic import BaseModel, Field

from backupd.docker import ContainerCreate, Mount
from backupd.restic.error import Error, ExitError
from backupd.restic.flags import TagFlag, Group
from backupd.settings import RepositorySettings, Settings


class BackupStatus(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#status
    """

    message_type: Literal["status"]
    seconds_elapsed: int | None = None
    seconds_remaining: int | None = None
    percent_done: float
    total_files: int | None = None
    files_done: int | None = None
    total_bytes: int | None = None
    bytes_done: int | None = None
    error_count: int | None = None
    current_files: list[str] | None = None


class BackupVerboseStatus(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#verbose-status
    """

    message_type: Literal["verbose_status"]
    action: Literal["new", "unchanged", "modified", "scan_finished"]
    item: str
    duration: float
    data_size: int
    data_size_in_repo: int
    metadata_size: int
    metadata_size_in_repo: int
    total_files: int


class BackupSummary(BaseModel):
    """
    https://restic.readthedocs.io/en/stable/075_scripting.html#summary
    """

    message_type: Literal["summary"]
    dry_run: bool = False
    files_new: int
    files_changed: int
    files_unmodified: int
    dirs_new: int
    dirs_changed: int
    dirs_unmodified: int
    data_blobs: int
    tree_blobs: int
    data_added: int
    data_added_packed: int
    total_files_processed: int
    total_bytes_processed: int
    backup_start: datetime
    backup_end: datetime
    total_duration: float
    snapshot_id: str | None = None


BackupMessage = Annotated[
    BackupStatus | BackupVerboseStatus | BackupSummary | Error | ExitError,
    Field(discriminator="message_type"),
]


def backup(volume: str) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    repo_mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        repo_mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)

    data_mount = Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True)

    tag_flag = TagFlag.for_app() | TagFlag.for_volume(volume)
    group_flag = Group.tags()

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --verbose=2 --json backup {tag_flag} {group_flag} /data",
        env=repository.env,
        mounts=[repo_mount, data_mount],
    )
