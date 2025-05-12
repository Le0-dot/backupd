from datetime import datetime
from typing import Annotated, Literal

from pydantic import BaseModel, Field, TypeAdapter

from backupd.docker import ContainerCreate, Mount
from backupd.settings import RepositorySettings, Settings


class BackupStatus(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#status
    """

    message_type: Literal["status"]
    seconds_elapsed: int
    seconds_remaining: int
    percent_done: float
    total_files: int
    files_done: int
    total_bytes: int
    bytes_done: int
    error_count: int
    current_files: list[str]


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


class BackupError(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#error
    """

    message_type: Literal["error"]
    # error.message: str
    during: str
    item: str


class BackupSummary(BaseModel):
    """
    https://restic.readthedocs.io/en/stable/075_scripting.html#summary
    """

    message_type: Literal["summary"]
    dry_run: bool
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
    BackupStatus | BackupVerboseStatus | BackupError | BackupSummary,
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

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd="restic --verbose --json backup --group-by tags "
        + f"--tag backupd --tag volume:{volume} /data",
        env=repository.env,
        mounts=[repo_mount, data_mount],
    )
