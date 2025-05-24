import logging
from datetime import datetime
from itertools import chain
from typing import Annotated, Literal

from pydantic import BaseModel, Field, TypeAdapter, ValidationError

from backupd.restic.flags import Tag

### snapshots


class GroupKey(BaseModel):
    hostname: str
    paths: list[str] | None
    tags: list[str]


class Snapshot(BaseModel):
    """
    https://restic.readthedocs.io/en/stable/075_scripting.html#snapshots
    """

    time: datetime
    parent: str | None = None
    tree: str
    paths: list[str]
    hostname: str
    username: str
    excludes: list[str] | None = None
    tags: list[str] = []
    program_version: str
    id: str
    short_id: str

    def is_for(self, volume: str) -> bool:
        return Tag.for_volume(volume) in self.tags


class SnapshotGroupping(BaseModel):
    group_key: GroupKey
    snapshots: list[Snapshot]


### errors


class Error(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id5
    """

    class ErrorMessage(BaseModel):
        message: str

    message_type: Literal["error"]
    error: ErrorMessage
    during: str
    item: str


class ExitError(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#exit-errors
    """

    message_type: Literal["exit_error"]
    code: int
    message: str


### backup


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

### restore


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


### common


def parse_messages[T](message_type: type[T], stdout: str, stderr: str) -> list[T]:
    lines = chain(stdout.splitlines(), stderr.splitlines())
    adapter: TypeAdapter[T] = TypeAdapter(message_type)

    messages: list[T] = []
    for line in lines:
        try:
            messages.append(adapter.validate_json(line))
        except ValidationError as e:
            logging.warning(e, extra={"exception": "ValidationError", "input": line})

    return messages
