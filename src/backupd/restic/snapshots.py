from datetime import datetime
from pathlib import Path

from pydantic import BaseModel

from backupd.docker import ContainerCreate, Mount
from backupd.restic.flags import Group, TagFlag
from backupd.settings import RepositorySettings, Settings


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
    paths: list[Path]
    hostname: str
    username: str
    excludes: list[str] | None = None
    tags: list[str] | None = None
    program_version: str
    id: str
    short_id: str


class SnapshotGroupping(BaseModel):
    group_key: GroupKey
    snapshots: list[Snapshot]


def snapshots(tags: list[TagFlag], groupping: Group) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)


    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --json snapshots {snapshot_id}",
        env=repository.env,
        mounts=[mount],
    )
