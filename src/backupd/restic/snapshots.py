from datetime import datetime
from pathlib import Path

from pydantic import BaseModel
from starlette.requests import empty_receive

from backupd.docker import ContainerCreate, Mount
from backupd.restic.flags import Group, Tag
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


def snapshots(tags: list[Tag], groupping: Group) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)

    tag_flags = " ".join(tag.flag for tag in tags)

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --json snapshots {tag_flags} {groupping.flag}",
        env=repository.env,
        mounts=[mount],
    )
