from datetime import datetime
from pathlib import Path

from pydantic import BaseModel, TypeAdapter

from backupd.docker import ContainerCreate, Mount
from backupd.settings import RepositorySettings, Settings


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


Snapshots = TypeAdapter(list[Snapshot])


def snapshots(tags: str) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    tag_filter = f"--tag {tags}" if tags else ""

    mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=False)

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --json snapshots {tag_filter}",
        env=repository.env,
        mounts=[mount],
    )
