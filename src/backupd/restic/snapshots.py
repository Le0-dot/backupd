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
    paths: list[Path]
    hostname: str
    username: str
    tags: list[str]
    program_version: str
    id: str
    short_id: str


Snapshots = TypeAdapter(list[Snapshot])


def snapshots(id: str, tags: str) -> ContainerCreate:
    settings = Settings()
    repository = RepositorySettings()

    tag_str = f"--tag {tags}" if tags else ""
    mount: Mount | None = None
    if repository.restic.backend == "local":
        [path] = repository.restic.location
        mount = Mount(Target=path, Source=path, Type="bind", ReadOnly=True)

    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"restic --json snapshots {tag_str} {id}",
        env=repository.env,
        mounts=[mount],
    )
