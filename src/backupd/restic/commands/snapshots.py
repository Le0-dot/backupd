from datetime import datetime
from pathlib import Path

from pydantic import BaseModel, TypeAdapter

from backupd.docker import ContainerCreate
from backupd.restic.repository import Repository
from backupd.settings import Settings


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


def snapshots(repository: Repository, id: str, tags: str) -> ContainerCreate:
    tag_str = f"--tag {tags}" if tags else ""

    settings = Settings()
    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"{repository.preexec} && restic --json snapshots {tag_str} {id}",
        env=repository.env,
        mounts=[repository.mount],
    )
