from datetime import datetime
from typing import Literal

from pydantic import BaseModel

from backupd.docker import ContainerCreate, Mount
from backupd.restic.repository import Repository
from backupd.settings import Settings


class BackupSummary(BaseModel):
    """
    https://restic.readthedocs.io/en/stable/075_scripting.html#summary
    """

    message_type: Literal["summary"]
    backup_start: datetime
    backup_end: datetime
    total_duration: float
    snapshot_id: str | None = None


def backup(repository: Repository, volume: str) -> ContainerCreate:
    settings = Settings()
    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd=f"{repository.preexec} && restic --verbose --json backup --group-by tags "
        + f"--tag backupd --tag volume:{volume} /data",
        env=repository.env | {"RESTIC_HOST": settings.hostname},
        mounts=[
            repository.mount,
            Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True),
        ],
    )
