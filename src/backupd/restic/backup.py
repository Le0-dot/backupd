from datetime import datetime
from typing import Literal

from pydantic import BaseModel

from backupd.docker import ContainerCreate, Mount
from backupd.settings import ResticSettings, Settings


class BackupSummary(BaseModel):
    """
    https://restic.readthedocs.io/en/stable/075_scripting.html#summary
    """

    message_type: Literal["summary"]
    backup_start: datetime
    backup_end: datetime
    total_duration: float
    snapshot_id: str | None = None


def backup(volume: str) -> ContainerCreate:
    settings = Settings()
    repository = ResticSettings()
    return ContainerCreate.shell(
        image=settings.runner_image,
        cmd="restic --verbose --json backup --group-by tags "
        + f"--tag backupd --tag volume:{volume} /data",
        env=repository.env + [f"RESTIC_HOST={settings.hostname}"],
        mounts=[
            repository.mount,
            Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True),
        ],
    )
