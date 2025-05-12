import logging
from dataclasses import dataclass, field
from functools import partial
from typing import Self

from aiodocker import Docker
from prometheus_client import Gauge

from backupd.docker import (
    ContainerInspect,
    ContainerRunResult,
    run_container,
)
from backupd.metrics import job_state, job_result
from backupd.restic.backup import BackupSummary, backup


@dataclass
class BackupJob:
    container: str
    volume: str
    logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("uvicorn.error")
    )

    @property
    def result_metric(self) -> partial[Gauge]:
        return partial(job_result.labels, kind="backup", volume=self.volume)

    @property
    def state_metric(self) -> partial[Gauge]:
        return partial(job_state.labels, kind="backup")

    def log(self, level: int, msg: str, extra: dict[str, str] | None = None) -> None:
        self.logger.log(
            level,
            msg,
            extra={
                "source": "job",
                "type": "backup",
                "container": self.container,
                "volume": self.volume,
            }
            | (extra or {}),
        )

    def mark_created(self) -> None:
        self.state_metric(state="created").inc()

        self.log(logging.INFO, "Created backup job")

    def mark_started(self) -> None:
        self.state_metric(state="created").dec()
        self.state_metric(state="started").inc()

        self.log(logging.INFO, "Started backup job")

    def mark_finished(self) -> None:
        self.state_metric(state="started").dec()
        self.state_metric(state="finished").inc()

        self.log(logging.INFO, "Finished backup job")

    def record_result(
        self,
        result: ContainerRunResult,
    ) -> None:
        level = [logging.ERROR, logging.INFO][result.success]
        status = ["failure", "success"][result.success]

        self.result_metric(status=status).inc()
        self.log(level, result.stdout, extra={"stream": "stdout"})
        self.log(level, result.stderr, extra={"stream": "stderr"})

        if result.success:
            summary = BackupSummary.model_validate_json(result.stdout.splitlines()[-1])
            self.log(
                logging.INFO,
                "job finished",
                extra={
                    "status": "success",
                    "start": str(summary.backup_start),
                    "duration": str(summary.total_duration),
                    "snapshot": str(summary.snapshot_id),
                },
            )

    def __post_init__(self) -> None:
        self.mark_created()

    async def __call__(self) -> None:
        self.mark_started()

        configuration = backup(self.volume)

        async with Docker() as client:
            result = await run_container(client, configuration, "backupd-backup")

        self.mark_finished()
        self.record_result(result)

    @classmethod
    def for_container(cls, container: ContainerInspect) -> list[Self]:
        make = partial(cls, container=container.Name)
        return [make(volume=volume.Name) for volume in container.volumes]
