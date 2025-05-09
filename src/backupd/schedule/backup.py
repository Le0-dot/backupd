import logging
from dataclasses import dataclass, field
from functools import partial
from typing import Self

from aiodocker import Docker
from prometheus_client import Counter, Gauge, Histogram

from backupd.docker import (
    ContainerInspect,
    ContainerRunResult,
    run_container,
)
from backupd.metrics import (
    backup_duration,
    backup_result,
    backup_start,
    job_state,
)
from backupd.restic.backup import BackupSummary, backup


@dataclass
class BackupJob:
    container: str
    volume: str
    logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("uvicorn.error")
    )

    @property
    def start_time_metric(self) -> Gauge:
        return backup_start.labels(container=self.container, volume=self.volume)

    @property
    def result_metric(self) -> partial[Counter]:
        return partial(
            backup_result.labels, container=self.container, volume=self.volume
        )

    @property
    def duration_metric(self) -> Histogram:
        return backup_duration.labels(container=self.container, volume=self.volume)

    @property
    def job_metric(self) -> partial[Gauge]:
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
        self.job_metric(state="created").inc()

        self.log(logging.INFO, "Created backup job")

    def mark_started(self) -> None:
        self.job_metric(state="created").dec()
        self.job_metric(state="started").inc()

        self.log(logging.INFO, "Started backup job")

    def mark_finished(self) -> None:
        self.job_metric(state="started").dec()
        self.job_metric(state="finished").inc()

        self.log(logging.INFO, "Finished backup job")

    def record_result(
        self,
        result: ContainerRunResult,
    ) -> None:
        if result.success:
            summary = BackupSummary.model_validate_json(result.stdout.splitlines()[-1])
            self.result_metric(status="success").inc()
            self.start_time_metric.set(summary.backup_start.timestamp())
            self.duration_metric.observe(summary.total_duration)
        else:
            self.result_metric(status="failure").inc()

        level = [logging.ERROR, logging.INFO][result.success]
        status = ["failure", "success"][result.success]
        self.log(level, f"Result of backup job is {status}")
        self.log(level, result.stdout, extra={"stream": "stdout"})
        self.log(level, result.stderr, extra={"stream": "stderr"})

    def __post_init__(self) -> None:
        self.mark_created()

    async def __call__(self) -> None:
        self.mark_started()

        configuration = backup(self.volume)

        async with Docker() as client:
            result = await run_container(client, configuration, "backup")

        self.mark_finished()
        self.record_result(result)

    @classmethod
    def for_container(cls, container: ContainerInspect) -> list[Self]:
        make = partial(cls, container=container.Name)
        return [make(volume=volume.Name) for volume in container.volumes]
