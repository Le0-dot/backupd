from dataclasses import dataclass, field
from functools import partial
import logging
from time import time
from typing import Self

from aiodocker import Docker
from prometheus_client import Counter, Gauge, Histogram

from backupd.docker import (
    Container,
    ContainerCreate,
    ContainerRunResult,
    Mount,
    run_container,
)
from backupd.metrics import (
    backup_duration,
    backup_result,
    backup_start,
    job_state,
)
from backupd.repository import Repository
from backupd.settings import Settings


@dataclass
class BackupJob:
    container: str
    volume: str
    repository: Repository
    logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("uvicorn.error")
    )

    @property
    def configuration(self) -> ContainerCreate:
        settings = Settings()
        return ContainerCreate.shell(
            image=settings.runner_image,
            cmd=f"{self.repository.preexec} && restic --verbose backup --group-by tags "
            + f"--tag backupd --tag container:{self.container} --tag volume:{self.volume} /data",
            env=self.repository.env | {"RESTIC_HOST": settings.hostname},
            mounts=[
                self.repository.mount,
                Mount(Target="/data", Source=self.volume, Type="volume", ReadOnly=True),
            ],
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
    def duration_metric(self) -> partial[Histogram]:
        return partial(
            backup_duration.labels, container=self.container, volume=self.volume
        )

    @property
    def job_metric(self) -> partial[Gauge]:
        return partial(job_state.labels, kind="backup")

    def log(self, level: int, msg: str, extra: dict[str, str] | None = None) -> None:
        self.logger.log(
            level,
            msg,
            extra={"container": self.container, "volume": self.volume} | (extra or {}),
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
        start_time: float,
        finish_time: float,
    ) -> None:
        status = ["failure", "success"][result.success]
        run_time = max(finish_time - start_time, 0)
        self.start_time_metric.set(start_time)
        self.result_metric(status=status).inc()
        self.duration_metric(status=status).observe(run_time)

        level = [logging.ERROR, logging.INFO][result.success]
        self.log(level, f"Result of backup job is {status} in {run_time:.2f} seconds")
        self.log(level, result.stdout, extra={"stream": "stdout"})
        self.log(level, result.stderr, extra={"stream": "stderr"})

    def __post_init__(self) -> None:
        self.mark_created()

    async def __call__(self) -> None:
        self.mark_started()

        async with Docker() as client:
            start_time = time()
            result = await run_container(client, self.configuration, "backup")
            finish_time = time()

        self.mark_finished()
        self.record_result(result, start_time, finish_time)

    @classmethod
    def for_container(cls, container: Container, repository: Repository) -> list[Self]:
        make = partial(cls, container=container.name, repository=repository)
        return [make(volume=volume) for volume in container.volumes]
