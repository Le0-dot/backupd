from functools import partial
from timeit import default_timer
from typing import Self

from aiodocker import Docker
from prometheus_client import Counter, Gauge, Histogram

from backupd.docker import (
    Container,
    ContainerCreate,
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


class BackupJob:
    def __init__(
        self, container_name: str, volume: str, repository: Repository
    ) -> None:
        self.config: ContainerCreate = ContainerCreate.shell(
            image=Settings().runner_image,
            cmd=f"{repository.preexec} &&"
            + f"echo {container_name} {volume}",  # NOTE: For testing purposes
            # cmd=f"{repository.preexec} && restic check",
            # cmd=f"{repository.preexec} && backup /data --tag backupd:{container}:{volume}",
            env=repository.env,
            mounts=[
                repository.mount,
                Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True),
            ],
        )

        self.start: Gauge = backup_start.labels(container=container_name, volume=volume)
        self.result: partial[Counter] = partial(
            backup_result.labels, container=container_name, volume=volume
        )
        self.duration: partial[Histogram] = partial(
            backup_duration.labels, container=container_name, volume=volume
        )
        self.job: partial[Gauge] = partial(job_state.labels, kind="backup")

        self.job(state="created").inc()

    async def __call__(self) -> None:
        self.job(state="created").dec()
        self.job(state="started").inc()

        async with Docker() as client:
            start_time = default_timer()
            result = await run_container(client, self.config, "backup")
            finish_time = default_timer()

        run_time = max(finish_time - start_time, 0)

        self.job(state="started").dec()
        self.job(state="finished").inc()

        status = "success" if result.success else "failure"

        self.start.set(start_time)
        self.result(status=status).inc()
        self.duration(status=status).observe(run_time)

        print(result.stdout)
        print(result.stderr)

    @classmethod
    def for_container(cls, container: Container, repository: Repository) -> list[Self]:
        return [cls(container.name, volume, repository) for volume in container.volumes]
