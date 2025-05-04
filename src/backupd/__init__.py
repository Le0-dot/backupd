from contextlib import asynccontextmanager
from functools import partial
from http import HTTPStatus
from itertools import chain
from typing import Self

from fastapi import FastAPI, Response
from prometheus_client import Counter, Gauge, Summary

from backupd.docker import (
    Client,
    Container,
    ContainerCreate,
    Mount,
    container_by_name,
    list_containers,
    run_container,
)
from backupd.metrics import (
    APIMetricsMiddleware,
    backup_failure,
    backup_success,
    backup_time,
    jobs,
    metrics_lifespan,
)
from backupd.repository import Repository
from backupd.settings import Settings
from backupd.task_queue import AppQueue, queue_lifespan


@asynccontextmanager
async def app_lifespan(app: FastAPI):
    async with metrics_lifespan(app), queue_lifespan(app):
        yield


app = FastAPI(lifespan=app_lifespan)
app.add_middleware(APIMetricsMiddleware)


@app.get("/container/{name}")
async def get_container(
    name: str, response: Response, client: Client
) -> Container | None:
    container = await container_by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
    return container


@app.get("/containers")
async def get_containers(client: Client) -> list[Container]:
    return await list_containers(client)


class BackupJob:
    def __init__(
        self, container_name: str, volume: str, repository: Repository
    ) -> None:
        self.config: ContainerCreate = ContainerCreate.shell(
            image=Settings().runner_image,
            cmd=f"{repository.preexec} &&"
            + "echo {container} {volume}",  # NOTE: For testing purposes
            # cmd=f"{repository.preexec} && restic check",
            # cmd=f"{repository.preexec} && backup /data --tag backupd:{container}:{volume}",
            env=repository.env,
            mounts=[
                repository.mount,
                Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True),
            ],
        )

        self.job: Gauge = jobs.labels(
            job_type="backup", container=container_name, volume=volume
        )
        self.success: Counter = backup_success.labels(
            container=container_name, volume=volume
        )
        self.failure: Counter = backup_failure.labels(
            container=container_name, volume=volume
        )
        self.time: Summary = backup_time.labels(container=container_name, volume=volume)

        self.job.inc()

    async def __call__(self) -> None:
        with self.time.time():
            result = await run_container(self.config, "backup")

        self.job.dec()

        counter = self.success if result.success else self.failure
        counter.inc()

        print(result.stdout)
        print(result.stderr)

    @classmethod
    def for_container(cls, container: Container, repository: Repository) -> list[Self]:
        return [cls(container.name, volume, repository) for volume in container.volumes]


@app.post("/backup/{name}")
async def post_backup_container(
    name: str,
    repository: Repository,
    response: Response,
    client: Client,
    queue: AppQueue,
) -> Container | None:
    container = await container_by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return

    jobs = BackupJob.for_container(container, repository)
    await queue.put(*jobs)

    return container


@app.post("/backup")
async def post_backup(
    repository: Repository,
    client: Client,
    queue: AppQueue,
) -> list[Container]:
    containers = await list_containers(client)

    jobs = map(partial(BackupJob.for_container, repository=repository), containers)
    await queue.put(*chain.from_iterable(jobs))

    return containers
