from contextlib import asynccontextmanager
from http import HTTPStatus
from itertools import chain

from fastapi import FastAPI, Response

from backupd.docker import (
    Client,
    Container,
    ContainerCreate,
    Mount,
    container_by_name,
    list_containers,
    run_container,
)
from backupd.metrics import AppMetrics, APIMetricsMiddleware, instrument_backup, metrics_lifespan
from backupd.repository import Repository
from backupd.task_queue import AppQueue, queue_lifespan


@asynccontextmanager
async def app_lifespan(app: FastAPI):
    async with metrics_lifespan(app), queue_lifespan(app):
        yield


app = FastAPI(lifespan=app_lifespan)
app.add_middleware(APIMetricsMiddleware)

docker_image = "docker.io/instrumentisto/restic:0.18"


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


def configure_backup(
    container: str, volume: str, repository: Repository
) -> ContainerCreate:
    backup_dir = "/data"
    return ContainerCreate.shell(
        image=docker_image,
        cmd=f"{repository.preexec} &&" + "echo 123",  # NOTE: For testing purposes
        # cmd=f"{repository.preexec} && restic check",
        # cmd=f"{repository.preexec} && backup {backup_dir} --tag backupd:{container}:{volume}",
        env=repository.env,
        mounts=[
            repository.mount,
            Mount(Target=backup_dir, Source=volume, Type="volume", ReadOnly=True),
        ],
    )


@app.post("/backup/{name}")
async def post_backup_container(
    name: str,
    repository: Repository,
    response: Response,
    client: Client,
    queue: AppQueue,
    metrics: AppMetrics,
) -> Container | None:
    container = await container_by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return

    for volume in container.volumes:
        backup = instrument_backup(metrics, run_container, container=name, volume=volume)
        configuration = configure_backup(name, volume, repository)
        await queue.put(backup, configuration, "backup")

    return container


@app.post("/backup")
async def post_backup(
    repository: Repository, client: Client, queue: AppQueue, metrics: AppMetrics
) -> list[Container]:
    containers = await list_containers(client)
    containers_info = chain.from_iterable(map(Container.iter_volumes, containers))

    for container, volume in containers_info:
        backup = instrument_backup(metrics, run_container, container=container, volume=volume)
        configuration = configure_backup(container, volume, repository)
        await queue.put(backup, configuration, "backup")

    return containers
