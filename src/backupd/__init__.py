from contextlib import asynccontextmanager
from functools import partial
from http import HTTPStatus
from itertools import chain
import logging

from fastapi import FastAPI, Response
from logfmter import Logfmter

from backupd.backup import BackupJob
from backupd.docker import (
    Client,
    Container,
    container_by_name,
    list_containers,
)
from backupd.metrics import (
    APIMetricsMiddleware,
    metrics_lifespan,
)
from backupd.repository import Repository
from backupd.settings import Settings
from backupd.task_queue import AppQueue, queue_lifespan


@asynccontextmanager
async def app_lifespan(app: FastAPI):
    async with metrics_lifespan(app), queue_lifespan(app):
        yield


_ = Settings()  # validate settings
app = FastAPI(lifespan=app_lifespan)
app.add_middleware(APIMetricsMiddleware)

logger =logging.getLogger("uvicorn")
logger.handlers[0].setFormatter(Logfmter())


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
