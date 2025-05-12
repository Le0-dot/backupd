import logging
from contextlib import asynccontextmanager
from http import HTTPStatus
from itertools import chain
from typing import Annotated, Self

from fastapi import Depends, FastAPI, Request, Response
from logfmter import Logfmter
from prometheus_client import make_asgi_app
from pydantic import BaseModel, PlainSerializer, ValidationError

from backupd.docker import (
    Client,
    ContainerInspect,
    run_container,
)
from backupd.metrics import APIMetricsMiddleware
from backupd.restic.restore import restore_latest
from backupd.restic.snapshots import Snapshot, Snapshots, snapshots
from backupd.schedule.jobs import BackupJob
from backupd.schedule.queue import TaskQueue
from backupd.settings import RepositorySettings, Settings


@asynccontextmanager
async def lifespan(app: FastAPI):
    _ = Settings()  # validate settings
    _ = RepositorySettings()  # validate restic

    app.mount("/metrics", make_asgi_app())

    app.state.queue = TaskQueue()
    app.state.queue.start()

    yield

    await app.state.queue.shutdown()


def get_task_queue(request: Request) -> TaskQueue:
    return request.app.state.queue


AppQueue = Annotated[TaskQueue, Depends(get_task_queue)]

app = FastAPI(lifespan=lifespan)
app.add_middleware(APIMetricsMiddleware)

# logger = logging.getLogger("uvicorn")
# logger.handlers[0].setFormatter(Logfmter())


class ContainerModel(BaseModel):
    name: Annotated[str, PlainSerializer(lambda s: s.removeprefix("/"))]
    volumes: list[str]

    @classmethod
    def from_inspect(cls, inspect: ContainerInspect) -> Self:
        volumes = [mount.Name for mount in inspect.volumes]
        return cls(name=inspect.Name, volumes=volumes)


@app.get("/list/container")
async def get_containers(client: Client) -> list[ContainerModel]:
    containers = await ContainerInspect.all(client)
    return list(map(ContainerModel.from_inspect, containers))


@app.get("/list/container/{name}")
async def get_container(
    name: str, response: Response, client: Client
) -> ContainerModel | None:
    container = await ContainerInspect.by_name(client, name)

    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    return ContainerModel.from_inspect(container)


@app.get("/list/snapshot")
async def list_snapshots(
    response: Response,
    client: Client,
    tags: str = "backupd",
) -> list[Snapshot] | None:
    configuration = snapshots(tags)

    result = await run_container(client, configuration, "backupd-retrieve")
    if not result.success:
        response.status_code = HTTPStatus.BAD_REQUEST
        return None

    try:
        return Snapshots.validate_json(result.stdout)
    except ValidationError:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None


@app.post("/backup")
async def backup_all_containers(
    client: Client,
    queue: AppQueue,
) -> list[ContainerModel]:
    containers = await ContainerInspect.all(client)

    jobs = map(BackupJob.for_container, containers)
    await queue.put(*chain.from_iterable(jobs))

    return list(map(ContainerModel.from_inspect, containers))


@app.post("/backup/{name}")
async def backup_container(
    name: str,
    response: Response,
    client: Client,
    queue: AppQueue,
) -> ContainerModel | None:
    container = await ContainerInspect.by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    jobs = BackupJob.for_container(container)
    await queue.put(*jobs)

    return ContainerModel.from_inspect(container)


@app.post("/restore/container/{name}")
async def restore_container_latest(
    name: str, response: Response, client: Client
) -> None:
    inspect = await ContainerInspect.by_name(client, name)

    if inspect is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return

    for volume in inspect.Mounts:
        result = await run_container(
            client, restore_latest(volume.Name), "backupd-restore"
        )
        print(result)
