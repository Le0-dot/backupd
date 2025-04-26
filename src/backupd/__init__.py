from http import HTTPStatus

from fastapi import FastAPI, Response
from prometheus_client import make_asgi_app

from backupd.docker import (
    Client,
    Container,
    Mount,
    configure_backup,
    container_by_name,
    list_containers,
    run_container,
)
from backupd.repository import Repository
from backupd.task_queue import AppQueue, queue_lifespan

app = FastAPI(lifespan=queue_lifespan)
app.mount("/metrics", make_asgi_app())

docker_image = "docker.io/instrumentisto/restic:0.18"


@app.get("/container/{name}")
async def get_container(
    name: str, response: Response, client: Client
) -> Container | None:
    container = await container_by_name(name, client)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
    return container


@app.get("/containers")
async def get_containers(client: Client) -> list[Container]:
    return await list_containers(client)


@app.post("/backup")
async def post_backup(repository: Repository, client: Client, queue: AppQueue) -> None:
    pass  # TODO: Add backup of all containers


def list_from[T](value: T | None) -> list[T]:
    if value is None:
        return []
    return [value]


@app.post("/backup/{name}")
async def post_backup_container(
    name: str,
    repository: Repository,
    response: Response,
    client: Client,
    queue: AppQueue,
) -> None:
    container = await container_by_name(name, client)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return

    configs = (
        configure_backup(
            image=docker_image,
            mounts=list_from(repository.mount)
            + [Mount(Target="/data", Source=volume, Type="volume", ReadOnly=True)],
            env=repository.env,
            preexec=repository.preexec,
            backup_dir="/data",
            backup_tag=f"backupd:{container.name}:{volume}",
        )
        for volume in container.volumes
    )
    backups = (
        run_container(config, "backup") for config in configs
    )  # Cannot pass current client as it will be closed by the time the coroutine is executed
    for backup in backups:
        await queue.put(
            backup
        )  # TODO: Make wrapper function that will log, add metrics, etc. to the backup and return None
