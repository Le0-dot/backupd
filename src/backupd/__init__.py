from collections.abc import Iterable
from http import HTTPStatus
from itertools import chain

from fastapi import FastAPI, Response
from prometheus_client import make_asgi_app

from backupd.docker import (
    Client,
    Container,
    ContainerCreate,
    Mount,
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
    container = await container_by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
    return container


@app.get("/containers")
async def get_containers(client: Client) -> list[Container]:
    return await list_containers(client)


def configure_container_backup(
    container: Container, repository: Repository
) -> Iterable[ContainerCreate]:
    backup_dir = "/data"
    return map(
        lambda volume: ContainerCreate.shell(
            image=docker_image,
            cmd=f"{repository.preexec} &&" + "echo 123",  # NOTE: For testing purposes
            # cmd=f"{repository.preexec} && restic check",
            # cmd=f"{repository.preexec} && backup {backup_dir} --tag backupd:{container.name}:{volume}",
            env=repository.env,
            mounts=[
                repository.mount,
                Mount(Target=backup_dir, Source=volume, Type="volume", ReadOnly=True),
            ],
        ),
        container.volumes,
    )


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

    configs = configure_container_backup(container, repository)
    for config in configs:
        await queue.put(run_container, config, "backup")

    return container


@app.post("/backup")
async def post_backup(
    repository: Repository, client: Client, queue: AppQueue
) -> list[Container]:
    containers = await list_containers(client)
    configs = map(
        lambda container: configure_container_backup(container, repository), containers
    )
    for config in chain.from_iterable(configs):
        await queue.put(run_container, config, "backup")

    return containers
