from http import HTTPStatus
from fastapi import FastAPI, Response

from backupd.docker import Client, Container, container_by_name, list_containers
from backupd.repository import Repository

app = FastAPI()


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
async def post_backup(repository: Repository, client: Client) -> None:
    pass  # TODO: Add backup of all containers


@app.post("/backup/{name}")
async def post_backup_container(
    name: str,
    repository: Repository,
    client: Client,
) -> None:
    pass  # TODO: Add backup of individual container


@app.get("/metrics")
async def get_metrics() -> str:
    return NotImplemented  # TODO: Add metrics to monitor the backups
