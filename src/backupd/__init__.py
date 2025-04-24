from fastapi import FastAPI, Response
from starlette.status import HTTP_404_NOT_FOUND

from backupd.docker import Client, Container, container_by_name, list_containers

app = FastAPI()


@app.get("/container/{name}")
def get_container(name: str, response: Response, client: Client) -> Container | None:
    container = container_by_name(name, client)
    if container is None:
        response.status_code = HTTP_404_NOT_FOUND
    return container


@app.get("/containers")
def get_containers(client: Client) -> list[Container]:
    print(list_containers(client))
    return list_containers(client)


@app.post("/backup")
def post_backup(client: Client) -> None:
    pass  # TODO: Add backup of all containers


@app.post("/backup/{name}")
def post_backup_container(name: str, client: Client) -> None:
    pass  # TODO: Add backup of individual container


@app.get("/metrics")
def get_metrics() -> str:
    return NotImplemented  # TODO: Add metrics to monitor the backups
