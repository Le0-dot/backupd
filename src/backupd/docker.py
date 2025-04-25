from collections.abc import Iterable
from typing import Annotated

from aiodocker import Docker, DockerError
from fastapi import Depends
from pydantic import BaseModel


async def client_dependency():
    client = Docker()
    try:
        yield client
    finally:
        await client.close()


Client = Annotated[Docker, Depends(client_dependency)]


class Container(BaseModel):
    name: str
    volumes: list[str]


def container_by_name(name: str, client: docker.DockerClient) -> Container | None:
    try:
        container = client.containers.get(name)
    except docker.errors.NotFound:
        return None

    mounts: Iterable[dict[str, str]] = container.attrs["Mounts"]
    volumes = filter(lambda mount: mount["Type"] == "volume", mounts)
    names = map(lambda volume: volume["Name"], volumes)

    return Container(name=name, volumes=list(names))


def list_containers(client: docker.DockerClient) -> list[Container]:
    containers = client.containers.list()
    names: Iterable[str] = map(lambda container: container.name, containers)

    container_list = map(lambda name: container_by_name(name, client), names)
    return list(filter(None, container_list))
