from http import HTTPStatus
from typing import Self
from fastapi import APIRouter, Response
from pydantic import BaseModel

from backupd.docker import Client, ContainerInspect, VolumeInspect
from backupd.restic.snapshots import Snapshot

router = APIRouter(prefix="/list")


class ContainerModel(BaseModel):
    name: str
    volumes: list[str]

    @classmethod
    def from_inspect(cls, inspect: ContainerInspect) -> Self:
        volumes = map(lambda m: m.Name, inspect.volumes)
        return cls(name=inspect.Name, volumes=list(volumes))


@router.get("/volume")
async def list_all_volumes(client: Client) -> list[str]:
    volumes = await VolumeInspect.all(client)
    named = filter(lambda v: not v.anonymous, volumes)
    names = map(lambda v: v.Name, named)
    return list(names)


@router.get("/volume/{name}")
async def list_volume(name: str, response: Response, client: Client) -> str | None:
    volume = await VolumeInspect.by_name(client, name)

    if volume is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    return volume.Name


@router.get("/container")
async def list_all_containers() -> list[ContainerModel]:
    return NotImplemented


@router.get("/container/{name}")
async def list_container(name: str) -> ContainerModel | None:
    return NotImplemented


@router.get("/snapshot")
async def list_all_snapshots() -> list[Snapshot]:
    return NotImplemented


@router.get("/snapshot/volume/{name}")
async def list_snapshots_for_volume(name: str) -> list[Snapshot]:
    return NotImplemented


@router.get("/snapshot/container/{name}")
async def list_snapshots_for_container(name: str) -> list[Snapshot]:
    return NotImplemented
