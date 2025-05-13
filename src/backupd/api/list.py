from http import HTTPStatus
from typing import Self

from fastapi import APIRouter, Response
from pydantic import BaseModel, TypeAdapter

from backupd.docker import Client, ContainerInspect, VolumeInspect, run_container
from backupd.restic.flags import Group, Tag
from backupd.restic.snapshots import Snapshot, SnapshotGroupping, snapshots

router = APIRouter(prefix="/list")


class VolumeModel(BaseModel):
    name: str

    @classmethod
    def from_inspect(cls, inspect: VolumeInspect) -> Self:
        return cls(name=inspect.Name)


class ContainerModel(BaseModel):
    name: str
    volumes: list[str]

    @classmethod
    def from_inspect(cls, inspect: ContainerInspect) -> Self:
        volumes = map(lambda m: m.Name, inspect.volumes)
        return cls(name=inspect.name, volumes=list(volumes))


@router.get("/volume")
async def list_all_volumes(client: Client) -> list[VolumeModel]:
    volumes = await VolumeInspect.all(client)
    named = filter(lambda v: not v.anonymous, volumes)
    names = map(VolumeModel.from_inspect, named)
    return list(names)


@router.get("/volume/{name}")
async def list_volume(
    name: str, response: Response, client: Client
) -> VolumeModel | None:
    volume = await VolumeInspect.by_name(client, name)

    if volume is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    return VolumeModel.from_inspect(volume)


@router.get("/container")
async def list_all_containers(client: Client) -> list[ContainerModel]:
    containers = await ContainerInspect.all(client)
    models = map(ContainerModel.from_inspect, containers)
    return list(models)


@router.get("/container/{name}")
async def list_container(
    name: str, response: Response, client: Client
) -> ContainerModel | None:
    container = await ContainerInspect.by_name(client, name)

    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    return ContainerModel.from_inspect(container)


@router.get("/snapshot")
async def list_all_snapshots(
    response: Response, client: Client
) -> list[SnapshotGroupping] | None:
    configuration = snapshots([], Group.tags())

    result = await run_container(client, configuration, "backupd-retrieve")
    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[SnapshotGroupping]).validate_json(result.stdout)
    return model


@router.get("/snapshot/volume/{name}")
async def list_snapshots_for_volume(
    name: str, response: Response, client: Client
) -> list[Snapshot] | None:
    configuration = snapshots([Tag.volume(name)], Group())

    result = await run_container(client, configuration, "backupd-retrieve")
    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[Snapshot]).validate_json(result.stdout)
    return model


@router.get("/snapshot/container/{name}")
async def list_snapshots_for_container(
    name: str, response: Response, client: Client
) -> list[SnapshotGroupping] | None:
    container = await ContainerInspect.by_name(client, name)
    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    tags = [Tag.volume(volume.Name) for volume in container.volumes]
    configuration = snapshots(tags, Group.tags())

    result = await run_container(client, configuration, "backupd-retrieve")
    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[SnapshotGroupping]).validate_json(result.stdout)
    return model
