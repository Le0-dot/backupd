from http import HTTPStatus
import logging
from typing import Self

from fastapi import APIRouter, Response
from pydantic import BaseModel, TypeAdapter

from backupd.docker import Client, ContainerInspect, VolumeInspect, run_container
from backupd.restic.flags import GroupFlag, TagFlag
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
    async def from_inspect(cls, client: Client, inspect: ContainerInspect) -> Self:
        volumes = await inspect.volumes(client)
        names = [volume.Name for volume in volumes]
        return cls(name=inspect.name, volumes=names)


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
    models = [
        await ContainerModel.from_inspect(client, container) for container in containers
    ]
    return models


@router.get("/container/{name}")
async def list_container(
    name: str, response: Response, client: Client
) -> ContainerModel | None:
    container = await ContainerInspect.by_name(client, name)

    if container is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    return await ContainerModel.from_inspect(client, container)


@router.get("/snapshot")
async def list_all_snapshots(
    response: Response, client: Client
) -> list[SnapshotGroupping] | None:
    configuration = snapshots([], GroupFlag.tags())
    logging.debug("retrieving snapshots", extra={"cmd": configuration.Cmd})

    result = await run_container(client, configuration, "backupd-retrieve")

    status = ["failure", "success"][result.success]
    level = [logging.ERROR, logging.INFO][result.success]

    logging.log(level, "finished retrieving snapshots", extra={"status": status})
    logging.debug(result.stdout, extra={"status": status, "stream": "stdout"})
    logging.debug(result.stderr, extra={"status": status, "stream": "stderr"})

    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[SnapshotGroupping]).validate_json(result.stdout)
    return model


@router.get("/snapshot/volume/{name}")
async def list_snapshots_for_volume(
    name: str, response: Response, client: Client
) -> list[Snapshot] | None:
    configuration = snapshots([TagFlag.for_volume(name)], GroupFlag())
    logging.debug("retrieving snapshots", extra={"cmd": configuration.Cmd})

    result = await run_container(client, configuration, "backupd-retrieve")

    status = ["failure", "success"][result.success]
    level = [logging.ERROR, logging.INFO][result.success]

    logging.log(level, "finished retrieving snapshots", extra={"status": status})
    logging.debug(result.stdout, extra={"status": status, "stream": "stdout"})
    logging.debug(result.stderr, extra={"status": status, "stream": "stderr"})

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

    volumes = await container.volumes(client)
    tags = [TagFlag.for_volume(volume.Name) for volume in volumes]
    configuration = snapshots(tags, GroupFlag.tags())
    logging.debug("retrieving snapshots", extra={"cmd": configuration.Cmd})

    result = await run_container(client, configuration, "backupd-retrieve")

    status = ["failure", "success"][result.success]
    level = [logging.ERROR, logging.INFO][result.success]

    logging.log(level, "finished retrieving snapshots", extra={"status": status})
    logging.debug(result.stdout, extra={"status": status, "stream": "stdout"})
    logging.debug(result.stderr, extra={"status": status, "stream": "stderr"})

    if not result.success:
        response.status_code = HTTPStatus.FAILED_DEPENDENCY
        return None

    model = TypeAdapter(list[SnapshotGroupping]).validate_json(result.stdout)
    return model
