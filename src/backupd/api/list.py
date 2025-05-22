from http import HTTPStatus
import logging

from fastapi import APIRouter, Response
from pydantic import TypeAdapter

from backupd.docker import DockerClient, run_container
from backupd.restic.flags import GroupFlag, TagFlag
from backupd.restic.snapshots import Snapshot, SnapshotGroupping, snapshots

router = APIRouter(prefix="/list")


@router.get("/volume")
async def list_all_volumes(client: DockerClient) -> list[str]:
    return await client.volumes()


@router.get("/volume/{name}")
async def list_volume(
    name: str, response: Response, client: DockerClient
) -> str | None:
    if await client.volume_exists(name):
        return name

    response.status_code = HTTPStatus.NOT_FOUND
    return None


@router.get("/container")
async def list_all_containers(client: DockerClient) -> dict[str, list[str]]:
    containers = await client.containers()
    models = {name: await client.volumes_from(name) or [] for name in containers}
    return models


@router.get("/container/{name}")
async def list_container(
    name: str, response: Response, client: DockerClient
) -> dict[str, list[str]] | None:
    if (volumes := await client.volumes_from(name)) is not None:
        return {name: volumes}

    response.status_code = HTTPStatus.NOT_FOUND
    return None


@router.get("/snapshot")
async def list_all_snapshots(
    response: Response, client: DockerClient
) -> list[SnapshotGroupping] | None:
    configuration = snapshots([], GroupFlag.tags())
    logging.debug("retrieving snapshots", extra={"cmd": configuration.Cmd})

    result = await run_container(client.docker, configuration, "backupd-retrieve")

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
    name: str, response: Response, client: DockerClient
) -> list[Snapshot] | None:
    configuration = snapshots([TagFlag.for_volume(name)], GroupFlag())
    logging.debug("retrieving snapshots", extra={"cmd": configuration.Cmd})

    result = await run_container(client.docker, configuration, "backupd-retrieve")

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
    name: str, response: Response, client: DockerClient
) -> list[SnapshotGroupping] | None:
    volumes = await client.volumes_from(name)
    if volumes is None:
        response.status_code = HTTPStatus.NOT_FOUND
        return None

    tags = map(TagFlag.for_volume, volumes)
    configuration = snapshots(tags, GroupFlag.tags())
    logging.debug("retrieving snapshots", extra={"cmd": configuration.Cmd})

    result = await run_container(client.docker, configuration, "backupd-retrieve")

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
