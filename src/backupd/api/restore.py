from fastapi import APIRouter

from backupd.restic.restore import RestoreMessage

router = APIRouter(prefix="/restore")

type RestoreSnapshot = list[RestoreMessage]
type RestoreVolume = list[RestoreMessage]
type RestoreContainer = dict[str, RestoreVolume]


@router.post("/restore/{snapshot}")
async def restore_snapshot(snapshot: str) -> RestoreSnapshot:
    return NotImplemented


@router.post("/restore/volume/{name}")
async def restore_volume(name: str) -> RestoreVolume:
    return NotImplemented


@router.post("/restore/container/{name}")
async def restore_container(name: str) -> RestoreContainer:
    return NotImplemented
