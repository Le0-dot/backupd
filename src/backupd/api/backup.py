from fastapi import APIRouter

from backupd.restic.backup import BackupMessage

router = APIRouter(prefix="/backup")

type VolumeBackup = list[BackupMessage]
type VolumesBackup = dict[str, VolumeBackup]
type ContainerBackup = dict[str, VolumeBackup]
type ConatinersBackup = dict[str, ContainerBackup]


@router.post("/backup/volume")
async def backup_all_volumes() -> VolumesBackup:
    return NotImplemented


@router.post("/backup/volume/{name}")
async def backup_volume() -> VolumeBackup:
    return NotImplemented


@router.post("/backup/container")
async def backup_all_conatiner() -> ConatinersBackup:
    return NotImplemented


@router.post("/backup/container/{name}")
async def backup_container() -> ContainerBackup:
    return NotImplemented
