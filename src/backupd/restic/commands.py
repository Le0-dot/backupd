from collections.abc import Iterable
from pathlib import Path
from typing import Literal

from backupd.restic.flags import GroupFlag, TagFlag


def snapshots(
    *,
    snapshot_id: str | None = None,
    tags: Iterable[TagFlag] | None = None,
    groupping: GroupFlag | None = None,
) -> list[str]:
    cmd = ("--json", "snapshots", snapshot_id, *(tags or []), groupping)
    return list(map(str, filter(None, cmd)))


def backup(*, volume: str, mountpoint: Path) -> list[str]:
    cmd = (
        "--verbose=2",
        "--json",
        "backup",
        TagFlag.for_app() | TagFlag.for_volume(volume),
        GroupFlag.tags(),
        mountpoint,
    )
    return list(map(str, cmd))


def restore(
    *, volume: str, mountpoint: Path, snapshot_id: str | Literal["latest"] = "latest"
) -> list[str]:
    cmd = (
        "--verbose=2",
        "--json",
        "restore",
        f"{snapshot_id}:{mountpoint.stem}",
        TagFlag.for_volume(volume),
        "--target",
        mountpoint,
    )
    return list(map(str, cmd))
