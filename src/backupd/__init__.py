from pathlib import Path

import docker

from backupd.config import Entry, EntryKind, Storage, read_configs
from backupd.restic import backup, check, init, snapshots

client = docker.from_env()


def backup_entry(storage: Storage, entry: Entry, tag: str) -> None:
    entry.hooks.run_start()

    restic = backup(storage, entry.where, tag)

    match entry.kind:
        case EntryKind.FILE:
            result = restic.run()
        case EntryKind.VOLUME:
            result = restic.run_docker(entry.to_mount())

    entry.hooks.run_complete(result.success)


def main() -> None:
    storages, entries = read_configs(Path("example-config"))

    local = storages["local"]

    if check(local).run().failure:
        print(init(local).run().stdout)

    backup_entry(storages["local"], entries["test"], "backupd:test")

    print(snapshots(local).run().stdout)
