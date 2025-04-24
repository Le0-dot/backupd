from pathlib import Path

from backupd.config.entry import read_entries
from backupd.restic import check, init


def main() -> None:
    entries = read_entries(Path("example-config"))
    entry = entries["test"]
    repo = entry.repo.read()

    if check(repo).failure:
        if (result := init(repo)).failure:
            print(result.stdout)
            raise RuntimeError("failed to initialize repository")

    print(check(repo).stdout)
