import sys
from pathlib import Path

from .config import read_configs, write_config


def main() -> None:
    storages, entries = read_configs(Path("example-config"))

    for name, value in storages.items():
        print()
        print(name)
        print()
        write_config(sys.stdout, value)

    for name, value in entries.items():
        print()
        print(name)
        print()
        write_config(sys.stdout, value)
