import sys
from pathlib import Path

from .config import Entry, Storage, read_config, write_config


def main() -> None:
    for filename in Path("example-config").rglob("**/*.storage"):
        with filename.open() as file:
            storage = read_config(file, Storage)
        write_config(sys.stdout, storage)
    for filename in Path("example-config").rglob("**/*.entry"):
        with filename.open() as file:
            entry = read_config(file, Entry)
        write_config(sys.stdout, entry)
