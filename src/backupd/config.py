import subprocess
from enum import StrEnum
from pathlib import Path
from typing import Annotated, TextIO

import toml
import docker.types
from annotated_types import MinLen
from pydantic import (
    BaseModel,
    ConfigDict,
    PlainSerializer,
    model_serializer,
)


class NoExtraModel(BaseModel):
    model_config = ConfigDict(extra="forbid")


NonEmptyStr = Annotated[str, MinLen(1)]


class Hook(NoExtraModel):
    cmd: NonEmptyStr

    def run(self) -> None:
        _ = subprocess.run(self.cmd, shell=True, capture_output=True, check=True)
        # TODO: Log outputs


type OptionalList[T] = Annotated[list[T], PlainSerializer(lambda x: x or None)]


class Hooks(NoExtraModel):
    start: OptionalList[Hook] = []
    success: OptionalList[Hook] = []
    failure: OptionalList[Hook] = []

    def run_start(self) -> None:
        for hook in self.start:
            hook.run()

    def run_complete(self, success: bool) -> None:
        if success:
            hooks = self.success
        else:
            hooks = self.failure

        for hook in hooks:
            hook.run()

    @model_serializer
    def serialize(self) -> dict[str, OptionalList[Hook]] | None:
        match self:
            case Hooks(start=[], success=[], failure=[]):
                return None
            case _:
                return {
                    "start": self.start,
                    "success": self.success,
                    "failure": self.failure,
                }


class EntryKind(StrEnum):
    FILE = "file"
    VOLUME = "volume"


class Entry(NoExtraModel):
    kind: Annotated[EntryKind, PlainSerializer(str)] = EntryKind.FILE
    where: NonEmptyStr
    storage: NonEmptyStr
    hooks: Hooks

    def to_mount(self, path: str = "/data") -> docker.types.Mount:
        types = {EntryKind.FILE: "bind", EntryKind.VOLUME: "volume"}
        return docker.types.Mount(
            path, self.where, type=types[self.kind], read_only=True
        )


class StorageKind(StrEnum):
    LOCAL = "local"
    RCLONE = "rclone"


class Storage(NoExtraModel):
    kind: Annotated[StorageKind, PlainSerializer(str)] = StorageKind.LOCAL
    path: NonEmptyStr
    secret: NonEmptyStr
    # secret: NonEmptyStr | None = None
    # command: NonEmptyStr | None = None

    # @model_validator(mode="after")
    # def validate_secrets(self) -> Self:
    #     if self.secret is None and self.command is None:
    #         raise ValueError("either 'secret' or 'command' must be defined")
    #     if self.secret is not None and self.command is not None:
    #         raise ValueError("'secret' and 'command' are mutually exclusive")
    #     return self


def read_config[T: BaseModel](file: TextIO, cls: type[T]) -> T:
    return cls.model_validate(toml.load(file))


def read_configs(dir: Path) -> tuple[dict[str, Storage], dict[str, Entry]]:
    storages: dict[str, Storage] = {}
    for path in dir.rglob("*.storage"):
        with path.open() as file:
            storages |= {path.stem: read_config(file, Storage)}

    entries: dict[str, Entry] = {}
    for path in dir.rglob("*.entry"):
        with path.open() as file:
            entry = read_config(file, Entry)

            if entry.storage not in storages:
                raise ValueError(
                    f"entry {path.stem} is configured with storage {entry.storage} which does not exists"
                )

            entries |= {path.stem: entry}

    return storages, entries


def write_config(file: TextIO, model: BaseModel) -> None:
    _ = toml.dump(model.model_dump(), file)
