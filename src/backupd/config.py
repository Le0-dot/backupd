from enum import StrEnum
from pathlib import Path
from typing import Annotated, Self, TextIO

import toml
from annotated_types import MinLen
from pydantic import (
    BaseModel,
    ConfigDict,
    PlainSerializer,
    model_serializer,
    model_validator,
)


class NoExtraModel(BaseModel):
    model_config = ConfigDict(extra="forbid")


NonEmptyStr = Annotated[str, MinLen(1)]


class Hook(NoExtraModel):
    cmd: NonEmptyStr


type OptionalList[T] = Annotated[list[T], PlainSerializer(lambda x: x or None)]


class Hooks(NoExtraModel):
    start: OptionalList[Hook] = []
    success: OptionalList[Hook] = []
    failure: OptionalList[Hook] = []

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
    DIRECTORY = "directory"
    VOLUME = "volume"


class Entry(NoExtraModel):
    kind: Annotated[EntryKind, PlainSerializer(str)]
    where: NonEmptyStr
    storage: NonEmptyStr
    hooks: Hooks


class Storage(NoExtraModel):
    path: NonEmptyStr
    secret: NonEmptyStr | None = None
    command: NonEmptyStr | None = None

    @model_validator(mode="after")
    def validate_secrets(self) -> Self:
        if self.secret is None and self.command is None:
            raise ValueError("either 'secret' or 'command' must be defined")
        if self.secret is not None and self.command is not None:
            raise ValueError("'secret' and 'command' are mutually exclusive")
        return self


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
