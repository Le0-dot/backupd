import subprocess
from enum import StrEnum
from pathlib import Path
from typing import Annotated, TextIO

from docker.types import Mount
import toml
from pydantic import (
    PlainSerializer,
    field_serializer,
    field_validator,
    model_serializer,
)

from backupd.config import CustomBase, NonEmptyStr
from backupd.config.source import RepoSource, source_kind


type OptionalList[T] = Annotated[list[T], PlainSerializer(lambda x: x or None)]


class Hook(CustomBase):
    cmd: NonEmptyStr

    def run(self) -> None:
        _ = subprocess.run(self.cmd, shell=True, capture_output=True, check=True)
        # TODO: Log outputs


class Hooks(CustomBase):
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

class Entry(CustomBase):
    kind: Annotated[EntryKind, PlainSerializer(str)] = EntryKind.FILE
    where: NonEmptyStr
    repo: RepoSource
    hooks: Hooks

    @field_validator("repo", mode="before")
    @classmethod
    def validate_repo(cls, repo: dict[str, dict[str, str]]) -> RepoSource:
        if len(repo) > 1:
            raise ValueError("only 1 repository source is pertimetted per entry")
        kind, data = repo.popitem()
        return source_kind(kind).model_validate(data)

    @field_serializer("repo")
    def serialize_repo(self, repo: RepoSource) -> dict[str, dict[str, str]]:
        return {repo.kind(): repo.model_dump()}


def read_entries(dir: Path) -> dict[str, Entry]:
    entries: dict[str, Entry] = {}
    for path in dir.rglob("*.toml"):
        with path.open() as file:
            entry = Entry.model_validate(toml.load(file))
            entries |= {path.stem: entry}

    return entries


def write_entry(file: TextIO, model: Entry) -> None:
    _ = toml.dump(model.model_dump(), file)
