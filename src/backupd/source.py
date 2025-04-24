import subprocess
from abc import ABC, abstractmethod
from typing import Annotated, final, override

import toml
from pydantic import (
    FilePath,
    PlainSerializer,
)
from pykeepass import PyKeePass
from pykeepass.exceptions import CredentialsError

from backupd.config import CustomBase, NonEmptyStr
from backupd.config.repo import Repo, from_kind


class RepoSource(CustomBase, ABC):
    name: Annotated[FilePath, PlainSerializer(str)]

    @classmethod
    @abstractmethod
    def kind(cls) -> str:
        pass

    @abstractmethod
    def _load(self) -> dict[str, str]:
        pass

    @final
    def read(self) -> Repo:
        data = self._load()

        if (kind := data.pop("kind", None)) is None:
            raise ValueError("no 'kind' key in repository configuration")

        instance = from_kind(kind).model_validate(data)

        if isinstance(instance, Repo):
            return instance
        raise RuntimeError(f"repository with '{kind}' kind does not implement the protocol")


class FileRepoSource(RepoSource):
    @override
    @classmethod
    def kind(cls) -> str:
        return "file"

    @override
    def _load(self) -> dict[str, str]:
        return toml.loads(self.name.read_text())


class KeePassRepoSource(RepoSource):
    passcmd: NonEmptyStr
    entry: NonEmptyStr

    @override
    @classmethod
    def kind(cls) -> str:
        return "keepass"

    @override
    def _load(self) -> dict[str, str]:
        password = subprocess.run(
            self.passcmd, shell=True, capture_output=True, check=True
        ).stdout.decode()

        try:
            kdb = PyKeePass(str(self.name), password)
        except CredentialsError as e:
            raise ValueError(
                f"invalid credentials for '{self.name}'\n"
                + "check if 'passcmd' adds a newline ending to the password"
            ) from e

        entry = kdb.find_entries(title=self.entry, first=True)
        if entry is None or isinstance(entry, list):
            raise ValueError(
                f"entry with title '{self.entry}' does not exists in '{self.name}'"
            )

        return entry.custom_properties | {"password": entry.password}


def source_kind(kind: str) -> type[RepoSource]:
    kinds = {cls.kind(): cls for cls in RepoSource.__subclasses__()}
    if kind not in kinds:
        raise ValueError(f"repository source '{kind}' does not exists")
    return kinds[kind]
