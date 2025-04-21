from typing import override
from backupd.config import CustomBase, NonEmptyStr
from abc import ABC, abstractmethod


class Repo(CustomBase, ABC):
    path: NonEmptyStr
    password: NonEmptyStr

    @classmethod
    @abstractmethod
    def kind(cls) -> str:
        pass

    def to_env(self) -> dict[str, str]:
        return {
            "RESTIC_REPOSITORY": f"{self.kind}:{self.path}",
            "RESTIC_PASSWORD": self.password,
        }


class LocalRepo(Repo):
    @override
    @classmethod
    def kind(cls) -> str:
        return "local"


class RcloneRepo(Repo):
    config: str

    @override
    @classmethod
    def kind(cls) -> str:
        return "rclone"


def repo_kind(kind: str) -> type[Repo]:
    kinds = {cls.kind(): cls for cls in Repo.__subclasses__()}
    if kind not in kinds:
        raise ValueError(f"unknown repository kind '{kind}'")
    return kinds[kind]
