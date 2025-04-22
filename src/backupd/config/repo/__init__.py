from functools import cache
from importlib import import_module
from types import ModuleType
from typing import Any, Protocol, Self, runtime_checkable

from backupd.config import NonEmptyStr


@runtime_checkable
class Repo(Protocol):
    kind: str
    path: NonEmptyStr
    password: NonEmptyStr

    @property
    def env(self) -> dict[str, str]: ...

    @classmethod
    def model_validate(cls, obj: Any) -> Self: ...


@runtime_checkable
class RepoModule(Protocol):
    Repository: type[Repo]


@cache
def kinds() -> dict[str, type]:
    module_names = [".local", ".rclone"]
    modules = (import_module(name, __package__) for name in module_names)

    def get_class(module: ModuleType) -> type:
        cls = getattr(module, "Repository", None)
        if not isinstance(cls, type) or not hasattr(cls, "kind"):
            raise RuntimeError("repository module does not have a repository class")
        return cls

    classes = map(get_class, modules)

    return {cls.kind: cls for cls in classes}


def from_kind(kind: str) -> type:
    if kind not in kinds():
        raise ValueError(f"repository with '{kind}' kind was not registred")
    return kinds()[kind]
