from collections.abc import Iterable, Iterator, Mapping
from dataclasses import dataclass
from functools import cached_property
from itertools import starmap
from typing import ClassVar, Self, cast, override

from pydantic import BaseModel, RootModel, model_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    runner_image: str
    timeout_seconds: int = 60 * 5

    model_config: ClassVar[SettingsConfigDict] = SettingsConfigDict(
        env_prefix="BACKUPD_", enable_decoding=False
    )

    def __init__(self) -> None:
        super().__init__()


type NestedMapping[T, U] = Mapping[T, U | "NestedMapping"]
type TreePath[T] = tuple[T, ...]


@dataclass
class DictTree[T, U](Iterable[tuple[TreePath[T], U]]):
    mapping: NestedMapping[T, U]

    @override
    def __iter__(self) -> Iterator[tuple[TreePath[T], U]]:
        for key, value in self.mapping.items():
            if not isinstance(value, Mapping):
                yield (key,), value
                continue

            nested = cast(NestedMapping[T, U], value)
            for path, leaf in DictTree(nested):
                yield (key, *path), leaf


class Restic(BaseModel):
    repository: str
    password: str
    host: str = "backupd"

    @property
    def backend(self) -> str:
        if len(split := self.repository.split(":")) == 1:
            return "local"
        return split[0]

    @property
    def location(self) -> list[str]:
        if len(split := self.repository.split(":")) == 1:
            return split
        return split[1:]

    @model_validator(mode="after")
    def validate_backend(self) -> Self:
        supported_backends = ["local", "rclone"]
        if self.backend not in supported_backends:
            raise ValueError(f"{self.backend} is not a supported backend")
        return self


class Rclone(RootModel[dict[str, str]]):
    def has_remote(self, remote: str) -> bool:
        definition = f"config_{remote}_type"
        return definition in self.root


class RepositorySettings(BaseSettings):
    restic: Restic
    rclone: Rclone | None = None

    model_config: ClassVar[SettingsConfigDict] = SettingsConfigDict(
        env_nested_delimiter="_",
        env_nested_max_split=1,
        enable_decoding=False,
    )

    def __init__(self) -> None:
        super().__init__()

    @model_validator(mode="after")
    def validate_rclone(self) -> Self:
        if self.restic.backend != "rclone":
            return self

        if self.rclone is None:
            raise ValueError(
                f"no rclone configuration for {self.restic.repository} repository"
            )

        remote, _path = self.restic.location
        if not self.rclone.has_remote(remote):
            raise ValueError(f"rclone has no definition of {remote} remote")

        return self

    @cached_property
    def env(self) -> list[str]:
        def to_env(path: TreePath[str], value: str) -> str:
            key = "_".join(path).upper()
            return f"{key}={value}"

        tree = DictTree(self.model_dump())
        variables = starmap(to_env, tree)
        return list(variables)
