from collections.abc import Callable
from itertools import chain
from typing import Annotated, ClassVar, Self

from pydantic import (
    BaseModel,
    PlainSerializer,
    RootModel,
    model_serializer,
    model_validator,
)
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    runner_image: str
    hostname: str = "backupd"

    model_config: ClassVar[SettingsConfigDict] = SettingsConfigDict(
        env_prefix="BACKUPD_", enable_decoding=False
    )

    def __init__(self) -> None:  # To prevent warnings for Settings()
        super().__init__()


class Restic(BaseModel):
    repository: str
    password: str

    @property
    def backend(self) -> str:
        return self.repository.split(":")[0]

    @property
    def location(self) -> list[str]:
        return self.repository.split(":")[1:]

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


def env_serializer(prefix: str) -> Callable[[BaseModel], list[str]]:
    def serializer(model: BaseModel) -> list[str]:
        return [f"{prefix}{k.upper()}={v}" for k, v in model.model_dump().items()]

    return serializer


class ResticSettings(BaseSettings):
    restic: Annotated[Restic, PlainSerializer(env_serializer("RESTIC_"), list[str])]
    rclone: (
        Annotated[Rclone, PlainSerializer(env_serializer("RCLONE_"), list[str])] | None
    ) = None

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

    @model_serializer(mode="wrap")
    def serialize(
        self, serializer: Callable[[Self], dict[str, list[str]]]
    ) -> list[str]:
        data = serializer(self)
        return list(chain.from_iterable(data.values()))


print(ResticSettings().model_dump())
