from typing import ClassVar
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    runner_image: str
    hostname: str = "backupd"

    model_config: ClassVar[SettingsConfigDict] = SettingsConfigDict(
        env_prefix="BACKUPD_", enable_decoding=False
    )

    def __init__(self) -> None:  # To prevent warnings for Settings()
        super().__init__()
