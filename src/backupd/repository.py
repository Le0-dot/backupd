from typing import ClassVar, Literal, override

from pydantic import BaseModel, ConfigDict, Field, model_validator


class RepositoryBase(BaseModel):
    model_config: ClassVar[ConfigDict] = ConfigDict(extra="forbid", frozen=True)

    path: str
    password: str

    @property
    def env(self) -> dict[str, str]:
        return {
            "RESTIC_REPOSITORY": self.path,
            "RESTIC_PASSWORD": self.password,
        }

    @property
    def preexec(self) -> str:
        return ":"  # No op


class LocalRespository(RepositoryBase):
    kind: Literal["local"]


class RcloneRepository(RepositoryBase):
    kind: Literal["rclone"]
    config: str

    @property
    @override
    def preexec(self) -> str:
        return f"echo '{self.config}' > ${{HOME}}/.config/rclone/rclone.conf"


Repository = Annotated[LocalRespository | RcloneRepository, Field(discriminator="kind")]
