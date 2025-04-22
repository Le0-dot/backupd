from typing import ClassVar, Literal
from backupd.config import CustomBase, NonEmptyStr


class Repository(CustomBase):
    kind: ClassVar[Literal["rclone"]] = "rclone"
    path: NonEmptyStr
    password: NonEmptyStr
    config: str

    @property
    def env(self) -> dict[str, str]:
        return {
            "RESTIC_REPOSITORY": f"{self.kind}:{self.path}",
            "RESTIC_PASSWORD": self.password,
        }
