from dataclasses import dataclass, field
from typing import Literal, Self


@dataclass
class Tag:
    """
    Represents individual `--tag` flag for the restic
    """

    tags: set[str] = field(default_factory=set)

    def __or__(self, value: Self, /) -> "Tag":
        return Tag(self.tags | value.tags)

    @property
    def flag(self) -> str:
        if self.tags:
            return f"--tag {','.join(self.tags)}"
        return ""

    @classmethod
    def app(cls) -> Self:
        return cls({"backupd"})

    @classmethod
    def volume(cls, name: str) -> Self:
        return cls({f"volume:{name}"})


@dataclass
class Group:
    """
    Represents `--group-by` flag for the restic
    """

    groups: set[Literal["host", "paths", "tags"]] = field(default_factory=set)

    @property
    def flag(self) -> str:
        if self.groups:
            return f"--group-by {','.join(self.groups)}"
        return ""

    @classmethod
    def host(cls) -> Self:
        return cls({"host"})

    @classmethod
    def paths(cls) -> Self:
        return cls({"paths"})

    @classmethod
    def tags(cls) -> Self:
        return cls({"tags"})
