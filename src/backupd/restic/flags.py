from collections.abc import Set
from dataclasses import dataclass, field
from typing import Literal, Self, override


class Tag(str):
    @classmethod
    def for_app(cls) -> Self:
        return cls("backupd")

    @classmethod
    def for_volume(cls, name: str) -> Self:
        return cls(f"volume:{name}")

    @property
    def volume(self) -> str | None:
        if self.startswith("volume:"):
            return self.removeprefix("volume:")
        return None


class TagFlag(set[Tag]):
    """
    Represents individual `--tag` flag for the restic
    """

    @override
    def __or__(self, value: Set[Tag], /) -> "TagFlag":
        return TagFlag(super().__or__(value))

    @override
    def __str__(self) -> str:
        if self:
            return f"--tag {','.join(self)}"
        return ""

    @classmethod
    def for_app(cls) -> Self:
        return cls({Tag.for_app()})

    @classmethod
    def for_volume(cls, name: str) -> Self:
        return cls({Tag.for_volume(name)})


@dataclass
class Group:
    """
    Represents `--group-by` flag for the restic
    """

    groups: set[Literal["host", "paths", "tags"]] = field(default_factory=set)

    @override
    def __str__(self) -> str:
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
