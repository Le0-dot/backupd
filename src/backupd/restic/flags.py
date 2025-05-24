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


@dataclass
class TagFlag:
    """
    Represents individual `--tag` flag for the restic
    """

    tags: set[Tag] = field(default_factory=set)

    def __or__(self, value: Self, /) -> Self:
        return self.__class__(self.tags | value.tags)

    @override
    def __str__(self) -> str:
        if self:
            return f"--tag={','.join(self.tags)}"
        return ""

    def __bool__(self) -> bool:
        return bool(self.tags)

    @classmethod
    def for_app(cls) -> Self:
        return cls({Tag.for_app()})

    @classmethod
    def for_volume(cls, name: str) -> Self:
        return cls({Tag.for_volume(name)})


@dataclass
class GroupFlag:
    """
    Represents `--group-by` flag for the restic
    """

    groups: set[Literal["host", "paths", "tags"]] = field(default_factory=set)

    @override
    def __str__(self) -> str:
        if self.groups:
            return f"--group-by={','.join(self.groups)}"
        return ""

    def __bool__(self) -> bool:
        return bool(self.groups)

    @classmethod
    def host(cls) -> Self:
        return cls({"host"})

    @classmethod
    def paths(cls) -> Self:
        return cls({"paths"})

    @classmethod
    def tags(cls) -> Self:
        return cls({"tags"})
