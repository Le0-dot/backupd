from typing import Annotated

from annotated_types import MinLen
from pydantic import (
    BaseModel,
    ConfigDict,
)


class CustomBase(BaseModel):
    model_config = ConfigDict(extra="forbid", frozen=True)


type NonEmptyStr = Annotated[str, MinLen(1)]
