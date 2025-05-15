from typing import Literal
from pydantic import BaseModel


class Error(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#id5
    """

    class ErrorMessage(BaseModel):
        message: str

    message_type: Literal["error"]
    error: ErrorMessage
    during: str
    item: str


class ExitError(BaseModel):
    """
    https://restic.readthedocs.io/en/latest/075_scripting.html#exit-errors
    """

    message_type: Literal["exit_error"]
    code: int
    message: str
