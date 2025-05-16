from itertools import chain
import logging
from typing import Literal
from pydantic import BaseModel, TypeAdapter, ValidationError


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


def parse_messages[T](message_type: type[T], stdout: str, stderr: str) -> list[T]:
    lines = chain(stdout.splitlines(), stderr.splitlines())
    adapter: TypeAdapter[T] = TypeAdapter(message_type)

    messages: list[T] = []
    for line in lines:
        try:
            messages.append(adapter.validate_json(line))
        except ValidationError as e:
            logging.warning(e, extra={"exception": "ValidationError", "input": line})

    return messages
