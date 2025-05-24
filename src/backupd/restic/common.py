from itertools import chain
import logging
from pydantic import TypeAdapter, ValidationError


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
