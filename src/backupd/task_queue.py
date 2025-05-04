from asyncio import CancelledError, Queue, Task, create_task
from collections.abc import Awaitable, Callable
from contextlib import asynccontextmanager
from typing import Annotated

from fastapi import Depends, FastAPI, Request

type Job = Callable[[], Awaitable[None]]


class TaskQueue:
    def __init__(self) -> None:
        self._queue: Queue[Job] = Queue()
        self._task: Task[None] | None = None

    def start(self) -> None:
        self._task = create_task(self._worker())

    async def put(self, *jobs: Job) -> None:
        for job in jobs:
            await self._queue.put(job)

    async def shutdown(self) -> None:
        if self._task is None:
            return

        _ = self._task.cancel()
        try:
            await self._task
        except CancelledError:
            pass
        finally:
            self._task = None

    async def _worker(self) -> None:
        while True:
            job = await self._queue.get()
            try:
                await job()
            except Exception as e:
                print(e)
            finally:
                self._queue.task_done()


@asynccontextmanager
async def queue_lifespan(app: FastAPI):
    app.state.queue = TaskQueue()
    app.state.queue.start()

    yield

    await app.state.queue.shutdown()


def get_task_queue(request: Request) -> TaskQueue:
    if not isinstance(app := request.app, FastAPI):
        raise TypeError("cannot retrieve task queue from non FastAPI application")

    try:
        queue = app.state.queue
    except KeyError:
        raise TypeError("queue does not exist in application")

    if not isinstance(queue, TaskQueue):
        raise TypeError("queue is not an instance of TaskQueue")

    return queue


AppQueue = Annotated[TaskQueue, Depends(get_task_queue)]
