from collections.abc import Awaitable
from contextlib import asynccontextmanager
from typing import Annotated, Callable, TypedDict

from fastapi import Depends, FastAPI, Request
from prometheus_client import Counter, Gauge, Summary, make_asgi_app

from backupd.docker import ContainerRunResult


class Metrics(TypedDict):
    success: Counter
    failure: Counter
    time: Summary
    scheduled: Gauge


@asynccontextmanager
async def metrics_lifespan(app: FastAPI):
    app.mount("/metrics", make_asgi_app())
    app.state.metrics = Metrics(
        success=Counter(
            "success",
            "Number of backups completed successfully",
            ("container", "volume"),
            namespace="backupd",
        ),
        failure=Counter(
            "failure",
            "Number of backups resulted in failure",
            ("container", "volume"),
            namespace="backupd",
        ),
        time=Summary(
            "backup",
            "Time it took to run the backup",
            ("container", "volume"),
            unit="seconds",
            namespace="backupd",
        ),
        scheduled=Gauge(
            "scheduled",
            "Number of job in the queue scheduled to run",
            ("job_type", "container", "volume"),
            namespace="backupd",
        ),
    )

    yield


def get_metrics(request: Request) -> Metrics:
    if not isinstance(app := request.app, FastAPI):
        raise TypeError("cannot retrieve metrics from non FastAPI application")

    try:
        metrics = app.state.metrics
    except KeyError:
        raise TypeError("metrics do not exist in application")

    return metrics


AppMetrics = Annotated[Metrics, Depends(get_metrics)]


def instrument_backup[**P](
    metrics: Metrics,
    callee: Callable[P, Awaitable[ContainerRunResult]],
    *labels: str,
) -> Callable[P, Awaitable[None]]:
    success = metrics["success"].labels(*labels)
    failure = metrics["failure"].labels(*labels)
    time = metrics["time"].labels(*labels)
    scheduled = metrics["scheduled"].labels("backup", *labels)

    async def wrapper(*args: P.args, **kwargs: P.kwargs) -> None:
        with time.time():
            result = await callee(*args, **kwargs)
        scheduled.dec()

        counter = success if result.success else failure
        counter.inc()

        # TODO: Add proper logging
        print(result.stdout)
        print(result.stderr)

    scheduled.inc()
    return wrapper
