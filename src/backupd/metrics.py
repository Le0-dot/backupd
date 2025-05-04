from collections.abc import Awaitable
from contextlib import asynccontextmanager
from typing import Annotated, Callable, TypedDict, override

from fastapi import Depends, FastAPI, Request, Response
from prometheus_client import Counter, Gauge, Summary, make_asgi_app
from starlette.middleware.base import BaseHTTPMiddleware

from backupd.docker import ContainerRunResult


class Metrics(TypedDict):
    api_calls: Counter

    backup_success: Counter
    backup_failure: Counter
    backup_time: Summary

    job_scheduled: Gauge


@asynccontextmanager
async def metrics_lifespan(app: FastAPI):
    app.mount("/metrics", make_asgi_app())
    app.state.metrics = Metrics(
        api_calls=Counter(
            name="calls",
            documentation="Number of api calls completed successfully",
            labelnames=("path", "method", "status"),
            namespace="backupd",
            subsystem="api",
        ),
        backup_success=Counter(
            name="success",
            documentation="Number of backups completed successfully",
            labelnames=("container", "volume"),
            namespace="backupd",
            subsystem="backup",
        ),
        backup_failure=Counter(
            name="failure",
            documentation="Number of backups resulted in failure",
            labelnames=("container", "volume"),
            namespace="backupd",
            subsystem="backup",
        ),
        backup_time=Summary(
            name="time",
            documentation="Time it took to run the backup",
            labelnames=("container", "volume"),
            unit="seconds",
            namespace="backupd",
            subsystem="backup",
        ),
        job_scheduled=Gauge(
            name="scheduled",
            documentation="Number of job in the queue scheduled to run",
            labelnames=("job_type", "container", "volume"),
            namespace="backupd",
            subsystem="job",
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
    **labels: str,
) -> Callable[P, Awaitable[None]]:
    success = metrics["backup_success"].labels(*labels)
    failure = metrics["backup_failure"].labels(*labels)
    time = metrics["backup_time"].labels(*labels)
    scheduled = metrics["job_scheduled"].labels("backup", *labels)

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


class APIMetricsMiddleware(BaseHTTPMiddleware):
    @override
    async def dispatch(
        self, request: Request, call_next: Callable[[Request], Awaitable[Response]]
    ) -> Response:
        response = await call_next(request)

        metric = get_metrics(request)["api_calls"]
        metric.labels(
            path=request.url.path, method=request.method, status=response.status_code
        ).inc()

        return response
