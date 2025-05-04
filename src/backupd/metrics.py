from collections.abc import Awaitable
from contextlib import asynccontextmanager
from typing import Callable, override

from fastapi import FastAPI, Request, Response
from prometheus_client import Counter, Gauge, Summary, make_asgi_app
from starlette.middleware.base import BaseHTTPMiddleware


api_calls = Counter(
    name="calls",
    documentation="Number of api calls completed successfully",
    labelnames=("path", "method", "status"),
    namespace="backupd",
    subsystem="api",
)
backup_success = Counter(
    name="success",
    documentation="Number of backups completed successfully",
    labelnames=("container", "volume"),
    namespace="backupd",
    subsystem="backup",
)
backup_failure = Counter(
    name="failure",
    documentation="Number of backups resulted in failure",
    labelnames=("container", "volume"),
    namespace="backupd",
    subsystem="backup",
)
backup_time = Summary(
    name="time",
    documentation="Time it took to run the backup",
    labelnames=("container", "volume"),
    unit="seconds",
    namespace="backupd",
    subsystem="backup",
)
jobs = Gauge(
    name="scheduled",
    documentation="Number of job in the queue scheduled to run",
    labelnames=("kind", "creation_time", "container", "volume"),
    namespace="backupd",
    subsystem="job",
)


@asynccontextmanager
async def metrics_lifespan(app: FastAPI):
    app.mount("/metrics", make_asgi_app())
    yield


class APIMetricsMiddleware(BaseHTTPMiddleware):
    @override
    async def dispatch(
        self, request: Request, call_next: Callable[[Request], Awaitable[Response]]
    ) -> Response:
        response = await call_next(request)

        api_calls.labels(
            path=request.url.path, method=request.method, status=response.status_code
        ).inc()

        return response
