from collections.abc import Awaitable
from typing import Callable, override

from fastapi import Request, Response
from prometheus_client import Counter, Gauge, Histogram
from starlette.middleware.base import BaseHTTPMiddleware

api_calls = Counter(
    name="api_calls",
    documentation="Number of api calls completed successfully",
    labelnames=("path", "method", "status"),
    namespace="backupd",
)
backup_start = Gauge(
    name="backup_last_start_timestamp",
    documentation="Timestamp of the last started backup",
    labelnames=("container", "volume"),
    unit="seconds",
    namespace="backupd",
)
backup_result = Counter(
    name="backup_result",
    documentation="Total number of completed backups",
    labelnames=("container", "volume", "status"),
    namespace="backupd",
)
backup_duration = Histogram(
    name="backup",
    documentation="Time it took to run the backup",
    labelnames=("container", "volume"),
    unit="seconds",
    namespace="backupd",
)
job_state = Gauge(
    name="job_state",
    documentation="Number of job in the queue scheduled to run",
    labelnames=("kind", "state"),
    namespace="backupd",
)


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
