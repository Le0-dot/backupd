from collections.abc import Awaitable
from typing import Callable, override

from fastapi import Request, Response
from prometheus_client import Counter, Histogram
from starlette.middleware.base import BaseHTTPMiddleware

api_calls = Counter(
    name="api_calls",
    documentation="Number of api calls completed successfully",
    labelnames=("path", "method", "status"),
    namespace="backupd",
)
backup_result = Counter(
    name="backup_result",
    documentation="Number of finished backups",
    labelnames=("volume", "status"),
    namespace="backupd",
)
backup_duration = Histogram(
    name="backup_duration",
    documentation="Duraction of successful backup",
    labelnames=("volume"),
    namespace="backupd",
)
restore_result = Counter(
    name="restore_result",
    documentation="Number of finished restorations",
    labelnames=("volume", "status"),
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
