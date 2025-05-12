from collections.abc import Awaitable
from typing import Callable, override

from fastapi import Request, Response
from prometheus_client import Counter, Gauge
from starlette.middleware.base import BaseHTTPMiddleware

api_calls = Counter(
    name="api_calls",
    documentation="Number of api calls completed successfully",
    labelnames=("path", "method", "status"),
    namespace="backupd",
)
job_state = Gauge(
    name="job_state",
    documentation="Number of jobs in a paticular state",
    labelnames=("kind", "state"),
    namespace="backupd",
)
job_result = Gauge(
    name="job_result",
    documentation="Number of jobs that finished in a paticular outcome",
    labelnames=("kind", "status", "volume"),
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
