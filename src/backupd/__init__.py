import logging
from collections.abc import Awaitable, Callable
from contextlib import asynccontextmanager
from typing import override

from fastapi import FastAPI, Request, Response
from prometheus_client import make_asgi_app
from pydantic import ValidationError
from starlette.middleware.base import BaseHTTPMiddleware

from backupd.api.backup import router as backup_router
from backupd.api.list import router as list_router
from backupd.api.restore import router as restore_router
from backupd.api.remove import router as remove_router
from backupd.metrics import api_calls
from backupd.settings import RepositorySettings, Settings


@asynccontextmanager
async def lifespan(app: FastAPI):
    logging.info("application started")

    try:
        settings = Settings()
        logging.debug(settings)
        repository = RepositorySettings()
        logging.debug(repository)
    except ValidationError as e:
        logging.error("incorrect settings, see documentation")
        logging.debug(e.errors(), extra={"exception": "ValidationError"})
        return

    app.mount("/metrics", make_asgi_app())
    logging.debug("mounted '/metrics'")

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


class LoggingMiddleware(BaseHTTPMiddleware):
    @override
    async def dispatch(
        self, request: Request, call_next: Callable[[Request], Awaitable[Response]]
    ) -> Response:
        logging.debug(
            "received request",
            extra={"path": request.url.path, "method": request.method},
        )

        try:
            response = await call_next(request)
        except Exception as e:
            logging.error(
                "unhandled exception during request",
                extra={"path": request.url.path, "method": request.method},
            )
            logging.debug(e, extra={"path": request.url.path, "method": request.method})
            raise Exception from e

        logging.info(
            "handled request",
            extra={
                "path": request.url.path,
                "method": request.method,
                "status": response.status_code,
            },
        )

        return response


app = FastAPI(lifespan=lifespan)

app.include_router(list_router)
app.include_router(backup_router)
app.include_router(restore_router)
app.include_router(remove_router)

app.add_middleware(APIMetricsMiddleware)
app.add_middleware(LoggingMiddleware)
