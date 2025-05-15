import logging
from contextlib import asynccontextmanager

from fastapi import FastAPI
from logfmter import Logfmter
from prometheus_client import make_asgi_app

from backupd.api.list import router as list_router
from backupd.api.backup import router as backup_router
from backupd.api.restore import router as restore_router
from backupd.metrics import APIMetricsMiddleware
from backupd.settings import RepositorySettings, Settings


@asynccontextmanager
async def lifespan(app: FastAPI):
    # TODO: disable uvicorn logs and add custom middleware for logging
    # logger = logging.getLogger("uvicorn")
    # logger.handlers[0].setFormatter(Logfmter())
    # logger = logging.getLogger("uvicorn.access")
    # logger.handlers[0].setFormatter(Logfmter())

    _ = Settings()  # validate settings
    _ = RepositorySettings()  # validate restic

    app.mount("/metrics", make_asgi_app())

    yield


app = FastAPI(lifespan=lifespan)
app.include_router(list_router)
app.include_router(backup_router)
app.include_router(restore_router)
app.add_middleware(APIMetricsMiddleware)
