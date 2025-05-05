FROM python:3.13-alpine3.21

COPY . /app

RUN pip install /app

CMD ["fastapi", "run", "--port", "9988", "/app/src/backupd/__init__.py"]
