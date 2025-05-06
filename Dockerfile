FROM python:3.13-alpine3.21

COPY . /backupd

RUN pip install /backupd

CMD ["fastapi", "run", "--port", "9988", "/backupd"]
