FROM python:3.13-alpine3.21

COPY . /backupd

RUN pip install /backupd

CMD ["uvicorn", "backupd:app", "--host", "0.0.0.0", "--port", "9988", "--log-config", "/backupd/log-config.json", "--no-access-log"]
