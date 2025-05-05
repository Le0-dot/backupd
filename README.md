# Backupd

Backup all your docker containers with ease.

## Installation

Get the latest docker tag from release page, or use ghcr.io/le0-dot/backupd:latest
```sh
docker pull ghcr.io/le0-dot/backupd:<tag>
```

## Usage

> [!NOTE]
> Backupd requires mounting docker socket or providing DOCKER_HOST environment variable

> [!WARNING]
> Backupd schedules start of docker container on HTTP request, which could lead to security risks if exposed to the internet. Due to this, it is recommened to limit access to the backupd service.

Backupd exposes couple of HTTP endpoints:
- GET `/containers` to list all container available to the backupd
- GET `/container/{name}` to see name and volumes of the specific container
- POST `/backup` to schedule backup of volumes for all **running** docker containers
- POST `/backup/{name}` to schedule backup of volumes for the specific **running** docker container
- GET `/metrics` to collect [prometheus](https://prometheus.io) metrics for monitoring and alerting
- GET `/docs` to view [Swagger UI](https://swagger.io/tools/swagger-ui)

All `/backup` endpoints additionally recieve POST body with information about restic repository (As of right now, only local and rclone repositories are supported). The exact format of required HTTP body could be seen at `/docs` endpoint.

## Examples

### Docker compose
```yaml file=compose.yaml
services:
  backupd:
    image: ghcr.io/le0-dot/backupd:latest
    container_name: backupd
    restart: unless-stopped
    ports:
      - 127.0.0.1:9988:9988
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock # Or set DOCKER_HOST
    environment:
      - BACKUPD_RUNNER_IMAGE=docker.io/instrumentisto/restic:0.18.0
      # - BACKUPD_HOSTNAME=... # Optional (default: backupd)
      # - DOCKER_HOST=...
```

### Trigger backup with curl
```sh
curl -X POST http://localhost:9988/backup --json '{"kind": "local", "location": "local:/location/of/restic/repository", "password": "password-to-restic-repository"}'
```

### Systemd Service and Timer to trigger backups
```ini file=backup@.service
[Unit]
Description=Trigger backup of docker volumes to repository in %I

[Service]
Type=oneshot
ExecStart=/usr/bin/curl -X POST http://localhost:9988/backup --json '@%I'
```

```ini file=backup@.timer
[Unit]
Description=Trigger backup of docker volumes every day at 5 AM to repository in %I

[Timer]
OnCalendar=*-*-* 05:00:00
Persistent=true
Unit=backup@%i.service

[Install]
WantedBy=default.target
```

Put the repository configuration in repo.json wherever you want. Then run
```sh
systemd-escape /full/path/to/repo.json --template backup@.timer
```

This will return the name of the timer you need to enable and start.

```sh
systemdctl enable --now backup@-fill-path-to-repo.json.timer
```
