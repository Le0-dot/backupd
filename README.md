# Backupd

Backup all your docker containers with ease.


## Features

- [x] Backuping individual volume
- [x] Backuping volumes for container
- [x] Restoring individual volume from any snapshot
- [x] Restoring volumes for container from latest snapshots
- [x] Exporting metrics in [prometheus](https://prometheus.io) compatible format
- [x] Support for `rclone` restic repositories
- [ ] Structured logging in logfmt format
- [ ] Forgeting of old snapshots
- [ ] Support for all restic repositories


## Installation

Get the latest docker tag from release page, or use ghcr.io/le0-dot/backupd:latest
```sh
docker pull ghcr.io/le0-dot/backupd:<tag>
```

## Usage

### Configuration

Backupd is configured exclusively with environment variables.

- `BACKUPD_RUNNER_IMAGE` - image that will be runnning the backups, should include executables for `sh`, `restic` and optionally `rclone` if used.
- `BACKUPD_TIMEOUT_SECONDS` (Optional, default: `300`) - maximum time for backup to run, consider increasing for huge volumes or bad latency.
- `BACKUPD_ABORT_ON_FAILURE` (Optional, default: `true`) - wherever to fail immedeatly in bulk backup/restore jobs.
- `DOCKER_HOST` (Optional) - should be used if you do not wish to mount docker socket directly to the container, see [docker documentation](https://docs.docker.com/reference/cli/docker/#environment-variables)
- `RESTIC_REPOSITORY` - as per [restic documentation](https://restic.readthedocs.io/en/stable/040_backup.html#environment-variables)
- `RESTIC_PASSWORD` - as per [restic documentation](https://restic.readthedocs.io/en/stable/040_backup.html#environment-variables)
- `RESTIC_HOST` (Optional, default: `backupd`) - as per [restic documentation](https://restic.readthedocs.io/en/stable/040_backup.html#environment-variables)
- `RCLONE_*`, `RCLONE_CONFIG_*` (Optional) - as per [rclone documentation](https://rclone.org/docs/#environment-variables)


### Endpoints

> [!WARNING]
> Backupd schedules start of docker container on HTTP request, which could lead to security risks if exposed to the internet. Due to this, it is recommened to limit access to the backupd service.

Backupd exposes a number of HTTP endpoints:
- GET `/list/volume` - list all volumes
- GET `/list/volume/{name}` - check if volume exists
- GET `/list/container` - list all containers
- GET `/list/container/{name}` - check if container exists
- GET `/list/snapshot` - list all snapshots
- GET `/list/snapshot/volume/{name}` - list all snapshots for volume
- GET `/list/snapshot/container/{name}` - list all snapshots for container
- POST `/backup/volume` - backup all volumes
- POST `/backup/volume/{name}` - backup volume
- POST `/backup/container` - backup all volumes for all container running containers
- POST `/backup/container/{name}` - backup all volumes for container
- POST `/restore/volume/{name}` - restore volume from latest snapshot
- POST `/restore/volume/{name}/{snapshot}` - restore volume from snapshot
- POST `/restore/container/{name}` - restore all volumes for container from their latest snapshot
- GET `/metrics` - collect [prometheus](https://prometheus.io) metrics for monitoring and alerting
- GET `/docs` - view [Swagger UI](https://swagger.io/tools/swagger-ui)

## Examples

### Docker compose
```yaml file=compose.yaml
services:
  backupd:
    image: ghcr.io/le0-dot/backupd:latest
    container_name: backupd
    restart: unless-stopped
    ports:
      - 127.0.0.1:9988:9988 # Expose backupd only to localhost
    volumes:
      - /var/run/docker.sock:/var/run/docker.sock # Or set DOCKER_HOST
    environment:
      - BACKUPD_RUNNER_IMAGE=docker.io/instrumentisto/restic:0.18.0 # Includes restic and rclone
      # - DOCKER_HOST=...

      - RESTIC_REPOSITORY=...
      - RESTIC_PASSWORD=...

      # - RCLONE_CONFIG_..._TYPE=... # Set RCLONE_CONFIG_* to configure rclone repository
```

### Trigger backup with curl
```sh
curl -X POST http://localhost:9988/backup/container
```

### Systemd Service and Timer to trigger backups

#### backup-containers.service

```ini
[Unit]
Description=Trigger backup of docker volumes to repository in %I

[Service]
Type=oneshot
ExecStart=/usr/bin/curl -X POST http://localhost:9988/backup/container
```

#### backup-containers.timer

```ini
[Unit]
Description=Trigger backup of docker volumes every day at 5 AM to repository in %I

[Timer]
OnCalendar=*-*-* 05:00:00
Persistent=true
Unit=backup-containers.service

[Install]
WantedBy=default.target
```

#### Starting the timer
Put the repository configuration in repo.json wherever you want. Then run the following command to start and enable the service with escaped file path as parameter.
```sh
systemctl enable --now backup-containers.timer
```

To verify that timer works run
```sh
systemctl status backup-containers.timer
```
