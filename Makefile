restic_env = RESTIC_REPOSITORY=$(shell pwd)/test-backup RESTIC_PASSWORD=123 RESTIC_HOST=backupd

repo:
	$(restic_env) restic init
snapshots:
	$(restic_env) restic --json snapshots --group-by tags
backup:
	$(restic_env) restic backup --tag backupd ./src/
host:
	$(restic_env) BACKUPD_RUNNER_IMAGE=docker.io/instrumentisto/restic:0.18.0 uv run uvicorn --port 9988 --log-config log-config.json --no-access-log --reload backupd:app
dummy-container:
	docker run -it --rm -v testvol:/data -v testvol2:/data2 busybox sh
build-image:
	docker build . --tag backupd:dev
container: build-docker
	docker run -v /var/run/docker.sock:/var/run/docker.sock -p 127.0.0.1:9988:9988 --env BACKUPD_RUNNER_IMAGE=docker.io/instrumentisto/restic:0.18.0 --env RESTIC_REPOSITORY=/home/le0/backupd/test-backup --env RESTIC_PASSWORD=123 backupd:dev
