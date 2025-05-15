run-host:
	BACKUPD_RUNNER_IMAGE=docker.io/instrumentisto/restic:0.18.0 RESTIC_REPOSITORY=/home/le0/backupd/test-backup RESTIC_PASSWORD=123 uv run fastapi dev --port 9988 .
dummy-container:
	docker run -it --rm -v testvol:/data -v testvol2:/data2 busybox sh
build-docker:
	docker build . --tag backupd:dev
run-docker: build-docker
	docker run -v /var/run/docker.sock:/var/run/docker.sock -p 127.0.0.1:9988:9988 --env BACKUPD_RUNNER_IMAGE=docker.io/instrumentisto/restic:0.18.0 --env RESTIC_REPOSITORY=/home/le0/backupd/test-backup --env RESTIC_PASSWORD=123 backupd:dev
