import subprocess
from collections.abc import Sequence
from dataclasses import dataclass

import docker
import docker.types

from backupd.config import Storage


@dataclass
class ProcessResult:
    exit_code: int
    stdout: str
    stderr: str

    @property
    def success(self) -> bool:
        return not self.exit_code

    @property
    def failure(self) -> bool:
        return bool(self.exit_code)


class ResticRunConfiguration:
    def __init__(self, storage: Storage, *args: str) -> None:
        self.args: list[str] = list(args)
        self.repository: str = storage.path
        self.password: str = storage.secret

    def run(self, restic: str = "/usr/bin/restic") -> ProcessResult:
        result = subprocess.run(
            [restic, *self.args],
            env={
                "RESTIC_REPOSITORY": self.repository,
                "RESTIC_PASSWORD": self.password,
            },
            capture_output=True,
        )
        return ProcessResult(
            exit_code=result.returncode,
            stdout=result.stdout.decode(),
            stderr=result.stderr.decode(),
        )

    def run_docker(
        self,
        *mounts: docker.types.Mount,
        image: str = "docker.io/instrumentisto/restic",
        tag: str = "0.18",
    ) -> ProcessResult:
        client = docker.from_env()

        container = client.containers.run(
            f"{image}:{tag}",
            self.args,
            environment={
                "RESTIC_REPOSITORY": "/repo",
                "RESTIC_PASSWORD": self.password,
            },
            detach=True,
            mounts=[docker.types.Mount("/repo", self.repository, type="bind"), *mounts],
        )
        response = container.wait()

        return ProcessResult(
            exit_code=response["StatusCode"],
            stdout=container.logs(stdout=True, stderr=False, stream=False).decode(),
            stderr=container.logs(stdout=False, stderr=True, stream=False).decode(),
        )


def check(storage: Storage) -> ResticRunConfiguration:
    return ResticRunConfiguration(storage, "check")


def backup(storage: Storage, path: str, tag: str) -> ResticRunConfiguration:
    return ResticRunConfiguration(storage, "backup", path, "--tag", tag)


def init(storage: Storage) -> ResticRunConfiguration:
    return ResticRunConfiguration(storage, "init")


def unlock(storage: Storage) -> ResticRunConfiguration:
    return ResticRunConfiguration(storage, "unlock")


def snapshots(storage: Storage, *args: str) -> ResticRunConfiguration:
    return ResticRunConfiguration(storage, "snapshots", *args)
