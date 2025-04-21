import subprocess
from dataclasses import dataclass

import docker
import docker.types

from backupd.config.repo import Repo


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
    def __init__(self, repo: Repo, *args: str) -> None:
        self.args: list[str] = list(args)
        self.repository: str = repo.path
        self.password: str = repo.key

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


def check(repo: Repo) -> ResticRunConfiguration:
    return ResticRunConfiguration(repo, "check")


def backup(repo: Repo, path: str, tag: str) -> ResticRunConfiguration:
    return ResticRunConfiguration(repo, "backup", path, "--tag", tag)


def init(repo: Repo) -> ResticRunConfiguration:
    return ResticRunConfiguration(repo, "init")


def unlock(repo: Repo) -> ResticRunConfiguration:
    return ResticRunConfiguration(repo, "unlock")


def snapshots(repo: Repo, *args: str) -> ResticRunConfiguration:
    return ResticRunConfiguration(repo, "snapshots", *args)
