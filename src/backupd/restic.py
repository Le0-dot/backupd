import subprocess

from docker import from_env
from docker.types import Mount
from pydantic import BaseModel

from backupd.config.repo import Repo


class ProcessResult(BaseModel):
    exit_code: int
    stdout: str
    stderr: str

    @property
    def success(self) -> bool:
        return not self.exit_code

    @property
    def failure(self) -> bool:
        return bool(self.exit_code)


def run_restic(
    repo: Repo, *args: str, binary: str = "/usr/bin/restic"
) -> ProcessResult:
    result = subprocess.run(
        [binary, *args],
        env=repo.env,
        capture_output=True,
    )
    return ProcessResult(
        exit_code=result.returncode,
        stdout=result.stdout.decode(),
        stderr=result.stderr.decode(),
    )


def check(repo: Repo) -> ProcessResult:
    return run_restic(repo, "check")


def backup(repo: Repo, path: str, tag: str) -> ProcessResult:
    return run_restic(repo, "backup", path, "--tag", tag)


def init(repo: Repo) -> ProcessResult:
    return run_restic(repo, "init")


def unlock(repo: Repo) -> ProcessResult:
    return run_restic(repo, "unlock")


def snapshots(repo: Repo, *args: str) -> ProcessResult:
    return run_restic(repo, "snapshots", *args)


def run_docker_backup(
    repo: Repo,
    volume: str,
    tag: str,
    image: str = "docker.io/instrumentisto/restic",
    image_tag: str = "0.18",
) -> ProcessResult:
    client = from_env()

    mounts = [Mount("/data", volume, type="volume", read_only=True)]
    if repo.kind == "local":
        mounts.append(Mount(repo.path, repo.path, type="bind"))

    container = client.containers.run(
        f"{image}:{image_tag}",
        ["backup", "/data", "--tag", tag],
        environment=repo.env,
        detach=True,
        mounts=mounts,
    )
    response = container.wait()

    return ProcessResult(
        exit_code=response["StatusCode"],
        stdout=container.logs(stdout=True, stderr=False, stream=False).decode(),
        stderr=container.logs(stdout=False, stderr=True, stream=False).decode(),
    )
