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
        return self.exit_code == 0


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
    result = container.wait()

    return ProcessResult(
        exit_code=result["StatusCode"],
        stdout=container.logs(stdout=True, stderr=False, stream=False).decode(),
        stderr=container.logs(stdout=False, stderr=True, stream=False).decode(),
    )
