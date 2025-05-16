from prometheus_client import Counter, Histogram

api_calls = Counter(
    name="api_calls",
    documentation="Number of api calls completed successfully",
    labelnames=("path", "method", "status"),
    namespace="backupd",
)
backup_result = Counter(
    name="backup_result",
    documentation="Number of finished backups",
    labelnames=("volume", "status"),
    namespace="backupd",
)
backup_duration = Histogram(
    name="backup_duration",
    documentation="Duraction of successful backup",
    labelnames=("volume",),
    namespace="backupd",
)
restore_result = Counter(
    name="restore_result",
    documentation="Number of finished restorations",
    labelnames=("volume", "status"),
    namespace="backupd",
)
