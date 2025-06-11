#!/bin/bash
set -euo pipefail

# setup-dev.sh -- prepare offline development environment for the Mjolnir project
# This script installs required tools and caches pre-commit hooks so that
# development can continue without internet access after setup.
# It is idempotent and safe to run multiple times.

if [[ "$(id -u)" -ne 0 ]]; then
  echo "Please run as root" >&2
  exit 1
fi

OS=$(uname -s)
if [[ "$OS" != "Linux" ]]; then
  echo "Unsupported OS: $OS" >&2
  exit 1
fi

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y --no-install-recommends \
    emacs-nox git python3 python3-pip

# Install pre-commit if not present
if ! command -v pre-commit >/dev/null 2>&1; then
  pip install --break-system-packages --no-cache-dir pre-commit
fi

# Install and cache pre-commit hooks
pre-commit install --install-hooks --config .pre-commit-config.yaml

# Byte compile to verify the package builds
emacs --batch -Q -L . -f batch-byte-compile mjolnir.el

cat <<'MSG'
Development environment setup complete.
Run 'pre-commit run --all-files' to verify everything is ready.
MSG
