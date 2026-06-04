# Reproducible Emacs environment for org-glance.
#
# Runs the ERT suite and interactive sessions on a PINNED Emacs version, so
# tests and interactive computations behave identically everywhere.
#
# Build (default Emacs 29.1; override e.g. --build-arg EMACS_VERSION=30.1):
#   podman build -t org-glance-test:emacs-29.1 -f Containerfile .
# Run the test suite:
#   podman run --rm org-glance-test:emacs-29.1
# Interactive Emacs (TUI) with org-glance available:
#   podman run --rm -it org-glance-test:emacs-29.1 eask emacs -nw
# One-off elisp on the pinned Emacs:
#   podman run --rm org-glance-test:emacs-29.1 \
#     eask eval "(progn (require 'org-glance) (princ \"ok\"))"
#
# See the Makefile `podman-*` targets for convenient wrappers.

ARG EMACS_VERSION=29.1
FROM docker.io/silex/emacs:${EMACS_VERSION}

# eask ships as an npm CLI. Installing it via npm avoids the eask apt repo and
# its unverified GPG key / --allow-insecure-repositories flags.
RUN apt-get update \
 && apt-get install -y --no-install-recommends nodejs npm ca-certificates git \
 && rm -rf /var/lib/apt/lists/* \
 && npm install -g @emacs-eask/cli \
 && npm cache clean --force

WORKDIR /work

# Prime the dependency layer from the Eask manifest alone, so editing sources
# does not re-download dependencies on every rebuild. (.eask/ is excluded via
# .containerignore, so host-compiled deps for a different Emacs never leak in.)
COPY Eask ./
RUN eask install-deps || true

# Project sources, then resolve deps + byte-compile for THIS image's Emacs.
COPY . .
RUN eask install-deps && eask recompile

# Default command: the configured ERT suite. Override the command for
# interactive use (see the header).
CMD ["eask", "run", "command", "test"]
