.PHONY: build test info clean lint

info:
	eask info

init:
	eask install-deps
	eask recompile
	eask install
	eask test activate

build:
	eask install-deps
	eask recompile
	eask package ./dist
	eask reinstall

test:
	make clean
	eask recompile
	eask package ./dist
	eask reinstall
	eask run command test

lint:
	make clean
	eask lint checkdoc

clean:
	eask clean elc

# --- Podman: reproducible Emacs on a pinned version --------------------------
# Override the Emacs version with: make podman-test EMACS_VERSION=30.1
PODMAN ?= podman
EMACS_VERSION ?= 29.1
IMAGE ?= org-glance-test:emacs-$(EMACS_VERSION)

.PHONY: podman-build podman-test podman-emacs podman-shell podman-clean

podman-build:
	$(PODMAN) build --build-arg EMACS_VERSION=$(EMACS_VERSION) -t $(IMAGE) -f Containerfile .

# Run the full ERT suite on the pinned Emacs.
podman-test: podman-build
	$(PODMAN) run --rm $(IMAGE)

# Interactive Emacs (TUI) on the pinned version with org-glance loaded; data
# under ./.podman-data persists across runs as `org-glance-directory'.
podman-emacs: podman-build
	mkdir -p .podman-data
	$(PODMAN) run --rm -it -v "$(CURDIR)/.podman-data:/root/org:Z" $(IMAGE) \
	  eask emacs -nw --eval "(progn (require 'org-glance) (setq org-glance-directory \"/root/org\"))"

# Shell inside the container (sources + deps baked in).
podman-shell: podman-build
	$(PODMAN) run --rm -it $(IMAGE) /bin/bash

podman-clean:
	-$(PODMAN) rmi $(IMAGE)
