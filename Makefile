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

# --- Fat build: one loadable file --------------------------------------------
# Concatenate the multi-directory sources into a single `dist/org-glance.el' in
# load-history order (dependencies precede dependents, so macros are defined
# before use), then prove it byte-compiles and loads with only itself + its
# dependencies on `load-path' (sources removed).  Ship dist/org-glance.el(c).

# --- Version bumping ---------------------------------------------------------
# org-glance versions are MAJOR.MINOR.PATCH.BUILD.YYYYMMDD.REV (package-build /
# ELPA style, e.g. 0.1.0.0.20260709.0).  `make major|minor|patch|build' bumps
# that base component, resets the lower base components, stamps today's date and
# resets REV; `make rev' bumps REV for another release the same day.  The version
# is written to both the Eask spec and the org-glance.el header.
.PHONY: major minor patch build rev bump-version
major: BUMP := major
minor: BUMP := minor
patch: BUMP := patch
build: BUMP := build
rev:   BUMP := rev
major minor patch build rev: bump-version

bump-version:
	@cur=`sed -n 's/^(package "org-glance" "\([0-9.]*\)".*/\1/p' Eask`; \
	test -n "$$cur" || { echo "error: could not read version from Eask"; exit 1; }; \
	set -- `echo "$$cur" | tr '.' ' '`; \
	maj=$${1:-0}; min=$${2:-0}; pat=$${3:-0}; bld=$${4:-0}; olddate=$${5:-0}; rev=$${6:-0}; \
	today=`date +%Y%m%d`; \
	case "$(BUMP)" in \
	  major) maj=$$((maj+1)); min=0; pat=0; bld=0; rev=0 ;; \
	  minor) min=$$((min+1)); pat=0; bld=0; rev=0 ;; \
	  patch) pat=$$((pat+1)); bld=0; rev=0 ;; \
	  build) bld=$$((bld+1)); rev=0 ;; \
	  rev)   if [ "$$olddate" = "$$today" ]; then rev=$$((rev+1)); else rev=0; fi ;; \
	  *) echo "usage: make major|minor|patch|build|rev"; exit 1 ;; \
	esac; \
	new="$$maj.$$min.$$pat.$$bld.$$today.$$rev"; \
	sed -i "s/^(package \"org-glance\" \"$$cur\"/(package \"org-glance\" \"$$new\"/" Eask; \
	sed -i "s/^;; Version: [0-9][0-9.]*/;; Version: $$new/" org-glance.el; \
	echo "org-glance: $$cur -> $$new"

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
