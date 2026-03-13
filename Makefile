.PHONY: build test info clean lint docker-test docker-test-all

info:
	eask info

init:
	eask install-deps
	eask recompile
	eask install

build:
	eask install-deps
	eask recompile
	eask package ./dist
	eask reinstall

test:
	make clean
	eask recompile
	eask run command test

lint:
	make clean
	eask lint checkdoc

clean:
	eask clean elc

DOCKER ?= docker
DOCKER_COMPOSE ?= docker compose

docker-test:
	$(DOCKER) build --build-arg EMACS_VERSION=$(or $(EMACS_VERSION),29.4) -t org-glance:emacs-$(or $(EMACS_VERSION),29.4) .
	$(DOCKER) run --rm org-glance:emacs-$(or $(EMACS_VERSION),29.4)

docker-test-all:
	$(DOCKER_COMPOSE) up --build --abort-on-container-exit
