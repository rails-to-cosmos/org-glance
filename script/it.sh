#!/usr/bin/env bash

docker run \
       --volume "$PWD":/org-glance \
       --workdir /org-glance flycheck/emacs-cask:27.1 \
       /bin/bash -c "make test"
