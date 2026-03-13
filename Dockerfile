ARG EMACS_VERSION=29.4
FROM silex/emacs:${EMACS_VERSION}-eask

WORKDIR /app

COPY Eask Makefile ./
RUN eask install-deps

COPY . .

RUN eask recompile

CMD ["eask", "run", "command", "test"]
