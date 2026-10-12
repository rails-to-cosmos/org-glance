# Cache reconciliation starves interactive Emacs

## Symptom

Main remains effectively unusable while a large immutable-family cache rebuild
runs. Startup returns, but commands and redisplay stall until reconciliation
finishes.

## Reproduction

1. Open a store whose committed SQLite proof is stale and which contains about
   6,787 headline families.
2. Start Emacs with org-glance enabled and open Main.
3. Observe rebuild progress reach the end of the family census.
4. Try ordinary Emacs input while reconciliation continues.

The UI stalls even though reconciliation was launched by
`org-glance-cache:refresh-async`.

## Evidence

- `src/data/org-glance-cache.el:905` uses `make-thread`, so reconciliation still
  executes Lisp in Emacs's cooperative interpreter scheduler.
- `src/data/org-glance-cache.el:675` only calls `thread-yield` at explicit
  boundaries. Filesystem, JSON, SQLite, and projection work between those
  boundaries continues to consume the interactive process.
- `src/data/org-glance-cache.el:910` runs the complete reconciliation inside
  that Emacs thread.

## Required behavior

Large reconciliation must not execute on the interactive Emacs Lisp scheduler.
Run it in an isolated process and publish a completed cache generation, while
Main and point reads continue from the last committed projection.
