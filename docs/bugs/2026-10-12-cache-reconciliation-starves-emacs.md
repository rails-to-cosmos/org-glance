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

- The original implementation used `make-thread`, so reconciliation still ran
  in Emacs's cooperative interpreter scheduler.
- Its `thread-yield` calls covered only selected projection boundaries.

## Required behavior

Large reconciliation must not execute on the interactive Emacs Lisp scheduler.
Run it in an isolated process and publish a completed cache generation, while
Main and point reads continue from the last committed projection.

## Resolution

Reconciliation now runs in an isolated batch Emacs process and full recovery
publishes a separately validated generation. Graph reads never survey the
external relay or request reconciliation. A bounded coordinator poll wakes the
isolated worker, which drains the relay. Recovery has no cooperative-yield
branch. The contracts are pinned by
`headline-read-never-enters-reconciliation`,
`cache-refresh-never-yields-on-the-caller-thread`, and
`cache-refresh-worker-rebuilds-in-separate-emacs`.
