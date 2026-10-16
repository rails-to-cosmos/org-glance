# A stale proof skips a missing family projection

**Status:** fixed

**Date:** 2026-10-16

S16 interop reproduced a verified ten-event family with a correct current
Snapshot and no shared or producer projection. Synchronous graph startup
returned no title even though all authoritative events and payloads existed.

At [commit 1e9eee6](https://github.com/rails-to-cosmos/org-glance/commit/1e9eee6),
`src/data/org-glance-cache.el:1274` checks projection coherence
only when the directory proof is current. With a stale proof, incremental
reconciliation trusts the unchanged event inventory and skips the missing Row.

Reproduce by caching a family, deleting its shared and producer projections
and directory proof, then calling `org-glance-cache--reconcile-now`.

Startup now checks coherence independently and requests complete reconciliation
when projections are incoherent. Coherent stale inventories retain their
incremental path; unchanged warm startup creates no stage. The focused ERT
regression fails before the guard change and repairs the missing title after it.
