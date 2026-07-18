# Generalize: graph-owned content-path accessor

**Status:** proposed · generalizer audit 2026-06-07

## Pattern

The blob filename `"data.org"` joined onto `headline-data-path` appears three
times — twice inside the store (`put-content`, `get-content`) and once
**across the layer boundary** in `org-glance-material-v2:open` (~l.121), which
hand-builds the path and re-implements the existence check (`f-exists?` +
"No stored content" error) that the store's `get-content`/`headline` already
encode. The blob layout is a graph-v2 storage detail (MIGRATION-PLAN
Appendix A); the view layer shouldn't know it.

## Files

- `org-glance-graph-v2.el` (~l.410, ~l.420), `org-glance-material-v2.el`
  (~l.118–123).

## Proposed change

```elisp
(cl-defun org-glance-graph-v2:content-path (graph id)
  "Path of ID's content blob (existing or not)."
  (f-join (org-glance-graph-v2:headline-data-path graph id) "data.org"))
```

`put-content`/`get-content` route through it; `material-v2:open` becomes
"liveness check → `content-path` → error if missing → `find-file`" with zero
knowledge of `data.org` or the shard layout.

## LOC estimate

+6 / −8 now; every future consumer that needs the blob *path* (sync
hardening in Phase 4 will) gets it for free instead of forking the join.

## Risk

Trivial — additive accessor + internal re-route; public behavior unchanged.

## Existing precedent

The path-accessor chain in graph-v2 itself
(`store-path` → `data-path`/`meta-path` → `headline-meta-path`);
`content-path` is the missing rung.
