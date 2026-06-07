# Generalize: org-glance-core-v2.el for the graph-lifecycle trio

**Status:** proposed · generalizer audit 2026-06-07

## Pattern

Every v2 command module depends on three session primitives that physically
live in the top-level `org-glance.el` that loads them — a genuine load cycle
papered over with declarations:

- the global `(defvar org-glance-graph-v2 …)` (org-glance.el ~l.93) is
  re-declared bare in `material-v2.el` and `overview-v2.el` and read at ~12
  sites;
- `org-glance-initialized?-v2` is `declare-function`-ed in **three** files
  (`capture-v2.el`, `material-v2.el`, `overview-v2.el`) and asserted at 9 call
  sites;
- `org-glance-init-v2` completes the trio.

## Files

- `org-glance.el` (~l.93, ~l.250–256), `org-glance-capture-v2.el` (~l.12–14),
  `org-glance-material-v2.el` (~l.25–26), `org-glance-overview-v2.el`
  (~l.43–44).

## Proposed change

A ~20-line `org-glance-core-v2.el` holding exactly: the `org-glance-graph-v2`
global defvar, `org-glance-init-v2`, `org-glance-initialized?-v2` (and
plausibly the `org-glance-use-graph-v2` defcustom, today in material-v2 for no
deep reason). All v2 modules `(require 'org-glance-core-v2)`; the bare
defvars and the triplicated `declare-function` lines disappear;
`org-glance.el` requires core-v2 like everything else.

Deliberately **not** moved: `org-glance-tags:completing-read`,
`org-glance:create-tag` (v1-coupled; Phase 3 deletes their world).

## LOC estimate

+~25 (new file) / −~10 (declarations) now — the win is structural: the v2
stack becomes acyclic for real, new v2 modules need one require instead of a
declaration block, and Phase 3's v1 deletion gets simpler (org-glance.el
shrinks toward pure wiring).

## Risk

Pure relocation, no behavior change; load-order regressions would surface in
the init path every test exercises. Low-medium.

## Existing precedent

`org-glance-graph-v2.el` already plays exactly this base-module role for the
store layer — every v2 file requires it with no back-edge.
