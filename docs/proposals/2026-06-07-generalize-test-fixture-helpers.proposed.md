# Generalize: shared fixtures for the v2 test suite

**Status:** proposed · generalizer audit 2026-06-07

## Pattern

`test/test-helpers.el` has only 4 helpers; the growing v2 suite hand-rolls the
same idioms inline:

- **staleness control** — `(set-file-times (org-glance-graph-v2:headline-meta-path g) (time-subtract/add (current-time) 100))` ×7 in `test-overview.el` (+1 in `test-segments.el`), with the magic `100` copy-pasted;
- **render spy** — `cl-letf` over `org-glance-overview-v2:render` counting calls, ×3 verbatim;
- **`completing-read` stub** — ×13 in `test-material.el`, ×4 in `test-overview.el`, several ad-hoc lambda shapes;
- **materialized-save simulation** — insert content + `setq-local` material locals + `(org-glance-material-v2:sync)`, ×2;
- **crash injection** — `cl-letf` over `--write-manifest` to error at the commit point, ×2 in `test-segments.el`;
- `reopen-graph` / `sealed-segments` / inline `count-records` live in `test-segments.el` but are general.

Roughly half of every new cache/coherence/recovery test is this boilerplate.
Bonus altitude fix: funneling tests' use of graph-v2 `--`-privates through
`org-glance-test:` wrappers keeps the privates free to change (precedent: the
existing `org-glance-test:sealed-segments` wrapper).

## Files

- `test/test-helpers.el` (target), `test/test-overview.el`,
  `test/test-segments.el`, `test/test-material.el`.

## Proposed change

Add to `test-helpers.el` (move the `test-segments.el` locals there too):

```elisp
(org-glance-test:make-fresh graph)     ; backdate headlines.jsonl vs caches
(org-glance-test:make-stale graph)     ; future-date headlines.jsonl
(org-glance-test:with-render-spy (count) body…)        ; macro
(org-glance-test:stub-completing-read result body…)    ; macro
(org-glance-test:simulate-material-save graph id content)
(org-glance-test:with-crash-at fn body…)               ; macro: FN errors once
(org-glance-test:count-records graph)
(org-glance-test:reopen-graph graph)   ; moved
(org-glance-test:sealed-segments graph); moved
```

Then sweep the three test files to use them.

## LOC estimate

+~25 in `test-helpers.el` / −~80–120 across the suite. Per future
cache/coherence/recovery test: ~5–10 lines saved, and staleness/spy semantics
documented once instead of re-derived.

## Risk

Test-only; zero production risk. The sweep must keep all 97 tests green —
mechanical.

## Existing precedent

`org-glance-test:with-graph` / `org-glance-test:headline` /
`org-glance-test:sealed-segments` — same convention, just underpopulated.
