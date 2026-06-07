# Generalize: one descriptor table for overview filter keys

**Status:** proposed · generalizer audit 2026-06-07

## Pattern

Every overview filter key is wired by hand into **four lockstep sites** in
`org-glance-overview-v2.el` (plus docstrings and tests). The latest variant
(`:title-contains`) touched: the `filter-keys` defconst, a `spec-predicate`
clause, a `--key-value` case, and a `--canonical-pairs` case — and the
canonicalize/slug logic for `:tags`/`:done-keywords`/`:title-contains` is
*literally duplicated* between the last two. Two steps are **silently
forgettable**:

- add the key to `filter-keys` but forget the `spec-predicate` clause → the
  filter **silently matches everything** (no clause pushed = no constraint);
- forget the `--canonical-pairs`/`--key-value` case → wrong-but-plausible
  cache identity (e.g. case-variants fragmenting the cache).

The boolean trio and the `:schedule`/`:deadline` pair are already loop-driven
(proof the shape wants to be a table); the scalar/normalizing keys never got
the same treatment.

## Files

- `org-glance-overview-v2.el`: `filter-keys` (~l.56), `spec-predicate`
  (~l.102–166), `--key-value` (~l.171–183), `--canonical-pairs` (~l.185–197),
  `--normalize-spec` special-casing (~l.74–89).

## Proposed change

One declarative table; all four helpers derive from it:

```elisp
(defconst org-glance-overview-v2:filter-table
  ;; KEY -> (:accessor FN :match KIND :canon FN)
  ;; KIND ∈ equal | eql | bool | substring | member-all | present-absent
  `((:tags           :accessor ,#'org-glance-headline-metadata-v2:tags
                     :match member-all :canon downcase-sort)
    (:state          :accessor ,#'org-glance-headline-metadata-v2:state :match equal-or-empty)
    (:title          :accessor ,#'org-glance-headline-metadata-v2:title :match equal)
    (:title-contains :accessor ,#'org-glance-headline-metadata-v2:title
                     :match substring :canon downcase)
    (:priority       :accessor ,#'org-glance-headline-metadata-v2:priority :match eql)
    (:linked         :accessor ,#'org-glance-headline-metadata-v2:linked? :match bool)
    ;; ... :propertized :encrypted :id :hash :schedule :deadline
    ))
```

- `filter-keys` := table keys + the three structural specials.
- `spec-predicate` := loop over the table (`plist-member` gate, build clause
  from `:match` kind); a key present in the table without a `:match` errors at
  load — the silent-match-all hole closes.
- `--canonical-pairs` / `--key-value` := one `:canon` lookup each; the
  duplicated three-branch cascade disappears.
- `:done`/`:done-keywords` (parameterized pair) and `:where` (escape hatch)
  stay special-cased — forcing them into the table would be over-abstraction.

**Constraint:** the table must reproduce today's cache keys and SPEC
identities byte-for-byte — `test-overview.el` pins exact key strings
(`tags=task`, `state=TODO&tags=work`, …) and identity inequalities; they are
the acceptance gate.

## LOC estimate

+~40 (table + kind builders) / −~55 now (clause cascade + duplicated canon
branches). Per future key: ~12 lines across 4 sites → **1 table row**; closes
two silent failure modes. Plausible future keys: `:tags-any` (OR), `:level`,
`:archived`, `:priority-range`, `:closed` — 4+.

## Risk

No store-format or public-API change. Cache dirs/SPEC sidecars regenerate
harmlessly if identities shifted — but tests forbid that anyway. Single-file
locality.

## Existing precedent

The boolean-flag loop and `:schedule`/`:deadline` loop inside `spec-predicate`
itself (`org-glance-overview-v2.el` ~l.145–162) — this proposal finishes what
they started.
