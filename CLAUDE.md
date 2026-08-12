# org-glance — working conventions

## Documentation is org-mode, not markdown

All documentation authored in this repo is written in **org-mode**
(`.org`). Cross-reference documents with **org-links**, e.g.
`[[file:2026-07-06-performance-audit.org][Performance audit]]` or a
section anchor `[[file:foo.org::*Design][Design]]`. Proposals live in
`docs/proposals/` and follow the naming `YYYY-MM-DD-slug.proposed.org`
→ renamed to `…​.done.org` when implemented. The changelog is
`CHANGELOG.org`.

## Keep doc references live

After changing code, grep the docs (`docs/`, the proposals) for
references to anything you touched and update them in the same pass. A
proposal citing `foo.el:365` after a refactor silently misleads the
next reader, so better use links to git commit itself alongside file +
line.

## Keep the properties reference current

`docs/properties.org` lists every special drawer property, content
marker (`#+begin_crypt`, `#+begin_pin`), and file keyword org-glance
gives meaning to. When a feature adds, renames, or changes the
semantics of one (e.g. a new `ORG_GLANCE_*` property or `#+begin_*`
block), update `docs/properties.org` in the **same pass** — the rule
that ships it, where it is set, and where it is read.

## Keep the CHANGELOG current

`CHANGELOG.org` is the running summary of the project's actual state
(Keep a Changelog structure, in org; date-based versions). After
implementing a feature or any user-visible change, add a bullet under
the `* Unreleased` heading in the **same pass** — grouped under `**
Added` / `** Changed` / `** Fixed` / `** Removed`. On a version bump,
rename `Unreleased` to the new version and open a fresh `*
Unreleased`. A user-visible change landing without a changelog entry
is an incomplete change; internal-only refactors (no behaviour change)
need no entry.

Keep it **compact — facts only**: one line per bullet, naming the command, key,
or file that changed. No rationale, no prose; that belongs in `docs/proposals/`.

## Global invariants

Rules the whole codebase enforces; no change may violate them. Full statements
with evidence anchors: [[file:docs/invariants.org][docs/invariants.org]].

1. WAL is append-only; duplicates resolve by physical position (last wins);
   `seq` is storage-only — never ordering, never re-stamped.
2. Store writes are atomic temp-then-rename; MANIFEST swap is the commit
   point; compaction commits MANIFEST before truncating the open segment.
3. A valid MANIFEST is byte-stable — rebuilt only when broken.
4. Schema tables (`org-glance-headline-metadata:fields`,
   `org-glance-filter:table`) are the single source of truth; append new
   metadata fields at the end only (row order = JSON key order). A list-valued
   field MUST encode to a JSON vector — `--append`'s `json-serialize` runs
   outside the error-demoted hook, so a nil/list encoder crashes every save.
5. Blobs are canonical; indexes are derived and rebuildable; metadata computes
   before any write, blob lands before its WAL record. The content hash ignores
   the id/hash properties and the LOGBOOK drawers, so clock churn never
   invalidates it. The property index is a
   pure cache — hash-guarded with O(N) blob fallback, dropped by reindex; never
   trust it in a durable write. The `org-glance-material:` body link is
   canonical; `relations` AND `links` metadata are projections, never written
   independently.
6. Ids are path-safety-checked via `error` (never `cl-assert`) before any
   filesystem use.
7. Single-user, no locking; staleness detection uses the full store snapshot
   (mtime + size + segment names), never mtime alone.
8. Git conflicts heal by union merge; `.eld` sidecar merges are commutative and
   non-inflating (earliest/latest/`max`, never a sum).
9. Side-index hooks, view refresh, occurrence snapshots and the plugin loader
   are error-demoted — they may never break a save, an open, a display or
   init; `org-glance-plugin-enable` is the deliberate loud counterpart.
10. View coherence is flag-stale + pull-refresh; when freshness is in doubt,
    rebuild.
11. Never clobber unsaved user edits: `user-error` or skip, never overwrite.
12. Store content parses in temp buffers via `org-glance--org-mode`
    (`delay-mode-hooks`, `tab-width` 8); never `find-file` sources to read.
13. Tags are canonical downcased interned symbols at the boundary; deserialized
    metadata carries STRING tags — coerce with `(downcase (format "%s" tag))`
    or read via `tag-strings`. Case-twins collapse at every boundary (parse,
    read, retag, material save); `org-tag-re` validation runs ONLY at creation
    boundaries, never on read/removal. Relation kinds are canonical dash-slugs
    at every boundary (encode/decode/deserialize); spaced form is display-only.
14. Crypt: plaintext never touches disk; `#+begin_crypt` markers are the
    persistent secrecy annotation. Secrecy is per-block — text between blocks
    stays plaintext and indexed, even for an encrypted headline. Materialize
    opens SEALED; decryption is explicit and hardens the buffer before any
    plaintext lands.
15. Table Title column is mandatory — never removable/hideable; the rule is
    spelled once, in `org-glance-table--mandatory-column?` (remove-column
    refuses it, the prompt never offers it, compose-columns un-hides it).
16. Per-tag column schema: `:hidden` is a denylist of removed built-ins (new
    built-ins still appear); `C-c +` candidates exclude `ORG_GLANCE_*` + CATEGORY;
    order + sort persist separately per filter.
17. Transient filters (`:where`, `:refers-to`, `:id-any` — table-flagged,
    judged by `org-glance-filter:transient?`) never persist per-filter state:
    no overview cache, no table config, no column schema.  Sole exception:
    explicit `C-c C-c` in a reference table saves the layout under a scope
    key (anchor id / tag pair, per direction) in `table-refs.eld` — never
    under the filter identity.
18. Scoped relation layouts: anchor travels as `:context` (never rebuilt from
    the filter); resolve = anchor entry > tag pairs > latest `:applied`;
    a scoped entry replaces the whole column set (never merges with the
    per-tag schema); scope-less relation views render defaults, never the
    ":none:" entry; pair order (anchor tag > row tag) single-sourced in
    `org-glance-table--refs-tag-pairs`.
19. The table is a pure projection: every `--act-*` mutation delegates to the
    `org-glance-material:` layer; bulk ops degrade per-row, never batch-abort.
20. Overview headings are self-sufficient, metadata-only: state, priority,
    one planning line, id drawer, interval line, relations, plain links —
    zero blob parses; agenda + link-following need no materialization.
21. Reserved properties (`org-glance-material-hidden-properties`) are managed
    keys: concealed in material buffers, hand edits reverted on save with a
    warning; the revert touches only the heading drawer (disjoint from the
    crypt seal).
22. Material saves rewrite user content only through announced normalize hooks
    (reserved-property revert, case-twin tag collapse — each warns — and the
    crypt seal).
23. LLM session state (running/exited/stopped, buffer names, titles) derives
    live at fill from two cheap sources — the provider's recorded sessions for
    this graph, overlaid with live `*llm:…*` buffers — never persisted, never a
    full-graph scan. Enforcing code lives in the external `org-glance-llm`
    plugin repo (loaded via `org-glance-plugins`); the rule governs that
    plugin. `org-glance-graph:cache-read`/`:cache-write` stay the public
    sidecar API for a plugin that does want a derived cache under `cache/`.
24. Table refills restore the (row, CELL) pair via
    `org-glance-view:point-context` / `:restore-point` — never just the row.
25. Links are addressed by their enclosing list-item path plus their own label
    — description, else the `KEY:` text introducing them in their item, else
    the raw link (the link's own item label is dropped); the picker descends
    one component per prompt and breaks path ties by target.
26. Plugins (`org-glance-plugins`) load error-demoted, self-register their UI
    remove-then-append, and never unload.
27. `after-save-hook` depth order is load-bearing: `material:sync` (0) runs
    BEFORE `--decrypt-buffer` (90) and `--hide-reserved-properties` (100), so
    the WAL and every index see SEALED bytes — the mechanism behind inv 14.
28. Crypt block rewrites iterate blocks LAST-to-FIRST; forward iteration
    invalidates later blocks' recorded positions after any length change.
29. `--from-element` strips org's `ARCHIVE` marker from the parsed tag set;
    it is bookkeeping (the `archived?` flag), never a collection tag.
30. `org-glance-graph:get-headline` is tri-state — nil (unknown) /
    `tombstone` / metadata. Read-only callers collapse it via
    `org-glance-graph:live-meta`; `graph:delete` and the external fold need
    the tombstone case and share one guard, `graph--tombstone-spec`.
31. `org-glance-material` never top-level `require`s `org-glance-table`
    (table requires material); the relation pivot requires it at call time.
32. Torn-line tolerance is scoped to the OPEN segment's FINAL line; a JSON
    error anywhere else re-signals rather than masking store corruption.
33. External writers (the `glance` daemon) append moved ids to
    `meta/EXTERNAL.jsonl`, a delete wearing the third field `"tombstone":true`
    and nothing else wearing it; the fold keeps one entry per id at its first
    sighting with its LAST sighting's kind, tests it with `(eq t …)`, and
    treats an unknown key as inert (the compatibility claim, both ways). Every
    READ folds pending entries in (`--fold-external-maybe` in
    `--ensure-cache`) — throttled, non-reentrant, `condition-case`'d — as ONE
    `graph:insert` for records and tombstones alike, and Emacs alone truncates
    the file, always AFTER that append.

## Known hazards

Costs the code ships WITH — lettered, because an invariant is a rule a change
must preserve and these are what today's design gives up. Each was reproduced by
probe and pinned by a test in `test/test-external.el`, so closing one turns its
case red. Full statements, with the evidence:
[[file:docs/invariants.org][docs/invariants.org]].

H1. The external fold's truncate race eats a tombstone for good. Two Emacsen
    folding one store take no lock (inv 7), so the second `--truncate-external`
    drops characters counted against a file the first already emptied. A lost
    WRITE note self-heals — the blob is still the truth and the id's next edit
    says so again; a lost TOMBSTONE never does, and the record stays live over
    bytes in the daemon's trash. Closing it wants a lock across Emacsen, which
    reopens inv 7.
H2. A folded delete is undone by the id's open material buffer. The tombstone
    arm touches no buffer, so the next save's `material:sync` appends a LIVE
    record and writes the blob back; the occurrence history stays behind. Open
    because a fold runs in the BACKGROUND, and `material:delete`'s
    consent-when-dirty guard (inv 11) exists because discarding a dirty buffer
    needs a human.

## Fix — and prevent — the whole class

A reported problem is one sample of a class. When you fix it, sweep the codebase
for every instance of the same class and fix them in the same pass — a redundant
point reset, a duplicated computation, an O(N²) idiom, a rhetorical tic in prose.
State the class, find all sites, fix together, verify green.

Prevent it going forward:
- **Authors / actors:** write the general form correctly the first time; adding a
  fresh instance of a known class is a regression.
- **Reviewers:** flag the class a change belongs to and scan for its other
  instances, so one fix generalizes and the class stops recurring.

## Docstrings & comments

Cut genuine bloat — over-explanation, redundancy, three sentences where one
works. Keep docstrings proper English and checkdoc-valid (they are public API,
shown by `C-h f`): a complete imperative first line, arg names in CAPS, facts
intact. Terse, but complete.

Never use the "negation-reveal" pattern ("not X, but Y" / "it's not just A,
it's B" / "this isn't about A, it's about B") in any generated text — docs,
comments, commit messages, prose. State the point directly.

## Code conventions

- **Naming:** public API uses `:` (`org-glance-headline:metadata`);
  private helpers use `--` (`org-glance-graph--append`) — never `:--`.
- **Docstrings:** keep every line ≤ 80 columns (byte-compile warns
  otherwise).
- **Rich args:** functions taking several lambda/same-typed arguments
  use `cl-defun … &key`, not positional.

## Build / test

```sh
eask recompile        # must compile warning-free
eask run command test # the ERT suite
```
