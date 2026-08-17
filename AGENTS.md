# org-glance — working conventions

`CLAUDE.md` is a symlink to this file.

## Documentation

- Docs are org-mode (`.org`); cross-references are org-links —
  `[[file:2026-07-06-performance-audit.org][Performance audit]]`, section anchor
  `[[file:foo.org::*Design][Design]]`.
- Proposal file name: `docs/proposals/YYYY-MM-DD-slug.<status>.org`, status `proposed` →
  `done` when implemented.
- After changing code, grep `docs/` and the proposals for what you touched and update
  them in the same pass; cite a git commit link beside file + line, never a bare
  `foo.el:365`.
- `docs/properties.org` lists every special drawer property, content marker
  (`#+begin_crypt`, `#+begin_pin`) and file keyword org-glance gives meaning to; adding,
  renaming or re-defining one updates it in the SAME pass — the rule that ships it, where
  it is set, where it is read.
- `CHANGELOG.org` is Keep a Changelog in org, date-based versions. A user-visible change
  adds a bullet under `* Unreleased` in the same pass, grouped `** Added` / `** Changed`
  / `** Fixed` / `** Removed`; a version bump renames `Unreleased` and opens a fresh one.
  An internal-only refactor owes none.
- KEEP THE FACT, DROP THE ESSAY. Rules files — this one, `docs/invariants.org`,
  `CHANGELOG.org` — carry facts, one line each: no reasoning, no history, no
  justification. A changelog bullet names the command, key or file that changed.
  Measurements and failure reproductions live in `docs/invariants.org`; argument lives in
  `docs/proposals/`.

## Global invariants

Rules the whole codebase enforces; no change may violate them. Full statements with
evidence anchors: [[file:docs/invariants.org][docs/invariants.org]].

1. WAL is append-only; duplicates resolve by physical position (last wins); `seq` is
   storage-only — never ordering, never re-stamped.
2. Store writes are atomic temp-then-rename; MANIFEST swap is the commit point;
   compaction commits MANIFEST before truncating the open segment.
3. A valid MANIFEST is byte-stable — rebuilt only when broken.
4. Schema tables (`org-glance-headline-metadata:fields`, `org-glance-filter:table`) are
   the single source of truth; append new metadata fields at the end only (row order =
   JSON key order). A list-valued field MUST encode to a JSON vector (`--append`'s
   `json-serialize` runs outside the error-demoted hook); the load-time guard checks slot
   ORDER, never encode kinds.
5. Blobs are canonical; indexes are derived and rebuildable; metadata computes before any
   write, blob lands before its WAL record. The content hash ignores the id/hash
   properties and the LOGBOOK drawers. The property index is a pure cache — hash-guarded
   with O(N) blob fallback, dropped by reindex, never trusted in a durable write. The
   `org-glance-material:` body link is canonical; `relations` AND `links` metadata are
   projections, never written independently. Occurrence snapshots are canonical content,
   GC'd with the id dir at compaction.
6. Ids are path-safety-checked via `error` (never `cl-assert`) before any filesystem use.
7. Single-user, no locking; staleness detection uses the full store snapshot (mtime +
   size + segment names), never mtime alone.
8. Git conflicts heal by union merge; `.eld` sidecar merges are commutative and
   non-inflating (earliest/latest/`max`, never a sum), the property index being the
   deliberate lossy-floor exception. The jsonl resolver is the WAL's alone, named as an
   ALLOWLIST — the open segment and `seg-<gen>.jsonl` — so every other JSONL family in
   `meta/` is out by construction: the notification queue, its generations, glance's
   `COMPLETIONS.jsonl`. The NAMES are the handle, the resolver running ahead of
   `--reconcile-manifest`. `--ensure-gitattributes` hands git THE SAME ALLOWLIST —
   `headlines.jsonl` and `seg-*.jsonl` off those two predicates, never a `*.jsonl` glob —
   covering the one cohort the gitignore cannot reach, git applying no ignore rule to a
   tracked path.
9. Side-index hooks, view refresh, occurrence snapshots and the plugin loader are
   error-demoted — they may never break a save, an open, a display or init;
   `org-glance-plugin-enable` is the deliberate loud counterpart.
10. View coherence is flag-stale + pull-refresh; when freshness is in doubt, rebuild.
11. Never clobber unsaved user edits: `user-error` or skip, never overwrite.
    `material:delete` names unsaved edits in its consent prompt and tombstones BEFORE
    discarding the buffer.
12. Store content parses in temp buffers via `org-glance--org-mode` (`delay-mode-hooks`,
    `tab-width` 8); never `find-file` sources to read.
13. Tags are canonical downcased interned symbols at the boundary; deserialized metadata
    carries STRING tags — coerce with `(downcase (format "%s" tag))` or read via
    `tag-strings`. Case-twins collapse at every boundary (parse, read, retag, material
    save); `org-tag-re` validation runs ONLY at creation boundaries, never on
    read/removal. Relation kinds are canonical dash-slugs at every boundary
    (encode/decode/deserialize); spaced form is display-only.
14. Crypt: plaintext never touches disk; `#+begin_crypt` markers are the persistent
    secrecy annotation. Secrecy is per-block — text between blocks stays plaintext and
    indexed, even for an encrypted headline. Materialize opens SEALED; decryption is
    explicit and hardens the buffer before any plaintext lands. An encrypted material
    buffer never snapshots; both encrypt paths purge existing snapshots.
15. Table Title column is mandatory — never removable/hideable; the rule is spelled once,
    in `org-glance-table--mandatory-column?` (remove-column refuses it, the prompt never
    offers it, compose-columns un-hides it).
16. Per-tag column schema: `:hidden` is a denylist of removed built-ins (new built-ins
    still appear); `C-c +` candidates exclude `ORG_GLANCE_*` + CATEGORY; order + sort
    persist separately per filter.
17. Transient filters (`:where`, `:refers-to`, `:id-any` — table-flagged, judged by
    `org-glance-filter:transient?`) never persist per-filter state: no overview cache, no
    table config, no column schema. Sole exception: explicit `C-c C-c` in a reference
    table saves the layout under a scope key (anchor id / tag pair, per direction) in
    `table-refs.eld` — never under the filter identity.
18. Scoped relation layouts: anchor travels as `:context` (never rebuilt from the filter);
    resolve = anchor entry > tag pairs > latest `:applied`; a scoped entry replaces the
    whole column set (never merges with the per-tag schema); scope-less relation views
    render defaults, never the ":none:" entry; pair order (anchor tag > row tag)
    single-sourced in `org-glance-table--refs-tag-pairs`, the mirrored pair being a
    DIFFERENT scope.
19. The table is a pure projection: every `--act-*` mutation delegates to the
    `org-glance-material:` layer; bulk ops degrade per-row, never batch-abort.
20. Overview headings are self-sufficient, metadata-only: state, priority, one planning
    line, id drawer, interval line, relations, plain links — zero blob parses; agenda +
    link-following need no materialization.
21. Reserved properties (`org-glance-material-hidden-properties`) are managed keys:
    concealed in material buffers, hand edits reverted on save with a warning; the revert
    touches only the heading drawer (disjoint from the crypt seal).
22. Material saves rewrite user content only through announced normalize hooks
    (reserved-property revert, case-twin tag collapse — each warns — and the crypt seal).
23. LLM session state (running/exited/stopped, buffer names, titles) derives live at fill
    from the provider's recorded sessions for this graph overlaid with live `*llm:…*`
    buffers — never persisted, never a full-graph scan. Enforcing code lives in the
    external `org-glance-llm` plugin repo (loaded via `org-glance-plugins`), which the
    rule governs. `org-glance-graph:cache-read`/`:cache-write` stay the public sidecar API
    for a plugin that does want a derived cache under `cache/`.
24. Table refills restore the (row, CELL) pair via `org-glance-view:point-context` /
    `:restore-point` — never just the row.
25. Links are addressed by their enclosing list-item path plus their own label —
    description, else the `KEY:` text introducing them in their item, else the raw link
    (the link's own item label is dropped); the picker descends one component per prompt,
    takes a lone candidate at any depth, and breaks exhausted-path ties by target.
26. Plugins (`org-glance-plugins`) load error-demoted, self-register their UI
    remove-then-append, and never unload.
27. `after-save-hook` depth order is load-bearing: `material:sync` (0) runs BEFORE
    `--decrypt-buffer` (90) and `--hide-reserved-properties` (100), so the WAL and every
    index see SEALED bytes — the mechanism behind inv 14.
28. Crypt block rewrites iterate blocks LAST-to-FIRST; forward iteration invalidates later
    blocks' recorded positions after any length change.
29. `--from-element` strips org's `ARCHIVE` marker from the parsed tag set; it is
    bookkeeping (the `archived?` flag), never a collection tag.
30. `org-glance-graph:get-headline` is tri-state — nil (unknown) / `tombstone` / metadata.
    Read-only callers collapse it via `org-glance-graph:live-meta`; `graph:delete` and the
    external fold need the tombstone case and share one guard, `graph--tombstone-spec`.
31. `org-glance-material` never top-level `require`s `org-glance-table` (table requires
    material); the relation pivot requires it at call time.
32. Torn-line tolerance is scoped to the OPEN segment's FINAL line; a JSON error anywhere
    else re-signals.
33. External writers (the `glance` daemon) append moved ids to `meta/EXTERNAL.jsonl`; a
    delete wears the third field `"tombstone":true` and nothing else wears it. The fold
    keeps one entry per id at its first sighting with its LAST sighting's kind, tests it
    with `(eq t …)`, and treats an unknown key as inert (compatibility, both ways). Every
    READ folds pending entries in (`--fold-external-maybe` in `--ensure-cache`) —
    throttled, non-reentrant, `condition-case`'d — as ONE `graph:insert` for records and
    tombstones alike; the fold's cursor moves always AFTER that append.
34. The fold moves a CURSOR and never rewrites `EXTERNAL.jsonl`. `meta/EXTERNAL.cursor`
    is `OFFSET WINDOW PREFIX` — a decimal BYTE offset plus two sha1s — written
    temp-then-rename after the records land (inv 33's crash rule). Any doubt reads as 0
    (cursor absent, garbled, fewer than two digests, bytes that no longer hash, more
    bytes than the file holds), never guarded with `max`. Reads never mutate, so two
    Emacsen need no lock (inv 7). Growth is ROTATION: cursor first, then the file
    renamed `EXTERNAL-<gen>`, drained ahead of the live file and retired on its own
    cursor. Retirement MOVES a spent generation into `meta/spent/` and never unlinks
    one; only `org-glance-graph:clear-spent-external` removes what it left. The family
    is git-ignored — what crosses machines is the WAL record the fold produces, at the
    cost of hazard H3. Refusals, ordering rules, the retirement contract and the
    evidence: [[file:docs/invariants.org][docs/invariants.org]].

## Known hazards

Costs the code ships WITH; a letter is never reused, each pinned by a case in
`test/test-external.el` that turns red when it closes. An OPEN entry states its cost, what
would close it and why it stands; a CLOSED one keeps its letter and what closed it. Full
statements: [[file:docs/invariants.org][docs/invariants.org]].

H1. CLOSED 2026-08-12 by inv 34 — the external fold's truncate race ate a tombstone. The
    compare-and-swap that preceded it left an inner window (`f-write-text` is
    `write-region` with `append` nil); the cursor closes it by removing the rewrite. Of
    its two SYNCED causes the conflict resolver is shut by structure (inv 8) and the git
    union merge only where git does not already track the family (inv 34, at the cost of
    H3) — on a store that predates the ignore it is live, and the digest is what stands
    there.
H2. A folded delete is undone by the id's open material buffer: the tombstone arm touches
    no buffer, so the next save's `material:sync` appends a LIVE record and writes the
    blob back, and the occurrence history stays behind. Open because a fold runs in the
    BACKGROUND, and `material:delete`'s consent-when-dirty guard (inv 11) exists because
    discarding a dirty buffer needs a human.
H3. A daemon HERE cannot notify an Emacs THERE: the notification family is git-ignored —
    once git stops tracking it, which on a store made before the ignore takes a
    `git rm --cached` — so machine B folds nothing A's daemon announced. Free where the
    daemon and Emacs share a machine, which is what `glance` is built for; a per-host
    `EXTERNAL-<host>.jsonl` would close it and is written up as NOT TAKEN. A reader who
    has the topology undoes it per store with `git add -f`, and takes the merge door back
    with it.

## Fix — and prevent — the whole class

A reported problem is one sample of a class: state the class, find all its sites, fix them
in the same pass, verify green — a redundant point reset, a duplicated computation, an
O(N²) idiom, a rhetorical tic in prose.

- **Authors / actors:** write the general form correctly the first time; a fresh instance
  of a known class is a regression.
- **Reviewers:** name the class a change belongs to and scan for its other instances.

## Docstrings & comments

- Comment density stays near 3% of lines.
- A comment earns its line only as one of four, each ONE line: a hazard or ordering
  constraint the code cannot state; a pointer to the invariant that owns the rule
  (`;; invariant 27: sync runs before --decrypt-buffer.`); a deliberate-difference note
  ("spelled twice on purpose"); "why not the obvious thing".
- A rule lives in the invariant list once; a comment restating it, or a sentence restating
  the line under it, is deleted on sight.
- Docstrings are proper English and checkdoc-valid (public API, shown by `C-h f`): a
  complete imperative first line, arg names in CAPS, facts intact. Terse, but complete.
- Never the "negation-reveal" pattern ("not X, but Y" / "it's not just A, it's
  B") in any generated text — docs, comments, commit messages, prose. State the
  point directly.

## Code conventions

- **Naming:** public API uses `:` (`org-glance-headline:metadata`); private helpers use
  `--` (`org-glance-graph--append`) — never `:--`.
- **Docstrings:** keep every line ≤ 80 columns (byte-compile warns otherwise).
- **Rich args:** functions taking several lambda/same-typed arguments use
  `cl-defun … &key`, not positional.

## Build / test

```sh
eask recompile        # must compile warning-free
eask run command test # the ERT suite
```
