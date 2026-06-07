# org-glance — Migration & Stabilization Plan

Status: **active** · Started 2026-06-03 · Branch: `master`

This document is the agreed plan for simplifying org-glance and bringing it to a
stable state. It supersedes the scattered TODOs in `org-glance.org` for the
migration effort specifically.

---

## 1. Background

org-glance is an Emacs knowledge-graph over org-mode:

> headlines → tagged with **classes/tags** → each tag gets a read-only
> **overview** buffer (clones matching headlines across files) → **materialize**
> one headline into an editable buffer → **sync back** to source (hash-based
> conflict detection) → a persistent **graph** indexes headline metadata.

The repository is frozen mid-rewrite. Two stacks coexist:

| | **v1** (production) | **v2** (incoming) |
|---|---|---|
| Headline | `org-glance-headline.el` — org-element list, `car='headline` | `org-glance-headline-v2.el` — `cl-defstruct`, lazy thunks |
| Store | `org-glance-metadata.el` — per-tag `.metadata.el`, `prin1` hash-table | `org-glance-graph-v2.el` — append-only `headlines.jsonl` + tombstones |
| Capture | `org-glance-capture` | `org-glance-capture-v2` |
| Init | `org-glance-init` | `org-glance-init-v2` |
| Wired into commands? | **yes, all of them** | **no** (reachable only via `org-glance-init-v2`) |
| Tested? | barely | by tests that are **disabled** in `Eask` |

**Dangerous inversion:** production runs on v1; the tests target v2; and v2 is
both unfinished and buggy. The store models differ fundamentally:

- **v1 `.metadata.el`** is a *lightweight index into source files*: `id →
  (plain-title begin file commentedp archivedp linked propertized encrypted
  buffer closed)`. The org files are the source of truth; metadata caches a
  pointer (`file` + `begin`) and some feature flags.
- **v2 `headlines.jsonl`** stores a *denormalized snapshot* per headline (state,
  title, tags, hash, schedule, deadline, priority), append-only, last-write-wins
  via reverse scan, with `:tombstone t` records for deletion.

---

## 2. Decisions (locked)

1. **Coexistence, not big-bang.** Keep v1 working as a fallback while v2 matures.
   Provide a **careful runtime migration** from old-style metadata to the new
   graph: on init, detect legacy `.metadata.el`, emit a **deprecation warning**
   and **prompt** the user to migrate.
2. **JSONL for now.** The append-only `headlines.jsonl` graph is good enough as a
   prototype store. Do not redesign the storage format yet.
3. **Single-user assumption.** Drop the graph mutex / locking entirely; the model
   is sequential.
4. **Fix-first sequencing.** Repair the v2 graph core *on master* before adopting
   the `refactor` branch's rename/reorg. The refactor branch is rename-first and
   carries the same bugs forward (plus new regressions — `graph:lock` called but
   its definition deleted; struct slot renamed `directory`→`location` while
   accessors still call `:directory`). We harvest its *ideas*, not its diff.
5. **Dev-env:** adopt the `refactor` branch's **Dockerfile** (with supply-chain
   hardening as a follow-up). **Do NOT** adopt `flake.nix` / `flake.lock`.
6. **Metadata index = pure projection.** The JSONL metadata is a *denormalized
   projection* of the headline (the hot fields views need), distinct from the
   *cold* content blob; the `hash` field links the two. Keep the metadata struct
   **behavior-free** and only ever produce it via `org-glance-headline-v2:metadata`
   so its shape can't drift. The one deliberate coupling — `graph-v2:headline`
   consulting the index — is for **liveness** (tombstones), which is an index
   concern, not storage. (We considered dropping the index and deriving on demand;
   rejected — it would turn every list/agenda op into O(N) org-parses.)

---

## Status — 2026-06-05

- ✅ **Phase 0 (graph core)** — done & green.
- ✅ **Phase 0.5 (capture-v2 + content persistence)** — done & green.
- ✅ **Phase 1 (runtime migration)** — done & green.
- ✅ **Phase 2 (wire v2 into commands; flip default)** — **COMPLETE.** Selection /
  materialize / open / extract / overview / agenda all read the v2 graph;
  `org-glance-use-graph-v2` now defaults **on**. Overview adds filtering (plist
  spec), per-filter caching, v1-parity keymap, and per-overview done-keywords.
- ✅ **Phase 5 (Podman dev-env)** — done; `make podman-test [EMACS_VERSION=…]`.
- ✅ **Wired suite:** **71/71** (`eask run command test`), green local (30.2) **and**
  in-container (Emacs 29.1).
- ✅ **v2 headline unit tests** — green (run separately; not yet wired into
  `Eask`, see Phase 4).
- ▶️ **Next:** Phase 3 (delete v1) or Phase 4 (stabilize: CI, GC, harden sync) —
  see those sections.

Tooling: `eask` via `mise` (`npm:@emacs-eask/cli`); run with
`mise exec -- eask run command test`, or `make podman-test` for pinned Emacs.

---

## 3. Guiding principles

- The **org files are canonical.** Metadata/graph are rebuildable indexes. This
  makes migration safe: worst case, rebuild from sources.
- **Preserve existing `ORG_GLANCE_ID`s** through migration so `org-glance-visit:`
  / `org-glance-open:` links keep working.
- Every phase ends **green** (tests pass) before the next begins.
- Honor `CODESTYLE.org`: `?`-predicates, `struct:field` / `struct:-field`,
  3-part lazy methods, `-v2` suffix until v1 is deleted.

---

## Phase 0 — Repair the v2 graph core ✅ DONE

The keystone. Until the graph round-trips correctly, every "promote v2" step
rests on broken code. Scope is `org-glance-graph-v2.el` + its immediate consumer
`org-glance-capture-v2.el` + real tests.

**Shipped:** all bugs below fixed; plus two issues surfaced during the work —
(a) `headline-data-path` crashed on ids < 2 chars (`substring`); now shards long
ids, stores short ones flat; (b) the v2 graph stored `data/`+`meta/` directly
under `org-glance-directory`, which v1 tag discovery (`^[[:word:]]+`) mis-read as
phantom tags — the store now nests under a dot-prefixed **`.org-glance/`** root
that v1 and org-agenda both ignore.

### Bugs to fix

| #   | Location                                                                     | Bug                                                                                                                                             | Fix                                                                                           |
|-----|------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| 0.1 | `graph-v2.el` metadata struct (l.30–38)                                      | no `:id` field, yet `:get-headline` looks up by `:id` and serialize never writes it → graph is **write-only**                                   | add `id` slot; populate in `:metadata`; write in `:serialize`; read in `:deserialize`         |
| 0.2 | `headline-v2:metadata` (l.40–49)                                             | `:schedule`/`:deadline` copied as **org-element timestamp objects** (lists of symbols) → `json-serialize` **errors** on any scheduled headline  | coerce to `:raw-value` strings (nil stays nil → `{}` → nil on read)                           |
| 0.3 | `:get-headline` (l.158–165)                                                  | calls `(deserialize graph it)` but `deserialize` takes 1 arg                                                                                    | drop the `graph` arg                                                                          |
| 0.4 | `:delete` (l.167–172)                                                        | calls non-existent `metadata-v2:tombstone` accessor; `:insert` call passes a bare plist whose `car` is a keyword (fails `:insert`'s type check) | check the `'tombstone`/`nil` sentinel from `:get-headline`; wrap the tombstone spec in a list |
| 0.5 | `:merge` (l.27)                                                              | empty stub; only caller is `capture-v2`                                                                                                         | delete it; reroute capture to `:add`                                                          |
| 0.6 | `:add` (l.151–156)                                                           | returns loop junk, not the graph (not chainable)                                                                                                | return `graph`                                                                                |
| 0.7 | struct `mutex` slot + `:lock` macro                                          | unused under single-user                                                                                                                        | delete both; inline `:insert` / `:make-id` bodies                                             |
| 0.8 | duplicate `defcustom org-glance-directory` (here **and** `org-glance.el:81`) | redefinition smell                                                                                                                              | keep one canonical definition                                                                 |
| 0.9 | `org-glance.el:91` `(defvar org-glance-graph-v2 (org-glance-graph-v2 …))`    | constructs a graph (and `mkdir`s) at **load time**, against the *default* dir                                                                   | make it lazy (`nil`); construct in `org-glance-init-v2`                                       |

### Acceptance criteria

- New `test/test-graph.el` (replacing the require-only stub) is green:
  - add a headline → `:get-headline` by id returns metadata with matching
    `id`/`title`/`state`/`tags`/`hash`;
  - add two headlines → both retrievable;
  - update (re-add) a headline → latest wins (reverse-scan);
  - `:delete` → `:get-headline` returns `'tombstone`;
  - a **scheduled** headline round-trips without a serialize crash (regression
    guard for 0.2).
- `org-glance-capture-v2` no longer references `:merge`; the after-finalize hook
  appends to the graph without error.
- The v2 headline unit tests in `test/test-headline.el` pass (they exercise the
  model the graph depends on).

---

## Phase 0.5 — Complete capture-v2 + content persistence ✅ DONE

Make capture produce a complete, retrievable headline.

- **Per-namespace ids.** `org-glance-graph-v2:capture` assigns a fresh
  `ORG_GLANCE_ID` to every id-less headline via `:make-id` (a UUID reserved by
  `mkdir`-ing its content-addressable dir → unique within the graph's
  namespace), and **preserves** existing ids.
- **Default template** (`org-glance-capture-v2:template`) so captures are
  well-formed (title + tag); `capture-v2` routes its finalize hook through
  `:capture`.
- **Enumeration:** `org-glance-graph-v2:headlines` returns live (non-tombstoned)
  metadata, latest-per-id.
- **Content persistence:** `:put-content` writes the body to
  `…/.org-glance/data/<id…>/data.org`; `:add` persists content for full
  headlines automatically. `:get-content` is the low-level blob reader;
  `:headline` reconstructs the full, **tombstone-aware** headline (nil for
  deleted/unknown ids).
- **Schedule/deadline** moved into the model: private `-schedule`/`-deadline`
  slots hold the parsed timestamp; public `:schedule`/`:deadline` methods return
  raw strings (JSON-safe). The graph's `metadata` just consumes them.

Green: 15 graph+capture tests (incl. content round-trip, tombstone reconstruct,
scheduled round-trip) + 15 v2 headline unit tests.

---

## Phase 1 — Runtime migration: legacy `.metadata.el` → `headlines.jsonl` ✅ DONE

Because org files are canonical and v1 metadata only *points* at them, migration
is a **re-scan**, not a field-by-field translation (we never read the v1
positional serialization nor trust its possibly-stale `begin` pointers):

1. `org-glance-legacy-metadata-files` detects legacy `*.metadata.el` under the
   directory (ignoring already-migrated `*.bak`).
2. `org-glance-migrate-maybe` (called from `org-glance-init`) `display-warning`s +
   `yes-or-no-p` prompts — and is a **no-op when no legacy files exist**, so init
   never blocks in the normal/already-migrated case.
3. `org-glance-migrate` (also `M-x`-able) scans the canonical org files under the
   directory — **skipping the `.org-glance/` store and v1 overview clones** (the
   `mode: org-glance-overview` prop-line marker) so a read-only clone can't
   override its source — ingests only `ORG_GLANCE_ID`-bearing headlines
   (preserving ids), then renames `.metadata.el` → `.metadata.el.bak` (never
   deletes). Returns the count ingested.
4. On no: keep using v1; re-prompt next session, or run `M-x org-glance-migrate`.

**Read strategy:** sources are parsed in a `with-temp-buffer` under
`delay-mode-hooks` — never `find-file`. This suppresses
`after-change-major-mode-hook`, so globalized minor modes (notably
`global-undo-tree-mode`) never activate: no undo recording, no `.~undo-tree~`
files, and no per-file `org-mode-hook` overhead.

**Known follow-up:** re-running `migrate` on an already-migrated dir re-appends
duplicate records (harmless — last-wins keeps reads correct, but grows the log).
`migrate-maybe` self-disables after the first run; the Phase 4 compaction pass
reclaims the growth.

Tests: `test/test-migrate.el` (6) — detection, full migrate (id/title/tags/content
preserved + non-destructive `.bak`), overview clones skipped, id-less headlines
ignored, prompt-confirmed migrate, and the no-legacy no-prompt guard.

---

## Phase 2 — Wire v2 into the commands (coexistence on)

Re-point the interactive commands off v1 onto the v2 graph, behind the
`org-glance-use-graph-v2` defcustom (default off until proven).

### Phase 2.1 — selection + materialize/sync ✅ DONE (`org-glance-material-v2.el`)

- **Flag:** `org-glance-use-graph-v2` (default nil).
- **Selection:** `org-glance-material-v2:completing-read` lists *live* graph
  headlines (`[tags] title`, optional FILTER) and returns the chosen metadata.
- **Materialize:** `org-glance-material-v2:open` opens the content blob
  (`…/data/<id>/data.org`) as a **real file** under `org-glance-material-v2-mode`.
  Because it's a genuine file buffer, **saving is an ordinary `save-buffer`** —
  `C-x C-s` (or evil `:w`, super-save, …) just works, no custom keybinding or
  `write-contents-functions` needed. (Earlier non-file-buffer designs made save
  config-fragile and produced spurious "changed in the graph" prompts.)
- **Sync:** a buffer-local `after-save-hook` (`org-glance-material-v2:sync`)
  appends a fresh metadata record from the saved content (append-only). Emacs's
  own "file changed on disk" handling covers concurrent edits, so there is **no
  custom conflict prompt**. A mismatched/edited `ORG_GLANCE_ID` is a no-op (warns).
- **Command + dispatch:** `M-x org-glance-materialize-v2`; `org-glance:materialize`
  routes here when the flag is on **and** no headline arg was passed — so
  programmatic/link-follow calls stay on v1. Coexistence-safe.
- Tests: `test/test-material.el` (6) — selection, filter, open→edit→apply
  round-trip, conflict prompt (decline aborts / accept overwrites), id guard,
  missing-id error.

### Phase 2.2a — open / extract ✅ DONE

- `org-glance-open-v2` / `org-glance-extract-v2` (+ helpers `:open-link`,
  `:extract` in `org-glance-material-v2.el`) read the stored blob (reconstruct →
  parse links / key-value pairs). `open-link` skips `org-glance-*` internal links
  and rebinds `org-link-frame-setup` like v1.
- `org-glance:materialize` / `:open` / `:extract` dispatch to the v2 command when
  `org-glance-use-graph-v2` is on **and** the headline arg is `nil` (interactive
  selection passes none). Dispatch keys on the **arg**, NOT `called-interactively-p`
  — the latter is opaque to transient's advice wrapper (it returns nil for a
  menu-invoked command, which crashed materialize with `wrong-type-argument … nil`;
  regression-tested). The stale-link edge (link-follow passing an unresolvable id)
  is handled in the **link handlers** (`org-glance-link:materialize`/`:open`),
  which now `user-error` on a nil metadata lookup instead of falling through.
- Tests: 11 in `test/test-material.el` — links (single/multiple/none/internal),
  extract (helper/none/command), **no-arg dispatch for all three commands** (the
  transient regression, proven to fail on the bug), and stale-link erroring.
  42/42 green local (30.2) + container (29.1).

### Phase 2.2b-i — active? filter ✅ DONE

- `org-glance-headline-metadata-v2:active?` / `:done?` derive from the `state`
  field (present in **every** record, including pre-existing ones), so they are
  backward-compatible. The three v2 commands now filter selection to active
  headlines (v1 parity). 43/43 green local + container.

### Phase 2.2b-ii — `linked?` / `propertized?` filter flags ✅ DONE

- Added `linked?` / `propertized?` / `encrypted?` to the metadata projection
  (`graph-v2.el`): computed from the headline's content at add time, serialized
  (`:linked`/`:propertized`/`:encrypted` JSON keys) and deserialized. New
  captures/migrations populate them automatically.
- **Backfill for existing graphs:** `org-glance-graph-v2:reindex` (+ interactive
  `M-x org-glance-reindex`, progress-reported) re-derives metadata for every live
  headline from its stored content, appending fresh flag-bearing records. Needed
  once after upgrading, since pre-existing records lack the flags. ⚠️ **Action:
  run `M-x org-glance-reindex` on your graph** to enable the open/extract filters
  on existing data.
- Filters wired: `open` → active?+linked?, `extract` → active?+(propertized? or
  encrypted?). `:completing-read` now errors helpfully ("No matching headlines —
  run M-x org-glance-reindex…") instead of an empty picker.
- 47/47 green local + container.

### Phase 2.2b — overview / agenda onto the graph ✅ DONE (`org-glance-overview-v2.el`)

- **Graph-backed read-only overview**: one org file under the store, rendered
  from headline *metadata* (no per-headline content parse), one heading per
  matching headline. `org-glance-overview` / `org-glance-agenda-v2` dispatch here.
  Interactively (the `C-x j o` transient action), the overview **prompts for a
  tag** (candidates = the graph's live tags; empty input = all) and filters by
  it — each tag gets its own cache, served while the graph is unchanged.
  Inside an overview buffer, **`/` opens a refinement transient**: narrow the
  current view further by todo state (`s`) or case-insensitive title substring
  (`/`, the new `:title-contains` spec key), or clear all filters (`c`).  Each
  refinement AND-composes onto the buffer's filter (re-filtering a dimension
  replaces it), visits the result non-destructively, and gets/re-uses its own
  cache like any other filter.
- **View coherence — invariant: an overview must never show outdated results.**
  Two mechanisms: (1) *eager surgical push* — a materialized save announces the
  fresh metadata (`org-glance-material-v2:sync-functions`); every open overview
  buffer patches just the affected heading in place (replace / drop /
  full-rebuild-if-newly-matching) and persists the patch to its cache file,
  keeping buffer == cache == render(store); (2) *lazy pull at the display
  boundary* — overview buffers re-check freshness whenever (re)shown or their
  window selected (`window-buffer-change-functions` +
  `window-selection-change-functions`, buffer-local) and rebuild if any other
  mutation (capture/delete/reindex/compact/another Emacs) made them stale.
  Opening a new overview was already coherent via mtime staleness.
- **Filtering**: every entry point takes an optional FILTER — nil (all), a bare
  tag, or a plist spec with keys `:tag`/`:tags`, `:state`, `:done`,
  `:done-keywords`, `:id`/`:title`/`:hash`, `:priority`, `:linked`/`:propertized`/
  `:encrypted`, `:schedule`/`:deadline` (`:present`/`:absent`), plus a `:where`
  predicate escape hatch. `plist-member` gates each clause, so `(:state nil)`
  ("stateless") ≠ omitting `:state`. Unknown keys error. Three pure helpers:
  `:spec-predicate`, `:spec-key`, `:fresh?`.
- **Caching**: unfiltered overview at `<store>/overview.org`; each cacheable
  filter at `<store>/overviews/<key>/overview.org`; an uncacheable `:where` at
  `overviews/transient.org`; the agenda at `overviews/agenda.org`. A fresh cache
  is served via an O(1) mtime check against `headlines.jsonl` — no JSONL read, no
  render. Cache dirs have **compact deterministic names**: the first 12 hex
  chars of the SHA-1 of the canonical filter identity — order-independent,
  filesystem-safe by construction, and identical across machines (no central
  index needed; the name is a pure function of the filter). The prefix is
  lossy; the filter's **identity** (the canonical `prin1` form) lives in a
  `SPEC` sidecar inside each dir — the human-readable record of what the dir
  holds, checked on every cache hit — so an astronomically rare name collision
  between distinct filters rebuilds the overview instead of ever serving the
  wrong one.
- **Done-keywords** are resolved by `org-glance--done-keywords` (reuses Org's own
  `org-done-keywords`, else derives from `org-todo-keywords`); a per-overview
  `:done-keywords` clause overrides it. This also fixed a latent bug: the v2
  material commands' `active?` filter is now correct outside an Org buffer.
- **Keymap (v1 parity)**: `n`/`p` headings, `f`/`b` siblings, `,`/`<`/`.`/`>`
  ends, `TAB` cycle, `RET`/`m` materialize, `o` open, `e` extract, `a` agenda,
  `g` refresh, `q` quit.
- **Reviewed** by an adversarial panel: 8 confirmed issues fixed (cache-key
  collision, tag case-sensitivity, `:done` buffer-locality, stale-buffer reuse on
  re-visit, agenda clobbering the browsed file, `(:tags nil)` cache fragmentation,
  raw error on a deleted headline).
- `org-glance-use-graph-v2` default flipped **on**. `org-glance-migrate-maybe`
  prompt re-enabled. Suite **71/71** local + container.

---

## Phase 3 — Delete / simplify

### Phase 3.1 — v1 deletion ✅ DONE (2026-06-07; −2551/+254 lines)

- **Deleted whole files (6):** `org-glance-headline.el`, `org-glance-metadata.el`,
  `org-glance-overview.el`, `org-glance-material-mode.el`,
  `org-glance-exception.el` (no live expander left),
  `org-glance-namespace.el` (sole consumer was the v1 tag-info struct).
- **Deleted from `org-glance.el`:** the `org-glance`/`org-glance-headlines`
  god-functions + `DB-OUTDATED`, the scope-scan stack, v1 `org-glance-capture`
  + capture-template machinery, the `org-glance-tags` hash family
  (`:tags`/`:tags-sorted`/`tags:completing-read`/`create-tag`/tag dirs),
  `org-glance:@` + `choose-and-apply`, `headline-alias`,
  `org-glance-link:state` + face + registration, the v1 auto-repeat advices,
  and the `org-glance:materialize`/`:open`/`:extract` dispatchers (the v2
  commands are invoked directly now; the `org-glance-use-graph-v2` flag is
  gone — there is nothing to fall back to). `org-glance-init` reduced to
  mkdir + graph init + `migrate-maybe`.
- **Rewired onto v2:** capture's interactive tag prompt (graph-derived
  candidates via new `org-glance-graph-v2:tags`, new tags allowed, no
  registration step); the org link `:follow`/`:complete` handlers
  (`org-glance-visit:`/`org-glance-open:`/`org-glance-overview:` resolve through
  the graph; stale ids/uninitialized state → `user-error`); the `C-x j`
  transient (`org-glance-ui.el`) binds the v2 commands directly and gained a
  `+` capture action.
- **Ported (user-requested), not deleted:** clone-on-repeat + datetime-mode.
  `org-glance-material-v2-mode` enables `org-glance-datetime-mode`;
  `org-glance-clone-on-repeat-p` (now in material-v2) archives the completed
  repetition of a repeated headline as a NEW graph headline (fresh id, repeater
  disarmed, done state preserved) `:before org-auto-repeat-maybe`, then trims
  the live headline to header + `#+begin_pin` blocks `:after`. 4 new tests,
  including an end-to-end `org-todo` repeat round-trip.
- **Support files pruned:** `utils.el` → 5 live helpers (links/key-value
  parsing, encrypt/decrypt region); `tag.el` → predicate + deftype +
  `to-string`. Unused deps dropped (`ht`, `elsa`, `ert-runner`);
  `Package-Requires` now `emacs 29.1, org, aes, dash, f, transient`.
- **Tests:** v1 tests deleted with the stack; `test-headline.el` rewritten to
  the 15 v2 model tests and **enabled in `Eask`** (closing the Phase 4
  "re-enable test-headline" item). Suite **114/114** local (30.2) + container
  (29.1), zero compile warnings.

### Phase 3.2 — drop `-v2` suffixes + `src/` layout ▶️ NEXT

- Drop the `-v2` suffix from every surviving identifier/file (CODESTYLE rule 4:
  the suffix only exists while both versions coexist) and adopt the `src/`
  tree the `refactor` branch started (data layer under `src/data/`) — without
  its `load-relative` dependency if Eask load-path config suffices.
- On-disk formats must not change (JSON keys, cache dir names, SPEC sidecars
  are symbol-independent — pinned by tests).

---

## Phase 4 — Stabilize & test

- **Re-enable `test/test-headline.el`** in `Eask` (currently commented at l.33);
  make it green. Single highest-value stability move.
- **Harden sync against data loss:** atomic write (temp-then-rename) in
  `org-glance-materialized-headline-apply`; distinguish "source changed" from
  "corrupted"; stop swallowing sync-hook errors via `with-demoted-errors`.
- **Namespace / de-stack advice** on `org-auto-repeat-maybe` (re-adds on reload).
- ✅ **Storage maintenance — segmented (LSM-lite) store + GC / compaction** —
  DONE (2026-06-05). The metadata store is no longer one ever-growing file:
  - **Segments:** `meta/headlines.jsonl` is the OPEN append segment; crossing
    `org-glance-graph-v2-segment-max-bytes` (soft cap, default 256 KiB, checked
    after each whole-batch append so records never split) seals it via atomic
    rename into immutable `seg-NNNNNNNNNN.jsonl`. A one-line JSON **MANIFEST**
    (`{version,next-gen,next-seq,segments[oldest-first],open}`) lists the live
    sealed set and is swapped atomically (temp+rename = the sole commit point).
    Records carry a store-global monotonic `seq` (storage ordinal only — never
    metadata; ignored by `deserialize`). Readers merge segments: `get-headline`
    newest-first first-hit; `headlines` oldest→newest, latest-per-id,
    first-sighting order (the insertion-order contract holds across files).
  - **Signal:** `headline-meta-path` still returns `meta/headlines.jsonl`, and
    every mutation (insert/delete/seal/compact) bumps its mtime — the overview
    cache (`overview-v2:fresh?`) needed zero changes.
  - **Compaction** (`M-x org-glance-graph-compact` + auto at
    `org-glance-graph-v2-compact-segment-count` sealed segments, default 4):
    folds ALL segments (sealed + open) into one, drops superseded records and
    dropped tombstones, **content-GCs** `data/<id…>/` blobs of fully-deleted
    ids, commits via one MANIFEST swap **before** truncating the open segment
    (ordering is load-bearing: truncate-first let a crash destroy a tombstone's
    only copy and resurrect a deleted headline — caught by adversarial review,
    regression-tested). No-op on an already-compact store.
  - **Crash recovery:** `--heal` at open adopts an interrupted seal (unlisted
    high-gen segment + empty open) but refuses compaction debris via a
    `seq`-overlap check (compaction copies ordinals; a genuine seal never
    does); torn trailing lines are truncated; orphans (`*.tmp.*`, unlisted
    `seg-*`) reaped. `seq` re-derived from disk (max+1) each open.
  - **Migration:** transparent + idempotent — a legacy single `headlines.jsonl`
    is adopted in place as the open segment by just writing the initial
    MANIFEST (no bytes moved); no-op on empty/converted stores.
  - Tests: `test/test-segments.el` (16) — seal-at-threshold, never-split,
    multi-segment read/order/UTF-8, torn-line + interrupted-seal + crashed-
    compaction recovery (incl. the tombstone-resurrection regression),
    legacy migration, compaction + content GC + no-op + auto-trigger +
    order-preservation, freshness signal. Suite: **87/87** local + container.

---

## Phase 5 — Dev environment ✅ DONE (Podman)

- **`Containerfile`** — `docker.io/silex/emacs:${EMACS_VERSION}` (pinned, default
  29.1, overridable). eask installed via **npm** — deliberately NOT the eask apt
  repo, avoiding its `--allow-insecure-repositories` / unverified GPG key. Deps
  primed from `Eask` in a cached layer; sources copied after; `eask install-deps
  && eask recompile` for the image's Emacs.
- **`.containerignore`** — excludes host `.eask/` + `*.elc` so the container
  compiles deps for ITS own Emacs (no cross-version contamination).
- **Makefile** `podman-build` / `podman-test` / `podman-emacs` (interactive TUI,
  data under `./.podman-data`) / `podman-shell` / `podman-clean`.
  `make podman-test EMACS_VERSION=30.1` to test another version.
- Verified: 31/31 green on Emacs 29.1; interactive `eask emacs` loads org-glance.
  **The E2E run caught a real portability bug** the local (30.2) suite missed —
  a `cl-assert`-based guard whose `cl-assertion-failed` isn't a portable `error`
  subtype in 29.1 (and is optimizable-out); switched to `error`.
- Optional hardening: pin the base image by digest for byte-reproducibility.
- **Skipped:** `flake.nix` / `flake.lock`.
- Local tool: `eask` via `mise` (`npm:@emacs-eask/cli`);
  `mise exec -- eask run command test`.

---

## Appendix A — data formats

**v1 `<tag>/<tag>.metadata.el`** — one `prin1`-ed `hash-table`, `equal` test,
`id (string) → value (list)` where `value` is ordered per
`org-glance-headline:spec`:
`(plain-title begin file commentedp archivedp linked propertized encrypted buffer closed)`.

**v2 store** lives under a hidden `<dir>/.org-glance/` root (kept out of the v1
tag namespace and org-agenda):

- **`<dir>/.org-glance/meta/`** — the segmented (LSM-lite) metadata store:
  - **`MANIFEST`** — one UTF-8 JSON line:
    `{"version":2,"next-gen":N,"next-seq":M,"segments":["seg-…jsonl",…],"open":"headlines.jsonl"}`.
    `segments` lists the live sealed segments oldest-first; swapped atomically
    (temp+rename) — the sole commit point. `next-gen`/`next-seq` are hints; the
    authorities are max `seg-NNN` on disk and max `seq` in live records.
  - **`headlines.jsonl`** — the OPEN append segment (always this name; its mtime
    is the store-change signal). One JSON object per line, appended per batch.
    Live record: `{"id","state","title","tags":[…],"hash","schedule","deadline",
    "priority","linked","propertized","encrypted","seq"}`. Tombstone:
    `{"id","tombstone":true,"seq":…}`. `seq` is a store-global monotonic storage
    ordinal (ignored by `deserialize`; legacy records lack it = ordered by
    position). Sealed into `seg-NNNNNNNNNN.jsonl` (atomic rename) when its size
    crosses `org-glance-graph-v2-segment-max-bytes`.
  - **`seg-NNNNNNNNNN.jsonl`** — immutable sealed segments, append-order,
    10-digit zero-padded generation (lexical = numeric = age order). Unlisted
    seg-* files and `*.tmp.*` are orphans: invisible to readers, reaped at open.
  - **Encoding: always UTF-8** everywhere (writer `f-append-text … 'utf-8`;
    readers bind `coding-system-for-read 'utf-8`). Regression-guarded —
    multibyte titles previously raised `json-utf8-decode-error`.
- **`<dir>/.org-glance/data/<id[0:2]>/<id[2:]>/data.org`** — the headline content
  blob (full subtree text). Ids ≤ 2 chars are stored flat (no shard). Written by
  `:put-content`; read by `:get-content` / reconstructed by `:headline`.

## Appendix B — `refactor` branch: harvest list

- ✅ Take: `src/data/org-glance-directory.el` (a real `org-glance-directory`
  type + predicate), the Dockerfile, the `-v2`-suffix removal + `src/data/`
  layout (Phase 3).
- ❌ Leave: the wholesale `org-glance-graph.el` (same core bugs + new
  regressions), `flake.nix`/`flake.lock`.
