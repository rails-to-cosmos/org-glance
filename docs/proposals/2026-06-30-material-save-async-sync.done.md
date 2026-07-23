# Fast material saves: WAL-commit on the hot path, views self-sync off it

**Status:** **DONE** — shipped 2026-06-30. `org-glance-material:sync` no longer fans out to
views; the save hot path is blob + WAL, views pull at their display boundary, and a new
`org-glance-view.el` owns view coherence for every view type. The only follow-up left is the
optional Phase 3 (in-place reparse, §"Secondary win").

> **Implementation note (2026-06-30, as built).** Shipped as designed; the as-built shape went a
> step deeper than §Design and was then tightened by a `/simplify` pass. This note is
> authoritative over the exploration sections below (esp. §"What gets added" and §Staging, which
> describe the lighter being fed by an optional idle tick — that option was dropped):
> - **Coherence is OWNED by `org-glance-view.el`, both halves, once.** PUSH —
>   `org-glance-view:mark-graph-stale` flips a boolean on each open view of the graph (no I/O, no
>   rebuild, no point movement), so the save hot path stays "blob + WAL" and the `glance:stale`
>   lighter shows immediately, with no global idle-timer lifecycle. PULL —
>   `org-glance-view:register` (a `&key` call taking `:stale-fn`/`:reload-fn`) stores the two
>   view-specific thunks buffer-local, installs the `window-buffer-change` /
>   `window-selection-change` hooks, and one shared `org-glance-view--refresh-when-stale`
>   dispatches to them. Each view (overview, table) supplies only its STALE-FN + RELOAD-FN; the
>   window wiring, the data-loss guard and the error demotion live once — a new view type adds
>   only a `register` call.
> - **The data-loss guard is `(and buffer-file-name (buffer-modified-p))`, centralized** in the
>   shared refresh. A modified FILE-backed view (the overview cache) is never reverted — only
>   re-flagged; a NON-FILE projection (the table, always "modified" after its programmatic fill,
>   holding no user data) reloads freely. (Generalizes the original "two deltas": push-not-poll,
>   and file-backed-only guard.)
> - **The lighter is the native mode-line construct:**
>   `(cl-pushnew '(org-glance-view--stale " glance:stale") mode-line-misc-info :test #'equal)` —
>   a buffer-local variable read at redisplay, with no per-buffer funcall.
> - **Verified:** 174/174 tests green, clean compile (the surgical-push subsystem and the
>   `sync-functions` hook are deleted; the table's redundant `--graph` buffer-local is gone).

## Problem

Saving a materialized headline (`C-x C-s`) does far more than persist the edit. The
`after-save-hook` (`org-glance-material:sync`) runs a chain whose tail scales with how
many views are open and how big they are — all on the user's keystroke:

| # | step | cost | must be synchronous? |
|---|------|------|----------------------|
| 1 | blob write `data/<id>/data.org` (atomic temp+rename) | one write, ∝ blob | **yes** — the durable content commit |
| 2 | reparse buffer via `--from-string` (org-mode in a temp buffer, cycle bound) | CPU ∝ blob | yes (cheap-ish) |
| 3 | `graph:insert` → `--append`: WAL line + `--ensure-newline-terminated` + `--maybe-seal` (stat) + `--maybe-compact` (read MANIFEST) | a few small file ops | yes — makes the change queryable |
| 4 | `--invalidate-cache` | O(1) drop (next read rebuilds) | yes |
| 5a | **overview fan-out** (`sync-functions`): for **each open overview buffer** → predicate + region search + edit + **`write-region` of the ENTIRE cache file** | **O(view-size × open-overviews) bytes, per save** | **no** |
| 5b | table fan-out: for each open table buffer → in-memory row upsert | O(open-tables), no disk | no |

Steps 1–4 are bounded by the edited headline. **Step 5a is the scalability cliff**: every
save rewrites each open overview's whole cache file to disk. Open three windows onto a
4 000-headline overview and a one-character save triggers three 4 000-line full-file
rewrites — on the hot path, synchronously, before the cursor returns. That is the slowness.

The eager surgical push was added (migration plan, "EAGER, SURGICAL push") to keep visible
buffers byte-fresh. It made the *patch* incremental but still pays a *full-file write* per
open overview per save — the incremental part never reached disk I/O.

## The contract: what must actually be synchronous

The blob (`data/<id>/data.org`) is the **canonical content**; the JSONL metadata is a
**projection** of it (`reindex` rebuilds metadata from blobs). So the durability floor is
just **step 1**: once the blob is on disk, the edit survives a crash.

Steps 2–4 are the *index commit* — they make the new state queryable by overviews / tables /
search / agenda. They are cheap (no full-file rewrites) and we keep them synchronous so
"save, then search, and it's there" holds. The user's framing — *"the change goes to the WAL
(headlines.jsonl)"* on save — is exactly steps 1–4.

Step 5 is **view materialization**, not durability and not the index. It belongs off the hot
path: *"other buffers track changes and synchronise asynchronously / update-on-read."*

## Design

**Hot path shrinks to the index commit.** `org-glance-material:sync` keeps steps 1–4 and
**drops the `run-hook-with-args 'sync-functions` fan-out**. Nothing per-view runs on save.

**Views become pull-based ("update on read"): mark stale, the user resolves.**
Two facts make this almost free:

1. Step 3's WAL append bumps `headlines.jsonl`'s mtime, so **every overview cache file and
   table fill is now automatically `stale?`** against the store. The freshness check that
   already gates the lazy path does the right thing with zero new bookkeeping.
2. The lazy refresh net already exists: `--refresh-when-stale` is on
   `window-buffer-change-functions` / `window-selection-change-functions`, so a view rebuilds
   the moment its window is (re)displayed or selected — i.e. exactly when the user *arrives* at
   it, where point/scroll are being re-established anyway.

What's missing is the *visible-but-not-selected* window (an overview sitting next to the
material buffer): post-save it shows stale rows until the user returns to it. The wrong fix is
to revert it from under the user — even though overviews/tables are read-only (no *data* is
lost), silently reverting a buffer the user is reading **jumps point, drops scroll position and
collapses folds**, and the existing revert is `(revert-buffer t t t)` — `noconfirm`, safe only
by the read-only invariant, never asserted. So instead:

- **Mark, don't revert.** A cheap mode-line lighter (e.g. `glance:stale`) flags a view whose
  cache predates the store. The staleness check is the O(1) mtime compare already in `stale?`;
  drive the lighter from it (on the hooks above, or a *lighter-only* idle tick that never
  touches buffer contents). The user refreshes when ready — `g` already rebuilds, or simply
  reselecting/redisplaying the window does (the safe boundary).
- **Never rebuild a modified buffer.** Guard every refresh path with `unless
  (buffer-modified-p)` → fall back to marking. Read-only views are never modified, so this is a
  no-op for them today; it makes the safety *explicit* and is the correct behaviour the day any
  read-write view exists (see "Read-write buffers" below).

Net save path: **blob write + one JSON append + O(1) invalidate.** No view writes. No
full-file rewrites. The expensive full re-render happens at most once per "glance" — when the
user looks at a stale view — never on the hot path, never out from under them.

### Read-write buffers (the data-loss question)

This whole mechanism touches **only read-only view buffers** (overview = `read-only-mode`,
table = `special-mode`). The editable **material** buffer is a plain file buffer org-glance
*never* auto-reverts — its blob is rewritten only by its **own** save (a clone-on-repeat mints a
fresh id, it does not overwrite). A concurrent overwrite (a second Emacs / a second material
buffer on the same id) is caught by Emacs's native "file changed on disk" prompt on save, as
today. So **no unsaved edit is ever discarded** — this proposal changes nothing about material
blob writes, and the `buffer-modified-p` guard above means even a future store-stale-aware
material buffer would *mark + let the user resolve* (reload / keep-and-overwrite / diff), never
silently revert.

### What gets deleted

- `org-glance-overview:on-headline-update`, `--patch-headline`, `--save-quietly`, and
  `--heading-region` (only the patch uses it) — the whole surgical-overview-write subsystem.
- `org-glance-table:on-headline-update` and `org-glance-table--patch` (live push half; the
  buffer-local done-keywords + `--refresh-when-stale` rebuild path stay).
- `org-glance-material:sync-functions` (the abnormal hook) and its `run-hook-with-args` call —
  no subscribers remain. Cheap to reintroduce if a real post-save extension point is ever
  wanted; YAGNI until then.

### What gets added

- A **staleness lighter** for view buffers: a buffer-local flag set from the existing `stale?`
  mtime compare, surfaced in the mode line (e.g. `glance:stale`). Updated on the hooks the lazy
  net already uses, plus optionally a *lighter-only* idle tick that walks displayed view windows
  and **only** toggles the flag — it never touches buffer contents, so it cannot move point or
  clobber anything. The user resolves with `g` (rebuild) or by reselecting the window.
- A `unless (buffer-modified-p)` guard on every refresh/revert path (`--refresh-when-stale`, the
  `g` command), so a modified buffer is marked stale rather than reverted. A no-op for today's
  read-only views; the safety net for any future read-write view.

## Options considered

1. **Pure pull + staleness lighter (chosen).** Delete the push; the lazy net on display/selection
   refreshes a view when the user arrives at it, and the lighter flags a visible-but-unfocused
   view as stale without reverting it. Smallest machinery, never disrupts a buffer the user is
   reading, never risks an edit.
2. **Idle coalescer that auto-reverts visible stale views.** Refreshes visible non-selected
   windows on an idle tick without user action. Rejected: reverting a buffer the user is reading
   jumps point / scroll / folds (no *data* loss for read-only views, but a real UX regression),
   and the `noconfirm` revert is safe only by the read-only invariant — a latent footgun. The
   lighter delivers the same "you're looking at stale data" signal without the disruption.
3. **Deferred push via idle timer (keep surgical patch, coalesced).** Keeps the incremental
   in-memory patch and re-persists once per idle tick. More machinery (pending-id set, dedup,
   buffer-kill cleanup) and *still* writes the overview cache file (just coalesced). The pull
   model gets the same coalescing for less code, because freshness-by-mtime is free.
4. **Memory-only patch, persist lazily.** Patch open buffers in RAM on save (cheap) but skip
   the disk write; rebuild-from-disk is what the lazy guard would do. Rejected: after an
   un-persisted patch the buffer's visited-modtime is older than the WAL, so `stale?` is true
   and the next display *clobbers the patch with a full rebuild from the stale file* — needs a
   buffer-local "fresh-in-memory through seq N" marker to suppress, i.e. exactly the bookkeeping
   we're trying to delete.
5. **Defer the index commit too (steps 2–4 async).** Save writes only the blob; an idle tick
   reparses + appends to the WAL. Maximises hot-path speed but breaks "save then immediately
   search finds it" and widens the window where the index disagrees with content. Out of scope
   / behind a flag at most; the blob is always the durable truth, so it's *safe*, just
   surprising. Not the default.

**Chosen: option 1** — pure pull, plus a non-reverting staleness lighter.

## Secondary win (optional, same PR or follow-up)

Step 2 stringifies the buffer and re-parses it in a *fresh* temp buffer
(`--from-string`) purely so the per-tag todo cycle can be `let`-bound globally (the W1 fix).
The save buffer is already an org buffer holding exactly one headline — we can re-run the
element parse **in place** under the same `let ((org-todo-keywords …))`, skipping the string
copy + temp-buffer org-mode spin-up. Pure CPU win on the hot path, no behaviour change. Verify
the cycle binding still reaches the in-buffer parse before relying on it.

## Staging

- **Phase 1 — shrink the hot path.** Delete the `sync-functions` fan-out and its two
  subscribers + the overview surgical-write helpers. Lean on the existing lazy net. Saves are
  now blob + WAL only. (This alone removes the cliff.) Add the `buffer-modified-p` guard to the
  refresh paths in the same pass.
- **Phase 2 — staleness lighter.** Surface `stale?` in the mode line so a visible-but-unfocused
  view advertises that it's behind the store; the user refreshes with `g` or by reselecting.
- **Phase 3 (optional) — in-place reparse** for step 2's CPU.

## Risk

Low–medium. Correctness of "no view shows stale results" already rests on the
mtime-freshness check, which the WAL append keeps feeding — we're *removing* an optimization,
not the invariant. **No data-loss path:** the machinery touches only read-only views, never
auto-reverts a modified buffer (the `buffer-modified-p` guard), and does not change material
blob writes — concurrent overwrites stay on Emacs's native conflict prompt. The visible
regressions to watch:

- A displayed-but-unfocused overview/table shows stale rows (flagged `glance:stale`) until the
  user refreshes (`g`) or reselects it — a deliberate trade of automatic freshness for never
  disturbing a buffer mid-read. Far cheaper than per-save full-file rewrites.
- The lazy rebuild is a full re-render (re-reads `:headlines`, re-renders, rewrites the cache
  once) vs the old O(1) patch — but it runs at most once per glance, only for the view the user
  looks at; under bursty editing (the slow case) it is strictly less total work.

## Test impact

- **Rewrite/retarget** `org-glance-test:overview-live-update-on-save` and
  `…-live-update-newly-matching` (test/test-overview.el:430,463): they assert the *eager push*
  (post-`sync` the open buffer is already patched + persisted). Under pull, post-`sync` the
  buffer is `stale?` and refreshes on next display — so the assertions move to "after
  `--refresh-when-stale` the buffer reflects the change," mirroring the existing
  `…-stale-buffer-refreshes-on-display` (test-overview.el:481).
- **Drop** the table live-patch tests that call `org-glance-table--patch` directly
  (test/test-table.el:127,152) or re-point them at `--refresh-when-stale`.
- **Add** a scaling guard: a save with N open large overviews performs **zero** overview-cache
  writes on the hot path (assert their cache-file mtimes are unchanged across the `sync`).
- **Add** a no-data-loss guard: `--refresh-when-stale` on a `buffer-modified-p` buffer leaves it
  untouched (marks stale, does not revert) — the regression test for the `buffer-modified-p`
  guard.
- The surgical `table-view--patch-line` row machinery (table-view.el) is unaffected — it backs
  the lazy rebuild's single-row upserts, not the deleted push.
