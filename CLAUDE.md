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
   metadata fields at the end only (row order = JSON key order).
5. Blobs are canonical; indexes are derived and rebuildable; metadata computes
   before any write, blob lands before its WAL record. The property index is a
   pure cache — hash-guarded with O(N) blob fallback, dropped by reindex; never
   trust it in a durable write. Relation edges: the `org-glance-material:` body
   link is canonical; `relations` metadata is a projection, never written
   independently.
6. Ids are path-safety-checked via `error` (never `cl-assert`) before any
   filesystem use.
7. Single-user, no locking; staleness detection uses the full store snapshot
   (mtime + size + segment names), never mtime alone.
8. Git conflicts heal by union merge; `.eld` sidecar merges are commutative and
   non-inflating (earliest/latest/`max`, never a sum).
9. Side-index hooks and view refresh are error-demoted — they may never break
   a save, an open, or a display.
10. View coherence is flag-stale + pull-refresh; when freshness is in doubt,
    rebuild.
11. Never clobber unsaved user edits: `user-error` or skip, never overwrite.
12. Store content parses in temp buffers via `org-glance--org-mode`
    (`delay-mode-hooks`, `tab-width` 8); never `find-file` sources to read.
13. Tags are canonical downcased interned symbols at the boundary; deserialized
    metadata carries STRING tags — coerce with `(format "%s" tag)`. Relation
    kinds are canonical dash-slugs at every boundary (encode/decode/deserialize);
    spaced form is display-only.
14. Crypt: plaintext never touches disk; `#+begin_crypt` markers are the
    persistent secrecy annotation. Secrecy is per-block — text between blocks
    stays plaintext and indexed, even for an encrypted headline.
15. Table Title column is mandatory — never removable/hideable (`--act-delcolumn`
    refuses it; `--apply-schema` strips it from the hidden set).
16. Per-tag column schema: `:hidden` is a denylist of removed built-ins (new
    built-ins still appear); `C-u +` candidates exclude `ORG_GLANCE_*` + CATEGORY;
    order + sort persist separately per filter.
17. Transient filters (`:where`, `:refers-to`, `:id-any` — table-flagged,
    judged by `org-glance-filter:transient?`) never persist per-filter state:
    no overview cache, no table config, no column schema.
18. Reserved properties (`org-glance-material-hidden-properties`) are managed
    keys: concealed in material buffers, hand edits reverted on save with a
    warning; the revert touches only the heading drawer (disjoint from the
    crypt seal).

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
