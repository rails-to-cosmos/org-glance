# org-glance — working conventions

## Documentation is org-mode, not markdown

All documentation authored in this repo is written in **org-mode** (`.org`),
not markdown. Cross-reference documents with **org-links**, e.g.
`[[file:2026-07-06-performance-audit.org][Performance audit]]` or a section
anchor `[[file:foo.org::*Design][Design]]`. Proposals live in
`docs/proposals/` and follow the naming `YYYY-MM-DD-slug.proposed.org` →
renamed to `…​.done.org` when implemented. (Older proposals are `.md`; new
ones are `.org`.) The changelog is `CHANGELOG.org` — org, like everything else.

Entry point for the current performance work:
[[file:docs/proposals/2026-07-06-performance-audit.org][docs/proposals/2026-07-06-performance-audit.org]].

## Keep doc references live

Code moves; docs that cite it go stale. After changing code, grep the docs
(`docs/`, the proposals) for references to anything you touched — line numbers,
function/variable names, "current"/"done" claims — and update them in the same
pass. A proposal citing `foo.el:365` after a refactor silently misleads the next
reader.

## Keep the CHANGELOG current

`CHANGELOG.org` is the running summary of the project's actual, user-facing
state (Keep a Changelog structure, in org; date-based versions). After
implementing a feature or any user-visible change, add a bullet under the
`* Unreleased` heading in the **same pass** — grouped under `** Added` /
`** Changed` / `** Fixed` / `** Removed`. On a version bump, rename `Unreleased`
to the new version and open a fresh `* Unreleased`. A user-visible change
landing without a changelog entry is an incomplete change; internal-only
refactors (no behaviour change) need no entry.

Keep it **compact — facts only**: one line per bullet, naming the command, key,
or file that changed. No rationale, no prose; that belongs in `docs/proposals/`.

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

- **Naming:** public API uses `:` (`org-glance-headline:metadata`); private
  helpers use `--` (`org-glance-graph--append`) — never `:--`.
- **Docstrings:** keep every line ≤ 80 columns (byte-compile warns otherwise).
- **Rich args:** functions taking several lambda/same-typed arguments use
  `cl-defun … &key`, not positional.
- **`table-view` is an external ELPA dependency**: declared in `Eask` via a
  `(source "rails-to-cosmos" "https://rails-to-cosmos.github.io/elpa/")` archive
  and a plain `(depends-on "table-view")`, plus the `Package-Requires` header.
  Generic table capabilities belong upstream at `~/sync/stuff/table-view`
  (commit + push, then publish to the ELPA); every org-glance adaptation lives
  in `src/view/org-glance-table.el`.
- **`agnostic-llm` is a required dependency** (powers `org-glance-llm`, the `l`
  action): declared in `Package-Requires` and via `(depends-on "agnostic-llm")`
  in `Eask` (from the same `rails-to-cosmos` ELPA). It pulls in `vterm` (a native
  module whose compile prompt breaks batch tooling), so the code loads it lazily
  through its autoload — a `declare-function` plus a direct call in
  `org-glance-llm`, never a load-time `require` — so byte-compile and headless
  test runs never load or build vterm. Upstream lives at `~/sync/stuff/agnostic-llm`.

## Build / test

```sh
eask recompile        # must compile warning-free
eask run command test # the ERT suite
```
