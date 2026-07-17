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
