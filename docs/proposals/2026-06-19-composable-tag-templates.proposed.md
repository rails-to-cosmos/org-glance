# Composable tag templates: tags as Haskell-style type classes

**Status:** Phase 1 **SHIPPED** — implemented 2026-06-22 as a *separate tag-config store*
(not §1's reserved `:class:` tag), capture path verified end-to-end 2026-06-30 (see the
implementation-update note below and "Implementation status" at the end) · Phases 2–3 still
**proposed** · original design exploration 2026-06-19 (5 approaches → 3 adversarial judges
→ synthesis)

> **Implementation update (2026-06-22) — supersedes §1's `:class:`-tag mechanism.**
> The original Phase-1 design marked a class prototype with a RESERVED `:class:`
> tag. That coupled config to the content tag namespace: it reserved a common
> word, surfaced `class` in `org-glance-graph:tags` / the capture picker, and let
> a config headline masquerade as content. **Replaced by a separate, optional
> tag-CONFIG store** (`org-glance-tag-config.el`):
> - **Location:** one hand-edited Org file at `<dir>/.org-glance/config/tags.org`
>   (inside the dot-store, so it is invisible to the content-migration scan and
>   org-agenda for free). `M-x org-glance-tag-config-edit` opens it.
> - **Schema:** one level-1 headline per tag, keyed by a `:TAG:` drawer property
>   (the headline carries NO org tags, so it can never pollute discovery). Body =
>   capture skeleton; drawer keys `:REQUIRES:` / `:VERSION:` (was `CLASS_*`).
> - **Per-tag TODO keywords:** a `:TODO_KEYWORDS:` drawer property holds a verbatim
>   `#+TODO:`-style cycle (e.g. `TODO READING | READ ABANDONED`). NB the property is
>   `TODO_KEYWORDS`, not `TODO` — org reserves `TODO` as the special todo-STATE
>   property, so `org-entry-get` would return the heading's keyword, not the drawer
>   value. The cycle is applied as a `#+TODO:` FILE keyword in the rendered overview
>   header (org then cycles + faces those states natively, surviving reopen-from-
>   cache, and the agenda inherits it since it shares `:render`) and prepended to the
>   capture template; the cycle's done-set is bound while the `:done`/`active?`
>   predicate is built, so selection is per-tag-correct with no spec/cache-key change.
>   Editing `tags.org` invalidates overview caches (its mtime joins `:fresh?`).
> - **Optional / degrading:** absent file / `:TAG:` / dimension → nil → the default
>   template + global keywords, byte-for-byte as before.
> - **Multi-tag:** a `#+TODO:` header is a single cycle; exactly one configured cycle
>   among the filtered tags is applied, 0 or >1 distinct fall back to global (merging
>   sequences is order-sensitive). Cycle MERGING is deferred with the rest of Phase 2.
> Tags themselves stay discovered from content; only their config moved out.

## Motivation

A tag is a bare downcased symbol (`org-glance-tag.el`): no metadata, no behavior,
discovered (never declared) from captured headlines. Capture templating is one
trivial line — `org-glance-capture:template` returns `"* TITLE%?  :tag1:tag2:"`.

We want tags to behave like **type classes** and **compose**: capturing a headline
tagged `travel baby pets` should produce the *composition* of each class's
contribution — i.e. Haskell's `(Travel a, Baby a, Pets a) => a`. The tag list is a
constraint set; the captured artifact is the specialized "instance".

## Design (synthesis)

### 1. A tag-class is an org-native prototype headline (no registration)

A class is just a captured headline tagged `:class:` whose **title names the class**,
whose **body is the skeleton template**, and whose **drawer carries class metadata**:

```org
* travel  :travel:class:
:PROPERTIES:
:CLASS_REQUIRES: location
:CLASS_VERSION: 1
:LOCATION:  %^{Destination}
:DEPARTS:   %^{Departs}t
:END:
*** Itinerary
    %?
```

Classes are discovered by the *same* machinery as tags (`org-glance-graph:headlines`
filtered by `:class:`), defined by the *same* act (capture), and "latest wins" falls
out of the append-only log. This keeps "**discovered, not registered**" true of
classes too: zero new store, zero new struct slots required on `org-glance-headline`,
self-hosting (classes are browsable/overviewable/diffable like any headline).

### 2. The elisp class value is a rebuildable projection/cache

`org-glance-tag-class` is an immutable cl-struct projected *from* the `:class:`
prototypes, built with the lazy triad (`:-table` attr / `--table` builder /
`:table` accessor) on graph load — never a source of truth. Power users get O(1)
lookup; the org `:class:` headlines stay canonical and the cache is reconstructable,
exactly as the graph caches headlines from org files. This dissolves the
"registry vs no-registration" tension.

### 3. Composition: factor the algebra from the render

The merged class is an **order-free, commutative product-of-submonoids** — never a
string until render:

| channel        | carrier                | merge `<>`                              | identity |
|----------------|------------------------|------------------------------------------|----------|
| `requires`     | set of tag symbols     | union (idempotent, commutative)          | ∅        |
| `fields`       | tag-namespaced alist   | keyed union; clash → §4 lattice          | ∅        |
| `prompts`      | name→spec plist        | keyed union; clash → §4 lattice          | ∅        |
| `body`         | set of keyed sections  | keyed union (dedup by section key)       | ∅        |

A product of monoids is a monoid, so `org-glance-tag-class:merge` is associative,
commutative and idempotent **by construction** — `travel+baby ≡ baby+travel`
*structurally*, and the diamond-superclass case (`travel` and `pets` both requiring
`location`) collapses for free. A **canonical total order is imposed only at render**
(`(phase, priority, tag-name)`), which is where org-capture's positional grammar is
honored — keeping order-independence a property of the value, not a render hack.

### 4. Conflicts: a join-semilattice, not last-writer-wins

Fields are **namespaced by their defining tag**, so the common case is conflict-free.
A field two classes both declare `:shared` is merged per-attribute as a least-upper-bound:
`type` unifies to the more-specific, `required? = OR`, `validator = AND`, and
`default`/`prompt` pick by a strategy lattice (`:const > :demand > :prompt > :default`).
This is monotone ("merging instances only strengthens constraints") — the genuine
type-class semantic. A `org-glance-tag-class-strict` defcustom turns an unequal clash
on a non-`:shared` field from resolve-and-record into a `user-error` (overlapping-
instance rejection).

### 5. Degradation is the identity element

A tag with no `:class:` prototype resolves to `org-glance-tag-class:empty`, whose
render delegates to today's `org-glance-capture:template`. A bare tag therefore
captures **byte-identically to today** — no `if typed` branch, `test-capture.el`
passes unmodified.

## Two implementation constraints (verified against the code)

1. **There is no `headline→headline` finalize seam.** `org-glance-graph:capture`
   takes a *buffer* and re-parses fresh headlines (`org-glance-graph:capture-buffer`);
   the `org-capture-after-finalize-hook` in `org-glance-capture.el` reads the file
   buffer, not a struct. All validation / property-fill / `finalize` MUST mutate the
   **buffer** in `org-capture-before-finalize-hook` (where a `user-error` aborts the
   capture). Any design threading a composed struct-transform "just before ingest" is
   building on air.

2. **org-capture's template string is the binding constraint.** Exactly one `%?`,
   `%^{…}` prompts fire in *text order*, positional `%\N` backrefs. The renderer
   (`org-glance-tag-class:render`) owns the canonical ordering and may push prompt /
   property-drawer work into `org-capture-mode-hook` rather than cram a rich composite
   into a single template string.

## Staging (deliberately minimal first)

The live store has **zero multi-tag headlines** and one bare `travel`. The merge
algebra is, today, designed for a workflow that does not yet exist — so build the
least until composition is actually exercised:

- **Phase 1 — single typed tag (ships value, no merge):** `completing-read` a tag;
  if a `:class:` prototype exists, capture by clone+fill of that one prototype
  (body + drawer + `%^{}` prompts); else today's behavior. Delivers the
  definition surface and `:CLASS_REQUIRES` resolution for one tag.
- **Phase 2 — composition:** plural `org-glance-capture:completing-read-tags`, the
  product-of-submonoids merge (§3), the conflict lattice (§4), strict mode.
- **Phase 3 (optional) — classes govern more than capture:** overview rendering,
  on-edit validation (`org-glance-material`), agenda projection.

## Elisp API sketch (per CODESTYLE)

```elisp
;; src/data/org-glance-tag-class.el  (projection layer)
(cl-defstruct (org-glance-tag-class (:predicate org-glance-tag-class?)
                                    (:conc-name org-glance-tag-class:))
  (tag nil :read-only t)            ; defining tag symbol
  (requires nil :read-only t)       ; superclass tag symbols
  (fields nil :read-only t)         ; alist: (tag . org-glance-tag-field)
  (prompts nil :read-only t)
  (body nil :read-only t)           ; alist of (section-key . org-string)
  (priority 0 :read-only t))

(cl-defun org-glance-tag-class:prototype (graph tag) ...)   ; latest :class: headline for TAG
(cl-defun org-glance-tag-class--from-prototype (headline) ...) ; builder
(cl-defun org-glance-tag-class:empty () ...)                ; identity
(cl-defun org-glance-tag-class:merge (a b) ...)             ; commutative monoid <>
(cl-defun org-glance-tag-class--closure (graph tags) ...)   ; transitive CLASS_REQUIRES
(cl-defun org-glance-tag-class:compose (graph tags)         ; fold closure under merge
  (cl-reduce #'org-glance-tag-class:merge
             (org-glance-tag-class--closure graph tags)
             :initial-value (org-glance-tag-class:empty)))
(cl-defun org-glance-tag-class:render (class title) ...)    ; -> org-capture template string
(cl-defun org-glance-tag-class:finalize-buffer (class) ...) ; runs in before-finalize-hook

;; src/view/org-glance-capture.el  (integration; degrade when empty)
(cl-defun org-glance-capture:template (tags &optional (title ""))
  (let ((class (org-glance-tag-class:compose org-glance-graph (org-glance--as-list tags))))
    (if (org-glance-tag-class:empty? class)
        (format "* %s%%?  :%s:" title (org-glance-capture--format-tags tags))  ; today
      (org-glance-tag-class:render class title))))
```

Reuse `org-glance-headline--copy`'s `cl-struct-slot-info` reflection loop for any COW
helper rather than hand-writing copiers.

## Alternatives considered

1. **Dictionaries desugared at capture time** (`deftag-class` → global table) — exact
   GHC analogy, but introduces a real registry and recovers determinism via a sort over
   non-commutative concat (inferior to §3's structural order-freedom).
2. **Fragments as product-of-submonoids** (`org-glance-fragment` `<>`) — the algebra
   that §3 adopts, but defined in elisp (`deftag`), so a class only exists if its elisp
   is loaded (the philosophy gap §1 closes by putting definitions in org).
3. **Capture pipeline of session transformers** — cleanest fold, and source of the
   `org-capture-before-finalize-hook` correction above; over-general until §4 exists.
4. **Prototype headlines as classes** — adopted as the definition surface (§1).
5. **Row-typed schemas** — adopted as the field/conflict model (§4), minus its elisp
   schema registry (org `:class:` headlines replace it).

The synthesis = **4's definition surface + 2/5's algebra & conflict lattice + 3's
fold and its hook correction**, staged per the over-engineering guard.

## Risk

Low to start (Phase 1 is additive and degrades to today's exact output; `test-capture.el`
is the guardrail). The real risk is in the renderer meeting org-capture's single-pass
template grammar (constraint #2) and is contained to `org-glance-tag-class:render`.
Phase 2's merge laws want property-based tests (associativity/commutativity/idempotence
of `:merge`, identity of `:empty`).

## Implementation status (2026-06-23)

**Phase 1 — DONE**, but built as a *separate, optional tag-config store* rather than
the reserved `:class:` tag of §1 (the implementation-update note at the top has the full
rationale: the `:class:` tag reserved a common word and polluted tag discovery). The
algebra (§3) and conflict lattice (§4) are unchanged as the design of record for Phase 2.

- **Definition surface** — `<dir>/.org-glance/config/tags.org`: one `:TAG:`-keyed
  level-1 headline per tag (carrying NO org tags, so it stays out of discovery), holding
  the capture skeleton + `:REQUIRES:` / `:VERSION:` / `:TODO_KEYWORDS:`. Authored via
  `M-x org-glance-tag-config-edit`. Discovered + cached (mtime+size) like the graph;
  absent file / `:TAG:` / dimension degrades to the default — `test-capture.el` unmodified.

- **As-built API** (`src/data/org-glance-tag-config.el`) vs. the §"Elisp API sketch":

  | proposed (`org-glance-tag-class:`)        | as built (`org-glance-tag-config:`)                         |
  |-------------------------------------------|-------------------------------------------------------------|
  | `:prototype` (latest `:class:` headline)  | `:resolve` (graph + tag → config from `tags.org`)           |
  | `:empty` / `:empty?` (struct identity)    | a nil `:resolve` IS the identity (no struct needed in P1)   |
  | `:render` (class title)                   | `:render` (config title tags)                               |
  | `:compose` / `:merge` / `--closure`       | **deferred to Phase 2** — no config keys are parsed for it yet (the `:requires`/`:version` scaffolding was removed; re-add when the algebra is built) |

- **Beyond the original scope — per-tag TODO keywords** (added during implementation):
  a `:TODO_KEYWORDS:` drawer property holds a verbatim `#+TODO:` cycle (e.g.
  `TODO READING | READ ABANDONED`). It is emitted as a `#+TODO:` *file keyword* in the
  rendered overview/agenda (so org cycles + faces those states natively, surviving
  reopen-from-cache) and prepended to the capture template; the cycle's done-set drives
  the `:done`/`active?` filter at render. The **table view** binds the same done-set for
  its active/done badge split, and **materialize→save** installs the cycle so an edited
  state like `READING` round-trips instead of folding into the title (regression-tested).
  NB the property is `TODO_KEYWORDS`, **not** `TODO` — org reserves `TODO` as the special
  todo-STATE property. Editing `tags.org` invalidates overview caches (its mtime joins the
  overview freshness check).

- **Capture path verified end-to-end (2026-06-30).** Prepending the `#+TODO:` cycle to the
  rendered template broke `org-capture`: type `entry` requires the template to *start* with a
  heading, so a leading file keyword raised "Template is not a valid Org entry or tree".
  `org-glance-capture` now splits the template into (preamble · entry) — the `#+TODO:`
  preamble is written to the capture target file first, and only the bare entry becomes the
  org-capture template. (The splitter keys on `string-prefix-p "*"`, NOT a `^\\*` regexp:
  Emacs `^` matches every line start, which had silently mis-detected the split point.) A
  latent bug surfaced alongside: metadata `:tags` were serialized to JSON as a vector but
  never decoded back to a list, so `member`/list ops on `org-glance-headline-metadata:tags`
  failed for any cache-loaded headline — now decoded to a list of strings. Both are covered
  by new end-to-end capture tests (`org-glance-test:tag-config-capture-book`,
  `…-capture-unconfigured`, `…-split-preamble` in `test-tag-config.el`): a real
  `org-glance-capture → org-capture → finalize → ingest` round-trip, not just `:render`.

**Phases 2–3 — still proposed** (§3–§5 stand as the algebra of record), but a 2026-06-23
adversarial review reframed the scope:

- **Reframe: "the inference gap", not "type classes".** There is exactly ONE fact the
  system cannot infer — a per-tag cycle's *order* and its active|done partition (the `|`).
  `org-glance-graph:states` already derives the *set* of used states from content, but a
  set has no order and no done-boundary, so it cannot drive cycling/faces or `:done`. That
  single un-inferable fact is what legitimately justifies an opt-in declaration. The honest
  framing of "discovered, not registered" (§Motivation): a tag's *existence* stays
  discovered; its TODO *semantics* require a small opt-in because they cannot be inferred.
  Inference of the partition (states ending in a marker, etc.) was considered and rejected
  — `DELEGATED`/`ABANDONED` are genuinely ambiguous and silent-misfiltering is the failure
  mode the overview-freshness machinery exists to avoid.

- **Decision gate for the merge algebra.** Do NOT implement `:merge` / the conflict lattice
  until there are real multi-tag headlines that actually need contributions from more than
  one config (say, >5). Every consumer is degenerate-to-single-key today (`capture` fires
  on `(= 1 (length tags))`; `cycle-for-filter` returns a cycle only when exactly one
  distinct cycle exists), so the composability the algebra provides is currently unexercised.

- **Caveat (W6): the capture skeleton is a render format, not canonical data.** The skeleton
  is stored as a verbatim org-capture template string (`%?`/`%^{…}` inside drawer values),
  and `:render` is string surgery. The §3 algebra needs *structured* fields/prompts to
  union-merge and take a least-upper-bound — you cannot LUB over opaque template strings. So
  the field/prompt channel must be lifted out of the template grammar (one drawer prop per
  field, or a `:FIELDS:` block, with `:render` *producing* the string) BEFORE `:merge` is
  built; Phase 1's `:render` must not be silently treated as a Phase-2 foundation.

- **Drift policy (advisory).** A tag's cycle changing in `tags.org` can silently re-bucket
  the active/done split of *old* headlines whose stored state is no longer in the new cycle.
  This is left advisory (no reconciliation); a future `:VERSION:` is the natural hook if a
  migration story is ever wanted.

Still deferred regardless: per-keyword distinct faces (org's generic todo/done faces apply
today), config-driven validation, and classes governing overview / on-edit / agenda
projection (Phase 3).
