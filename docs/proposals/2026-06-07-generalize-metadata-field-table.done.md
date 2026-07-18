# Generalize: field-spec table for the metadata projection

**Status:** proposed · generalizer audit 2026-06-07

## Pattern

The 11 metadata fields (`id state title tags hash schedule deadline priority
linked? propertized? encrypted?`) are hand-written **four times in strict
parallel** in `org-glance-graph-v2.el`: struct slots, the
`org-glance-headline-v2:metadata` constructor, `serialize`, and `deserialize`
— with per-field quirks scattered across them (flags coerce `(and … t)` on
write and `(eq t …)` on read; JSON keys drop the `?`; `:tags` vectorizes).
**Deserialize is the trap:** forget its line for a new field and the field is
silently always-nil on read (indistinguishable from "needs reindex"); forget
serialize and it is silently never persisted. Nothing errors — a silent
data-skew bug class.

## Files

- `org-glance-graph-v2.el`: struct slots (~l.50–64), `metadata` constructor
  (~l.66–81), `serialize` (~l.96–110), `deserialize` (~l.112–124).
- (Consumers like `render-headline` unaffected — they use accessors.)

## Proposed change

One ordered field-spec table driving constructor + serialize + deserialize
(the `cl-defstruct` stays hand-written for its slot options; a load-time
assertion checks its slots against the table):

```elisp
(defconst org-glance-headline-metadata-v2:fields
  ;; (SLOT         JSON-KEY      FROM-HEADLINE                              COERCE)
  `((id            :id           ,#'org-glance-headline-v2:id)
    (state         :state        ,#'org-glance-headline-v2:state)
    (title         :title        ,#'org-glance-headline-v2:title)
    (tags          :tags         ,#'org-glance-headline-v2:tags             vector-of-strings)
    (hash          :hash         ,#'org-glance-headline-v2:hash)
    (schedule      :schedule     ,#'org-glance-headline-v2:schedule)
    (deadline      :deadline     ,#'org-glance-headline-v2:deadline)
    (priority      :priority     ,#'org-glance-headline-v2:priority)
    (linked?       :linked       ,(lambda (h) (and (org-glance-headline-v2:links h) t))      bool)
    (propertized?  :propertized  ,(lambda (h) (and (org-glance-headline-v2:properties h) t)) bool)
    (encrypted?    :encrypted    ,(lambda (h) (and (org-glance-headline-v2:encrypted? h) t)) bool)))
```

`metadata`, `serialize`, `deserialize` become three short loops over the
table. Field order in the table preserves today's JSON key order, so
serialized records stay **byte-identical** (`test-graph` / `test-segments`
are the gate). Adding a projection field = 1 row + 1 struct slot (asserted),
and serialize/deserialize can no longer drift.

This *strengthens* MIGRATION-PLAN Decision 6 ("metadata only ever produced via
`org-glance-headline-v2:metadata` so its shape can't drift") — the shape
becomes data.

## LOC estimate

+~35 (table + three loops + slot assertion) / −~50 now. Per future field
(`:effort`, `:closed`, `:category`…): ~8 lines across 4 sites → **2 lines**;
eliminates the silent serialize/deserialize-skew bug class. Phase 2.2b added
three flag fields exactly this way — growth is proven.

## Risk

On-disk record byte-compatibility must hold (keep key order) — tested.
Public accessors unchanged. Single-file locality.

## Existing precedent

`org-glance-overview-v2.el`'s loop-driven boolean/planning clauses; the
(proposed) filter-key table — same data-driven shape one layer up.
