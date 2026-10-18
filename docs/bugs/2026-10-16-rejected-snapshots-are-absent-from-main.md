# Rejected Snapshots are absent from Main

## Symptom

Glance keeps a rejected Snapshot visible, while org-glance omits the same
Snapshot from Main. The rejected content cannot be materialized and edited in
org-glance.

## Reproduction

1. Publish two child Snapshots for one headline family.
2. Publish a `leaves-rejected` event targeting one child.
3. Reconcile org-glance's shared cache and open Main.
4. Observe only the surviving current Snapshot.
5. Attempt to materialize the rejected Snapshot by its exact version identity.

## Evidence

`org-glance-cache--project-family` computes `leaf-ids` from
`org-glance-version:candidates`, then builds payload observations and projection
rows only for those candidates (`src/data/org-glance-cache.el:274-275`,
`:350-380`). The read paths already include the `rejected` role
(`src/data/org-glance-cache.el:1258`, `:1316`, `:1387`). No projector row can
currently carry that role.

`org-glance-material--write-version` accepts only a selected current candidate
(`src/view/org-glance-material.el:141-150`). A rejected Snapshot remains a
structural Leaf, so editing it must accept that exact Leaf and publish the new
Snapshot above the complete structural frontier.

## Required behavior

Main exposes each readable rejected Snapshot with an exact Row identity.
Materializing that Row opens its immutable payload. Saving an edit publishes a
child above every structural Leaf; the child becomes the sole current Snapshot.

## Resolution

The family projector now observes every structural Leaf and assigns rejected
Snapshots the exact `HEADLINE/VERSION` Row identity. Point lookup and Material
resolve that identity to its logical family and digest-verified payload. The
save guard accepts an unchanged structural Leaf; the existing graph writer
publishes the edited child above the entire frontier.

The focused
`rejected-snapshot-is-visible-and-an-edit-makes-it-current` regression covers
visibility, exact materialization, structural parents, and the resulting
current title.
