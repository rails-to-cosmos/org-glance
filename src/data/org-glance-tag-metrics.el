;;; org-glance-tag-metrics.el --- per-tag event metrics sidecar -*- lexical-binding: t; -*-

;;; Commentary:
;; A side index of per-tag wall-clock facts the content store cannot provide (it
;; keeps only a monotonic `seq', never a timestamp).  The store fires
;; `org-glance-graph-before-append-functions' before every WAL append; this module
;; stamps :created (once, first sighting), :modified (every touching event) and
;; :captures/:removals counters into `<store>/config/tag-metrics.eld'.  Live
;; :count and :states stay DERIVED from the headlines so they cannot drift.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'org-glance-utils)
(require 'org-glance-tag)
(require 'org-glance-graph)

(cl-defun org-glance-tag-metrics--file (graph)
  "Path to GRAPH's per-tag metrics sidecar."
  (f-join (org-glance-graph:store-path graph) "config" "tag-metrics.eld"))

(cl-defun org-glance-tag-metrics--read (graph)
  "GRAPH's tag-metrics map: alist of TAG-STRING -> plist, or nil.
Auto-resolves a git-conflicted sidecar by union-merging every side's map
(`--merge-maps') through the shared `org-glance--heal-eld'.  A `config/*.eld' is
not `*.jsonl', so the WAL's union driver never prevents these -- two machines
that both captured leave conflict markers here."
  (org-glance--heal-eld
   (org-glance-tag-metrics--file graph)
   (lambda (sides)
     (org-glance-tag-metrics--merge-maps (cl-remove-if-not #'listp sides)))))

(cl-defun org-glance-tag-metrics--write (graph map)
  "Persist MAP (alist TAG-STRING -> plist) as GRAPH's tag-metrics sidecar."
  (org-glance--write-eld (org-glance-tag-metrics--file graph) map))

(cl-defun org-glance-tag-metrics--merge-plists (a b)
  "Union tag metric plists A and B.
:created keeps the earliest and :modified the latest; :captures/:removals take
the `max' (a counter only increments, so `max' never inflates -- a sum would
double-count the common base -- though it may undercount when both sides logged
distinct events, an accepted floor for this soft index); any other key prefers
B's non-nil value."
  (let ((out (copy-sequence a)))
    (cl-loop for (k v) on b by #'cddr do
             (let ((cur (plist-get out k)))
               (setq out
                     (plist-put out k
                                (pcase k
                                  (:created  (cond ((not cur) v) ((not v) cur)
                                                   ((time-less-p v cur) v) (t cur)))
                                  (:modified (cond ((not cur) v) ((not v) cur)
                                                   ((time-less-p cur v) v) (t cur)))
                                  ((or :captures :removals) (max (or cur 0) (or v 0)))
                                  (_ (or v cur)))))))
    out))

(cl-defun org-glance-tag-metrics--merge-maps (maps)
  "Fold tag-metrics MAPS (each an alist TAG-STRING -> plist) into one union map.
A tag present in several MAPS has its plists merged by `--merge-plists'."
  (let (merged)
    (dolist (map maps (nreverse merged))
      (dolist (cell map)
        (let ((existing (assoc (car cell) merged)))
          (if existing
              (setcdr existing (org-glance-tag-metrics--merge-plists
                                (cdr existing) (cdr cell)))
            (push (cons (car cell) (copy-sequence (cdr cell))) merged)))))))

(cl-defun org-glance-tag-metrics--blob-mtime (graph id)
  "Filesystem mtime of ID's content blob in GRAPH, or nil when absent."
  (let ((path (org-glance-graph:content-path graph id)))
    (and (f-exists? path)
         (file-attribute-modification-time (file-attributes path)))))

(cl-defun org-glance-tag-metrics--touch (graph specs)
  "Record GRAPH's tag events for the just-appended SPECS.
Live records bump :captures; tombstones (tags resolved from the still-current
cache) bump :removals; both stamp :created once and :modified now.  Runs on
`org-glance-graph-before-append-functions'."
  (let ((now (current-time))
        (map (org-glance-tag-metrics--read graph))
        (changed nil))
    (cl-labels
        ((bump (tag counter)
           (let* ((cell (assoc tag map))
                  (pl (cdr cell)))
             (unless (plist-get pl :created) (setq pl (plist-put pl :created now)))
             (setq pl (plist-put pl :modified now))
             (setq pl (plist-put pl counter (1+ (or (plist-get pl counter) 0))))
             (if cell (setcdr cell pl) (push (cons tag pl) map))
             (setq changed t)))
         (bump-tags (tags counter)
           (dolist (tag tags) (bump (format "%s" tag) counter))))
      (dolist (spec specs)
        (cond
         ((org-glance-headline-metadata? spec)
          (bump-tags (org-glance-headline-metadata:tags spec) :captures))
         ((plist-get spec :tombstone)
          (let ((meta (org-glance-graph:get-headline graph (plist-get spec :id))))
            (when (org-glance-headline-metadata? meta)
              (bump-tags (org-glance-headline-metadata:tags meta) :removals)))))))
    (when changed (org-glance-tag-metrics--write graph map))))

(add-hook 'org-glance-graph-before-append-functions #'org-glance-tag-metrics--touch)

(cl-defun org-glance-tag-metrics--ensure-created (graph live-tags)
  "Ensure every tag in LIVE-TAGS has a :created in GRAPH's sidecar; return the map.
Seed a missing one from the earliest content-blob mtime among its headlines
(falling back to now) and persist once.  Stats blobs ONLY when something needs
seeding, so warm calls do no I/O."
  (let* ((map (org-glance-tag-metrics--read graph))
         (unseeded (cl-remove-if (lambda (tag) (plist-get (cdr (assoc tag map)) :created))
                                 live-tags)))
    (if (null unseeded)
        map
      (let ((earliest (make-hash-table :test 'equal)))
        (dolist (meta (org-glance-graph:headlines graph))
          (let ((tags (mapcar (lambda (x) (format "%s" x))
                              (org-glance-headline-metadata:tags meta))))
            (when (cl-intersection tags unseeded :test #'string=)
              (let ((mtime (org-glance-tag-metrics--blob-mtime
                            graph (org-glance-headline-metadata:id meta))))
                (when mtime
                  (dolist (tag tags)
                    (when (and (member tag unseeded)
                               (or (null (gethash tag earliest))
                                   (time-less-p mtime (gethash tag earliest))))
                      (puthash tag mtime earliest))))))))
        (dolist (tag unseeded)
          (let* ((cell (assoc tag map))
                 (pl (plist-put (cdr cell) :created
                                (or (gethash tag earliest) (current-time)))))
            (if cell (setcdr cell pl) (push (cons tag pl) map))))
        (org-glance-tag-metrics--write graph map)
        map))))

(cl-defun org-glance-tag-metrics:all (graph)
  "Per-tag metrics for GRAPH's live tags.
Alist of TAG-STRING -> plist (:count N :states ((STATE . N)...) :created TS
:modified TS :captures N :removals N).  :count and :states are folded fresh from
the live headlines; the wall-clock facts and counters come from the sidecar,
seeding :created lazily for tags that predate the metrics index."
  (let ((counts (make-hash-table :test 'equal))
        (states (make-hash-table :test 'equal)))
    (dolist (meta (org-glance-graph:headlines graph))
      (let ((state (org-glance-headline-metadata:state meta)))
        (dolist (tag (org-glance-headline-metadata:tags meta))
          (let ((k (format "%s" tag)))
            (puthash k (1+ (gethash k counts 0)) counts)
            (when (org-glance--present-string? state)
              (let ((sa (gethash k states)))
                (cl-incf (alist-get state sa 0 nil #'string=))
                (puthash k sa states)))))))
    (let* ((live-tags (cl-loop for k being the hash-keys of counts collect k))
           (sidecar (org-glance-tag-metrics--ensure-created graph live-tags))
           result)
      (dolist (k live-tags)
        (let ((pl (cdr (assoc k sidecar))))
          (push (list k
                      :count (gethash k counts)
                      :states (gethash k states)
                      :created (plist-get pl :created)
                      :modified (plist-get pl :modified)
                      :captures (or (plist-get pl :captures) 0)
                      :removals (or (plist-get pl :removals) 0))
                result)))
      result)))

(provide 'org-glance-tag-metrics)
;;; org-glance-tag-metrics.el ends here
