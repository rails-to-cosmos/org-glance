;;; org-glance-tag-metrics.el --- per-tag event metric segments -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'org-id)
(require 'org-glance-utils)
(require 'org-glance-tag)
(require 'org-glance-graph)

(defvar org-glance-tag-metrics--session-id nil
  "Dynamically bound writer id override for tag-metrics tests.")

(defvar org-glance-tag-metrics--session-ids (make-hash-table :test #'equal)
  "Writer ids keyed by canonical store path for this Emacs process.")

(defconst org-glance-tag-metrics--segment-name-re
  (concat "\\`tag-metrics-seg-"
          "[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-"
          "[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\.eld\\'")
  "Matches a writer-owned tag-metrics segment basename.")

(defconst org-glance-tag-metrics--legacy-name-re
  "\\`tag-metrics-legacy-[[:xdigit:]]\\{40\\}\\.eld\\'"
  "Matches a migrated singleton tag-metrics segment basename.")

(cl-defun org-glance-tag-metrics--session-id (graph)
  "Return this Emacs process's stable writer id for GRAPH."
  (or org-glance-tag-metrics--session-id
      (let ((store (org-glance-graph:store-path graph)))
        (or (gethash store org-glance-tag-metrics--session-ids)
            (puthash store (org-id-uuid)
                     org-glance-tag-metrics--session-ids)))))

(cl-defun org-glance-tag-metrics--file (graph)
  "Path to this Emacs process's writer-owned metrics segment for GRAPH."
  (f-join (org-glance-graph:meta-path graph)
          (format "tag-metrics-seg-%s.eld"
                  (org-glance-tag-metrics--session-id graph))))

(cl-defun org-glance-tag-metrics--files (graph regexp)
  "Return GRAPH's regular metadata files whose basenames match REGEXP."
  (sort (cl-remove-if-not
         #'file-regular-p
         (directory-files (org-glance-graph:meta-path graph) t regexp))
        #'string<))

(cl-defun org-glance-tag-metrics--read-file (path)
  "Read one writer-owned metrics segment at PATH, healing divergent snapshots."
  (org-glance--heal-eld
   path
   (lambda (sides)
     (org-glance-tag-metrics--merge-maps (cl-remove-if-not #'listp sides)))
   (file-name-nondirectory path)))

(cl-defun org-glance-tag-metrics--read (graph)
  "Return GRAPH's folded tag-metrics map, an alist TAG-STRING -> plist.
Writer-owned segments add their disjoint counters.  Migrated singleton
snapshots first merge by extrema, preserving their shared historical base."
  (org-glance-tag-metrics--migrate graph)
  (let* ((legacy (mapcar #'org-glance-tag-metrics--read-file
                         (org-glance-tag-metrics--files
                          graph org-glance-tag-metrics--legacy-name-re)))
         (baseline (org-glance-tag-metrics--merge-maps legacy))
         (segments (mapcar #'org-glance-tag-metrics--read-file
                           (org-glance-tag-metrics--files
                            graph org-glance-tag-metrics--segment-name-re))))
    (org-glance-tag-metrics--sum-maps
     (if baseline (cons baseline segments) segments))))

(cl-defun org-glance-tag-metrics--write (graph map)
  "Persist MAP in this Emacs process's writer-owned segment for GRAPH."
  (org-glance--write-eld (org-glance-tag-metrics--file graph) map))

(cl-defun org-glance-tag-metrics--merge-plists (a b)
  "Union tag metric plists A and B (invariant 8).
`:created' keeps the earliest, `:modified' the latest, and the counters the
`max', which may undercount; any other key prefers B's non-nil value."
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

(cl-defun org-glance-tag-metrics--combine-maps (maps combine)
  "Fold tag-metrics MAPS by applying COMBINE to matching tag plists."
  (let (combined)
    (dolist (map maps (nreverse combined))
      (dolist (cell map)
        (let ((existing (assoc (car cell) combined)))
          (if existing
              (setcdr existing (funcall combine (cdr existing) (cdr cell)))
            (push (cons (car cell) (copy-sequence (cdr cell))) combined)))))))

(cl-defun org-glance-tag-metrics--merge-maps (maps)
  "Fold tag-metrics MAPS (alists TAG-STRING -> plist) into one union map."
  (org-glance-tag-metrics--combine-maps
   maps #'org-glance-tag-metrics--merge-plists))

(cl-defun org-glance-tag-metrics--sum-plists (a b)
  "Combine metrics from disjoint writer segments A and B."
  (let ((out (org-glance-tag-metrics--merge-plists a b)))
    (dolist (key '(:captures :removals) out)
      (setq out (plist-put out key (+ (or (plist-get a key) 0)
                                      (or (plist-get b key) 0)))))))

(cl-defun org-glance-tag-metrics--sum-maps (maps)
  "Fold disjoint writer MAPS, summing counters and merging timestamps."
  (org-glance-tag-metrics--combine-maps
   maps #'org-glance-tag-metrics--sum-plists))

(cl-defun org-glance-tag-metrics--migrate (graph)
  "Move GRAPH's legacy config singleton into a content-addressed baseline.
Divergent legacy files acquire different names and merge by extrema on read."
  (let ((legacy (org-glance-graph:config-file graph "tag-metrics.eld")))
    (when (f-exists? legacy)
      (let* ((map (org-glance--heal-eld
                   legacy
                   (lambda (sides)
                     (org-glance-tag-metrics--merge-maps
                      (cl-remove-if-not #'listp sides)))))
             (bytes (prin1-to-string map))
             (name (format "tag-metrics-legacy-%s.eld"
                           (secure-hash 'sha1 bytes)))
             (dest (f-join (org-glance-graph:meta-path graph) name)))
        (unless (f-exists? dest)
          (org-glance--write-eld dest map))
        (when (f-exists? legacy)
          (f-delete legacy))))))

(cl-defun org-glance-tag-metrics--touch (graph specs)
  "Record GRAPH's tag events for SPECS; hooked before each append.
A live record bumps `:captures', a tombstone `:removals' (its cached tags);
both stamp `:created' once and `:modified' now."
  (let ((now (current-time))
        (map (org-glance-tag-metrics--read-file
              (org-glance-tag-metrics--file graph)))
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
          (when-let* ((meta (org-glance-graph:live-meta graph (plist-get spec :id))))
            (bump-tags (org-glance-headline-metadata:tags meta) :removals))))))
    (when changed (org-glance-tag-metrics--write graph map))))

(add-hook 'org-glance-graph-before-append-functions #'org-glance-tag-metrics--touch)

(add-hook 'org-glance-graph-after-open-functions #'org-glance-tag-metrics--read)

(cl-defun org-glance-tag-metrics--ensure-created (graph live-tags)
  "Ensure LIVE-TAGS each have a `:created' in GRAPH's sidecar; return the map.
Seed a missing one from its headlines' earliest blob mtime, else now."
  (let* ((all (org-glance-tag-metrics--read graph))
         (map (org-glance-tag-metrics--read-file
               (org-glance-tag-metrics--file graph)))
         (unseeded (cl-remove-if (lambda (tag) (plist-get (cdr (assoc tag all)) :created))
                                 live-tags)))
    (if (null unseeded)
        all
      (let ((earliest (make-hash-table :test 'equal)))
        (dolist (meta (org-glance-graph:headlines graph))
          (let ((tags (org-glance-headline-metadata:tag-strings meta)))
            (when (cl-intersection tags unseeded :test #'string=)
              (let ((mtime (org-glance--file-mtime
                            (org-glance-graph:content-path
                             graph (org-glance-headline-metadata:id meta)))))
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
        (org-glance-tag-metrics--read graph)))))

(cl-defun org-glance-tag-metrics:all (graph)
  "Return per-tag metrics for GRAPH's live tags, seeding missing `:created'.
Alist TAG-STRING -> (:count N :states ((STATE . N)...) :created TS :modified TS
:captures N :removals N); `:count' and `:states' fold from live headlines."
  (let ((counts (make-hash-table :test 'equal))
        (states (make-hash-table :test 'equal)))
    (dolist (meta (org-glance-graph:headlines graph))
      (let ((state (org-glance-headline-metadata:state meta)))
        (dolist (k (org-glance-headline-metadata:tag-strings meta))
          (puthash k (1+ (gethash k counts 0)) counts)
          (when (org-glance--present-string? state)
            (let ((sa (gethash k states)))
              (cl-incf (alist-get state sa 0 nil #'string=))
              (puthash k sa states))))))
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
