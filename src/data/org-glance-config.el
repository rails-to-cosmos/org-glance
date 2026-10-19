;;; org-glance-config.el --- Typed shared settings -*- lexical-binding: t -*-

(require 'org-glance-config-file)
(require 'org-glance-tag-config)

(cl-defstruct (org-glance-settings (:constructor org-glance-settings--make)
                                   (:conc-name org-glance-settings:))
  tree cycles template tags storage views colors diagnostics)

(defun org-glance-config--error (message)
  "Signal an invalid configuration with MESSAGE."
  (signal 'org-glance-config-error (list message)))

(defun org-glance-config--object (value)
  "Require a JSON object VALUE."
  (unless (hash-table-p value) (org-glance-config--error "Expected a settings object"))
  value)

(defun org-glance-config--field (object key predicate &optional default)
  "Validate OBJECT's KEY with PREDICATE, using DEFAULT only for absence."
  (let ((value (gethash key object :missing)))
    (if (eq value :missing) default
      (unless (funcall predicate value)
        (org-glance-config--error (concat "Invalid setting: " key)))
      value)))

(defun org-glance-config--known (object names)
  "Refuse OBJECT members outside NAMES."
  (maphash (lambda (key _) (unless (member key names)
                             (org-glance-config--error (concat "Unknown setting: " key)))) object))

(defconst org-glance-config--newer-characters
  (let ((table (make-char-table nil nil)))
    (dolist (range '((#x88f . #x88f) (#xc5c . #xc5c) (#xcdc . #xcdc) (#x1c89 . #x1c8a)
                     (#xa7cb . #xa7cf) (#xa7d2 . #xa7d2) (#xa7d4 . #xa7d4) (#xa7da . #xa7dc)
                     (#xa7f1 . #xa7f1) (#x105c0 . #x105f3) (#x10940 . #x10959)
                     (#x10d40 . #x10d65) (#x10d6f . #x10d85) (#x10ec2 . #x10ec7)
                     (#x11380 . #x11389) (#x1138b . #x1138b) (#x1138e . #x1138e)
                     (#x11390 . #x113b5) (#x113b7 . #x113b7) (#x113d1 . #x113d1) (#x113d3 . #x113d3)
                     (#x116d0 . #x116e3) (#x11bc0 . #x11be0) (#x11bf0 . #x11bf9)
                     (#x11db0 . #x11ddb) (#x11de0 . #x11de9) (#x13460 . #x143fa)
                     (#x16100 . #x1611d) (#x16130 . #x16139) (#x16d40 . #x16d6c) (#x16d70 . #x16d79)
                     (#x16ea0 . #x16eb8) (#x16ebb . #x16ed3) (#x16ff2 . #x16ff6)
                     (#x187f8 . #x187ff) (#x18cff . #x18cff) (#x18d09 . #x18d1e) (#x18d80 . #x18df2)
                     (#x1ccf0 . #x1ccf9) (#x1e5d0 . #x1e5ed) (#x1e5f0 . #x1e5fa)
                     (#x1e6c0 . #x1e6de) (#x1e6e0 . #x1e6e2) (#x1e6e4 . #x1e6e5)
                     (#x1e6e7 . #x1e6ed) (#x1e6f0 . #x1e6f4) (#x1e6fe . #x1e6ff)
                     (#x2b73a . #x2b73f) (#x2cea2 . #x2cead) (#x323b0 . #x33479)))
      (set-char-table-range table range 'Cn))
    table)
  "Unicode 16/17 letters and numbers absent from the parser's Unicode 15.1.
Derived from GHC.Unicode; checked exhaustively at the paired boundary.")

(defun org-glance-config--category (char)
  "Return CHAR's parser-compatible category on the supported Unicode repertoire."
  (or (aref org-glance-config--newer-characters char)
      (get-char-code-property char 'general-category)))

(defun org-glance-config--tag (text)
  "Return non-nil when TEXT is an exact supported tag."
  (and (stringp text) (> (length text) 0)
       (if (string-match-p "[^\0-\177]" text)
           (cl-every (lambda (char)
                       (or (memq char '(?_ ?@ ?# ?% ?-))
                           (memq (org-glance-config--category char)
                                 '(Lu Ll Lt Lm Lo Nd Nl No)))) text)
         (string-match-p "\\`[A-Za-z0-9_@#%_-]+\\'" text))))

(defun org-glance-config--tag-name (text)
  "Use context-free Unicode lowercase, matching Data.Text.toLower.
U+0130 has the sole unconditional multi-character lowercase mapping.
Final sigma stays distinct; storage membership retains its owner's case folding."
  (if (string-match-p "[^\0-\177]" text)
      (mapconcat (lambda (char) (cond ((= char #x130) "i\u0307")
                                    ((= char #x212a) "k")
                                    ((eq (org-glance-config--category char) 'Cn) (char-to-string char))
                                    (t (char-to-string (downcase char))))) text "")
    (downcase text)))

(defconst org-glance-config--space "[\t-\r \u00a0\u1680\u2000-\u200a\u202f\u205f\u3000]"
  "Data.Char.isSpace: ASCII HT through CR, and Unicode Space_Separator.
NEL, line/paragraph separators and format characters are excluded.")

;; Unicode full-fold differences from lowercase, matching Data.Text 2.0.2.
(defconst org-glance-config--storage-fold-exceptions
  (let ((table (make-char-table nil nil)))
    (dolist (entry
             '((#xb5 . "\u03bc") (#xdf . "\u0073\u0073") (#x149 . "\u02bc\u006e") (#x17f . "\u0073")
                 (#x1f0 . "\u006a\u030c") (#x345 . "\u03b9") (#x390 . "\u03b9\u0308\u0301") (#x3b0 . "\u03c5\u0308\u0301")
                 (#x3c2 . "\u03c3") (#x3d0 . "\u03b2") (#x3d1 . "\u03b8") (#x3d5 . "\u03c6")
                 (#x3d6 . "\u03c0") (#x3f0 . "\u03ba") (#x3f1 . "\u03c1") (#x3f5 . "\u03b5")
                 (#x587 . "\u0565\u0582") (#x13a0 . "\u13a0") (#x13a1 . "\u13a1") (#x13a2 . "\u13a2")
                 (#x13a3 . "\u13a3") (#x13a4 . "\u13a4") (#x13a5 . "\u13a5") (#x13a6 . "\u13a6")
                 (#x13a7 . "\u13a7") (#x13a8 . "\u13a8") (#x13a9 . "\u13a9") (#x13aa . "\u13aa")
                 (#x13ab . "\u13ab") (#x13ac . "\u13ac") (#x13ad . "\u13ad") (#x13ae . "\u13ae")
                 (#x13af . "\u13af") (#x13b0 . "\u13b0") (#x13b1 . "\u13b1") (#x13b2 . "\u13b2")
                 (#x13b3 . "\u13b3") (#x13b4 . "\u13b4") (#x13b5 . "\u13b5") (#x13b6 . "\u13b6")
                 (#x13b7 . "\u13b7") (#x13b8 . "\u13b8") (#x13b9 . "\u13b9") (#x13ba . "\u13ba")
                 (#x13bb . "\u13bb") (#x13bc . "\u13bc") (#x13bd . "\u13bd") (#x13be . "\u13be")
                 (#x13bf . "\u13bf") (#x13c0 . "\u13c0") (#x13c1 . "\u13c1") (#x13c2 . "\u13c2")
                 (#x13c3 . "\u13c3") (#x13c4 . "\u13c4") (#x13c5 . "\u13c5") (#x13c6 . "\u13c6")
                 (#x13c7 . "\u13c7") (#x13c8 . "\u13c8") (#x13c9 . "\u13c9") (#x13ca . "\u13ca")
                 (#x13cb . "\u13cb") (#x13cc . "\u13cc") (#x13cd . "\u13cd") (#x13ce . "\u13ce")
                 (#x13cf . "\u13cf") (#x13d0 . "\u13d0") (#x13d1 . "\u13d1") (#x13d2 . "\u13d2")
                 (#x13d3 . "\u13d3") (#x13d4 . "\u13d4") (#x13d5 . "\u13d5") (#x13d6 . "\u13d6")
                 (#x13d7 . "\u13d7") (#x13d8 . "\u13d8") (#x13d9 . "\u13d9") (#x13da . "\u13da")
                 (#x13db . "\u13db") (#x13dc . "\u13dc") (#x13dd . "\u13dd") (#x13de . "\u13de")
                 (#x13df . "\u13df") (#x13e0 . "\u13e0") (#x13e1 . "\u13e1") (#x13e2 . "\u13e2")
                 (#x13e3 . "\u13e3") (#x13e4 . "\u13e4") (#x13e5 . "\u13e5") (#x13e6 . "\u13e6")
                 (#x13e7 . "\u13e7") (#x13e8 . "\u13e8") (#x13e9 . "\u13e9") (#x13ea . "\u13ea")
                 (#x13eb . "\u13eb") (#x13ec . "\u13ec") (#x13ed . "\u13ed") (#x13ee . "\u13ee")
                 (#x13ef . "\u13ef") (#x13f0 . "\u13f0") (#x13f1 . "\u13f1") (#x13f2 . "\u13f2")
                 (#x13f3 . "\u13f3") (#x13f4 . "\u13f4") (#x13f5 . "\u13f5") (#x13f8 . "\u13f0")
                 (#x13f9 . "\u13f1") (#x13fa . "\u13f2") (#x13fb . "\u13f3") (#x13fc . "\u13f4")
                 (#x13fd . "\u13f5") (#x1c80 . "\u0432") (#x1c81 . "\u0434") (#x1c82 . "\u043e")
                 (#x1c83 . "\u0441") (#x1c84 . "\u0442") (#x1c85 . "\u0442") (#x1c86 . "\u044a")
                 (#x1c87 . "\u0463") (#x1c88 . "\ua64b") (#x1e96 . "\u0068\u0331") (#x1e97 . "\u0074\u0308")
                 (#x1e98 . "\u0077\u030a") (#x1e99 . "\u0079\u030a") (#x1e9a . "\u0061\u02be") (#x1e9b . "\u1e61")
                 (#x1e9e . "\u0073\u0073") (#x1f50 . "\u03c5\u0313") (#x1f52 . "\u03c5\u0313\u0300") (#x1f54 . "\u03c5\u0313\u0301")
                 (#x1f56 . "\u03c5\u0313\u0342") (#x1f80 . "\u1f00\u03b9") (#x1f81 . "\u1f01\u03b9") (#x1f82 . "\u1f02\u03b9")
                 (#x1f83 . "\u1f03\u03b9") (#x1f84 . "\u1f04\u03b9") (#x1f85 . "\u1f05\u03b9") (#x1f86 . "\u1f06\u03b9")
                 (#x1f87 . "\u1f07\u03b9") (#x1f88 . "\u1f00\u03b9") (#x1f89 . "\u1f01\u03b9") (#x1f8a . "\u1f02\u03b9")
                 (#x1f8b . "\u1f03\u03b9") (#x1f8c . "\u1f04\u03b9") (#x1f8d . "\u1f05\u03b9") (#x1f8e . "\u1f06\u03b9")
                 (#x1f8f . "\u1f07\u03b9") (#x1f90 . "\u1f20\u03b9") (#x1f91 . "\u1f21\u03b9") (#x1f92 . "\u1f22\u03b9")
                 (#x1f93 . "\u1f23\u03b9") (#x1f94 . "\u1f24\u03b9") (#x1f95 . "\u1f25\u03b9") (#x1f96 . "\u1f26\u03b9")
                 (#x1f97 . "\u1f27\u03b9") (#x1f98 . "\u1f20\u03b9") (#x1f99 . "\u1f21\u03b9") (#x1f9a . "\u1f22\u03b9")
                 (#x1f9b . "\u1f23\u03b9") (#x1f9c . "\u1f24\u03b9") (#x1f9d . "\u1f25\u03b9") (#x1f9e . "\u1f26\u03b9")
                 (#x1f9f . "\u1f27\u03b9") (#x1fa0 . "\u1f60\u03b9") (#x1fa1 . "\u1f61\u03b9") (#x1fa2 . "\u1f62\u03b9")
                 (#x1fa3 . "\u1f63\u03b9") (#x1fa4 . "\u1f64\u03b9") (#x1fa5 . "\u1f65\u03b9") (#x1fa6 . "\u1f66\u03b9")
                 (#x1fa7 . "\u1f67\u03b9") (#x1fa8 . "\u1f60\u03b9") (#x1fa9 . "\u1f61\u03b9") (#x1faa . "\u1f62\u03b9")
                 (#x1fab . "\u1f63\u03b9") (#x1fac . "\u1f64\u03b9") (#x1fad . "\u1f65\u03b9") (#x1fae . "\u1f66\u03b9")
                 (#x1faf . "\u1f67\u03b9") (#x1fb2 . "\u1f70\u03b9") (#x1fb3 . "\u03b1\u03b9") (#x1fb4 . "\u03ac\u03b9")
                 (#x1fb6 . "\u03b1\u0342") (#x1fb7 . "\u03b1\u0342\u03b9") (#x1fbc . "\u03b1\u03b9") (#x1fbe . "\u03b9")
                 (#x1fc2 . "\u1f74\u03b9") (#x1fc3 . "\u03b7\u03b9") (#x1fc4 . "\u03ae\u03b9") (#x1fc6 . "\u03b7\u0342")
                 (#x1fc7 . "\u03b7\u0342\u03b9") (#x1fcc . "\u03b7\u03b9") (#x1fd2 . "\u03b9\u0308\u0300") (#x1fd3 . "\u03b9\u0308\u0301")
                 (#x1fd6 . "\u03b9\u0342") (#x1fd7 . "\u03b9\u0308\u0342") (#x1fe2 . "\u03c5\u0308\u0300") (#x1fe3 . "\u03c5\u0308\u0301")
                 (#x1fe4 . "\u03c1\u0313") (#x1fe6 . "\u03c5\u0342") (#x1fe7 . "\u03c5\u0308\u0342") (#x1ff2 . "\u1f7c\u03b9")
                 (#x1ff3 . "\u03c9\u03b9") (#x1ff4 . "\u03ce\u03b9") (#x1ff6 . "\u03c9\u0342") (#x1ff7 . "\u03c9\u0342\u03b9")
                 (#x1ffc . "\u03c9\u03b9") (#xab70 . "\u13a0") (#xab71 . "\u13a1") (#xab72 . "\u13a2")
                 (#xab73 . "\u13a3") (#xab74 . "\u13a4") (#xab75 . "\u13a5") (#xab76 . "\u13a6")
                 (#xab77 . "\u13a7") (#xab78 . "\u13a8") (#xab79 . "\u13a9") (#xab7a . "\u13aa")
                 (#xab7b . "\u13ab") (#xab7c . "\u13ac") (#xab7d . "\u13ad") (#xab7e . "\u13ae")
                 (#xab7f . "\u13af") (#xab80 . "\u13b0") (#xab81 . "\u13b1") (#xab82 . "\u13b2")
                 (#xab83 . "\u13b3") (#xab84 . "\u13b4") (#xab85 . "\u13b5") (#xab86 . "\u13b6")
                 (#xab87 . "\u13b7") (#xab88 . "\u13b8") (#xab89 . "\u13b9") (#xab8a . "\u13ba")
                 (#xab8b . "\u13bb") (#xab8c . "\u13bc") (#xab8d . "\u13bd") (#xab8e . "\u13be")
                 (#xab8f . "\u13bf") (#xab90 . "\u13c0") (#xab91 . "\u13c1") (#xab92 . "\u13c2")
                 (#xab93 . "\u13c3") (#xab94 . "\u13c4") (#xab95 . "\u13c5") (#xab96 . "\u13c6")
                 (#xab97 . "\u13c7") (#xab98 . "\u13c8") (#xab99 . "\u13c9") (#xab9a . "\u13ca")
                 (#xab9b . "\u13cb") (#xab9c . "\u13cc") (#xab9d . "\u13cd") (#xab9e . "\u13ce")
                 (#xab9f . "\u13cf") (#xaba0 . "\u13d0") (#xaba1 . "\u13d1") (#xaba2 . "\u13d2")
                 (#xaba3 . "\u13d3") (#xaba4 . "\u13d4") (#xaba5 . "\u13d5") (#xaba6 . "\u13d6")
                 (#xaba7 . "\u13d7") (#xaba8 . "\u13d8") (#xaba9 . "\u13d9") (#xabaa . "\u13da")
                 (#xabab . "\u13db") (#xabac . "\u13dc") (#xabad . "\u13dd") (#xabae . "\u13de")
                 (#xabaf . "\u13df") (#xabb0 . "\u13e0") (#xabb1 . "\u13e1") (#xabb2 . "\u13e2")
                 (#xabb3 . "\u13e3") (#xabb4 . "\u13e4") (#xabb5 . "\u13e5") (#xabb6 . "\u13e6")
                 (#xabb7 . "\u13e7") (#xabb8 . "\u13e8") (#xabb9 . "\u13e9") (#xabba . "\u13ea")
                 (#xabbb . "\u13eb") (#xabbc . "\u13ec") (#xabbd . "\u13ed") (#xabbe . "\u13ee")
                 (#xabbf . "\u13ef") (#xfb00 . "\u0066\u0066") (#xfb01 . "\u0066\u0069") (#xfb02 . "\u0066\u006c")
                 (#xfb03 . "\u0066\u0066\u0069") (#xfb04 . "\u0066\u0066\u006c") (#xfb05 . "\u0073\u0074") (#xfb06 . "\u0073\u0074")
                 (#xfb13 . "\u0574\u0576") (#xfb14 . "\u0574\u0565") (#xfb15 . "\u0574\u056b") (#xfb16 . "\u057e\u0576")
                 (#xfb17 . "\u0574\u056d")))
      (aset table (car entry) (cdr entry)))
    table)
  "Full case-fold mappings that differ from context-free lowercase.")

(defun org-glance-config--storage-tag-name (text)
  "Fold TEXT for shared storage membership, independently of scope identity."
  (if (string-match-p "[^\0-\177]" text)
      (mapconcat (lambda (char)
                   (or (aref org-glance-config--storage-fold-exceptions char)
                       (org-glance-config--tag-name (char-to-string char)))) text "")
    (downcase text)))

(defun org-glance-config:local-tags-p (settings tags)
  "Return non-nil when SETTINGS require local storage for any of TAGS."
  (let ((local (mapcar #'org-glance-config--storage-tag-name
                       (org-glance-settings:storage settings))))
    (cl-some (lambda (tag)
               (member (org-glance-config--storage-tag-name (format "%s" tag)) local)) tags)))

(defun org-glance-config--trim (text)
  "Trim TEXT's Unicode whitespace using the shared settings convention."
  (string-trim text (concat org-glance-config--space "+")
               (concat org-glance-config--space "+")))

(defun org-glance-config--token (text)
  "Return TEXT's keyword name, refusing malformed tokens and metadata."
  (unless (stringp text) (org-glance-config--error "Invalid TODO keyword or metadata"))
  (let ((end 0))
    (while (and (< end (length text))
                (let ((char (aref text end)))
                  (or (= char ?_)
                      (memq (org-glance-config--category char) '(Lu Ll Lt Lm Lo)))))
      (cl-incf end))
    (let ((suffix (substring text end)))
      (unless (and (> end 0) (not (string-match-p org-glance-config--space text))
                   (or (equal suffix "") (string-match-p "\\`([^()]*)\\'" suffix)))
        (org-glance-config--error "Invalid TODO keyword or metadata")))
    (substring text 0 end)))

(defun org-glance-config--cycles (value)
  "Validate VALUE's ordered TODO cycles and return their token pairs."
  (unless (vectorp value) (org-glance-config--error "todoCycles must be an array"))
  (mapcar
   (lambda (cycle)
     (org-glance-config--object cycle)
     (org-glance-config--known cycle '("active" "inactive"))
     (let* ((a (append (org-glance-config--field cycle "active" #'vectorp []) nil))
            (i (append (org-glance-config--field cycle "inactive" #'vectorp []) nil))
            (names (mapcar #'org-glance-config--token (append a i))))
       (unless (and names (= (length names) (length (delete-dups (copy-sequence names)))))
         (org-glance-config--error "Empty cycle or duplicate TODO keyword"))
       (cons a i))) (append value nil)))

(defun org-glance-config--names (cycles)
  "Merge CYCLES' keyword names with active meaning winning within one scope."
  (let* ((active (delete-dups (mapcan (lambda (c) (mapcar #'org-glance-config--token (car c))) cycles)))
         (inactive (delete-dups (mapcan (lambda (c) (mapcar #'org-glance-config--token (cdr c))) cycles))))
    (cons active (cl-remove-if (lambda (word) (member word active)) inactive))))

(defun org-glance-config--pairs (object)
  "Return OBJECT's members in key order."
  (let (pairs)
    (maphash (lambda (key value) (push (cons key value) pairs)) object)
    (sort pairs (lambda (a b) (string< (car a) (car b))))))

(defun org-glance-config:decode (snapshot)
  "Decode SNAPSHOT into typed shared settings without observing the filesystem."
  (let* ((object (org-glance-config-file--envelope (org-glance-config-file:object snapshot)))
         (cycles (org-glance-config--cycles
                  (org-glance-config--field object "todoCycles" #'vectorp [])))
         (tree (org-glance-config--object
                (org-glance-config--field object "tree" #'hash-table-p (make-hash-table :test 'equal))))
         (raw-tags (org-glance-config--field object "tags" #'hash-table-p (make-hash-table :test 'equal)))
         (seen (make-hash-table :test 'equal)) tags storage diagnostics)
    (org-glance-config--known tree '("inboxTag" "headlineHistoryDepth" "repositoryAutosync"
                                     "repositoryRemote" "googleOAuthClientId" "defaultAgent"))
    (let ((values (make-hash-table :test 'equal)))
      (let ((inbox (org-glance-config--tag-name
                    (org-glance-config--field tree "inboxTag" #'org-glance-config--tag "inbox"))))
        (unless (org-glance-config--tag inbox) (org-glance-config--error "Invalid normalized inbox tag"))
        (puthash "inboxTag" inbox values))
      (puthash "headlineHistoryDepth" (org-glance-config--field tree "headlineHistoryDepth"
                                                                (lambda (v) (and (integerp v) (<= 0 v) (<= v #x7fffffffffffffff))) 10) values)
      (puthash "repositoryAutosync" (org-glance-config--field tree "repositoryAutosync"
                                                              (lambda (v) (memq v '(t :false))) :false) values)
      (puthash "repositoryRemote" (org-glance-config--trim (org-glance-config--field tree "repositoryRemote"
                                                                         (lambda (v) (and (stringp v) (string-match-p
                                                                                "\\`[a-zA-Z0-9_./][a-zA-Z0-9_./-]*\\'" (org-glance-config--trim v)))) "origin")) values)
      (let ((client (org-glance-config--field tree "googleOAuthClientId"
                                              (lambda (v) (and (stringp v)
                                                               (> (length (org-glance-config--trim v)) 0)
                                                               (not (string-match-p org-glance-config--space
                                                                                    (org-glance-config--trim v))))))))
        (puthash "googleOAuthClientId" (and client (org-glance-config--trim client)) values))
      (puthash "defaultAgent" (org-glance-config--field tree "defaultAgent" #'stringp) values)
      (setq tree values))
    (dolist (pair (org-glance-config--pairs raw-tags))
      (let* ((name (org-glance-config--tag-name (car pair))) (raw (org-glance-config--object (cdr pair))))
        (unless (and (org-glance-config--tag name) (not (gethash name seen)))
          (org-glance-config--error "Invalid or ambiguous case-folded tag name"))
        (puthash name t seen)
        (org-glance-config--known raw '("title" "template" "todoCycles" "storage"))
        (let ((tag-cycles (org-glance-config--cycles (org-glance-config--field raw "todoCycles" #'vectorp [])))
              (policy (org-glance-config--field raw "storage"
                                                (lambda (v) (member v '("local" "portable"))))))
          (when (and (equal name "notification") (equal policy "portable"))
            (org-glance-config--error "notification requires local family storage"))
          (when (equal policy "local") (push name storage))
          (push (list name
                      (org-glance-tag-config :tag (intern name)
                                             :title (org-glance-config--field raw "title" #'stringp)
                                             :template (org-glance-config--field raw "template" #'stringp)
                                             :todo (mapconcat (lambda (cycle) (string-join
                                                                               (append (car cycle) '("|") (cdr cycle)) " ")) tag-cycles "\n"))
                      tag-cycles) tags))))
    (maphash (lambda (key _) (unless (member key '("format" "tree" "todoCycles" "tags" "views"
                                                   "template" "stateColors" "application" "actions" "datasets" "clients"))
                               (push (concat "unknown setting: " key) diagnostics))) object)
    (let ((views (org-glance-config--field object "views" #'hash-table-p (make-hash-table :test 'equal)))
          (colors (org-glance-config--field object "stateColors" #'hash-table-p (make-hash-table :test 'equal))))
      (org-glance-config--known views '("default" "agenda" "archive" "notifications"))
      (maphash (lambda (_ value) (unless (stringp value) (org-glance-config--error "Invalid view query"))) views)
      (maphash (lambda (_ pairs)
                 (org-glance-config--object pairs)
                 (maphash (lambda (_ value) (unless (stringp value)
                                              (org-glance-config--error "Invalid state color"))) pairs)) colors)
      (org-glance-settings--make :tree tree :cycles cycles
                                 :template (org-glance-config--field object "template" #'stringp)
                                 :tags (sort tags (lambda (a b) (string< (car a) (car b))))
                                 :storage (sort (delete-dups (cons "notification" storage)) #'string<)
                                 :views views :colors colors :diagnostics (sort diagnostics #'string<)))))

(defun org-glance-config:keywords (settings file tags)
  "Resolve SETTINGS with FILE's declared keyword pair and ordered TAGS."
  (let* ((system (org-glance-config--names (org-glance-settings:cycles settings)))
         (tag-settings (org-glance-settings:tags settings))
         (all (cons system (mapcar (lambda (tag) (org-glance-config--names (nth 2 tag))) tag-settings)))
         (recognition (delete-dups (append (mapcan (lambda (kw) (copy-sequence (car kw))) all)
                                           (mapcan (lambda (kw) (copy-sequence (cdr kw))) all)))))
    (org-glance-tag-config:resolve-keywords
     (append (list system)
             (mapcar (lambda (tag) (org-glance-config--names (nth 2 (assoc (org-glance-config--tag-name tag) tag-settings)))) tags)
             (list file)) recognition)))

(defun org-glance-config:header (settings file tags &optional declarations)
  "Render SETTINGS' effective FILE and TAGS declaration with DECLARATIONS metadata."
  (let* ((keywords (org-glance-config:keywords settings file tags))
         (cycles (append (org-glance-settings:cycles settings)
                         (mapcan (lambda (tag) (copy-sequence (nth 2
                                                                   (assoc (org-glance-config--tag-name tag) (org-glance-settings:tags settings))))) tags)))
         (tokens (append (apply #'append (mapcar (lambda (c) (append (car c) (cdr c))) cycles))
                         (mapcan (lambda (declaration) (split-string (nth 2 declaration))) declarations)))
         (decorate (lambda (name) (or (cl-find-if
                                       (lambda (token) (and (not (equal token "|"))
                                             (equal name (org-glance-config--token token))
                                             (not (equal name token)))) tokens) name))))
    (concat "#+TODO: " (string-join (append (mapcar decorate (car keywords)) '("|")
                                            (mapcar decorate (cdr keywords))) " ") "\n")))

(defun org-glance-config--fingerprint (value)
  "Hash VALUE with Aeson's control escapes; exact file revisions stay separate."
  (secure-hash
   'sha256
   (replace-regexp-in-string
    "\\\\\\(?:[\"\\\\/bfnrt]\\|u[0-9A-Fa-f]\\{4\\}\\)"
    (lambda (escape)
      (cond ((equal escape "\\b") "\\u0008")
            ((equal escape "\\f") "\\u000c")
            ((string-prefix-p "\\u" escape) (downcase escape))
            (t escape)))
    (substring (org-glance-config-file--serialize value) 0 -1) t t)))

(defun org-glance-config:fingerprints (settings)
  "Return SETTINGS' semantic dependency fingerprints separately from revisions."
  (let* ((system (org-glance-config--names (org-glance-settings:cycles settings)))
         (defaults (org-glance-tag-config:resolve-keywords nil nil))
         (tags (org-glance-settings:tags settings))
         (names (mapcar (lambda (tag) (cons (car tag) (org-glance-config--names (nth 2 tag)))) tags))
         (tree (org-glance-settings:tree settings)))
    (cl-labels ((pair (p) (vector (vconcat (car p)) (vconcat (cdr p))))
                (cycles (cs) (vconcat (mapcar #'pair cs)))
                (hash (version value) (org-glance-config--fingerprint (vector version value))))
      (list
       (hash "recognition/1" (vconcat (sort (delete-dups (append
                 (car defaults) (cdr defaults) (car system) (cdr system)
                 (mapcan (lambda (p) (append (cadr p) (copy-sequence (cddr p)))) names))) #'string<)))
       (hash "classification/1" (vector (pair defaults) (pair system)
                                        (vconcat (mapcar (lambda (p) (vector (car p) (pair (cdr p))))
                                                         (cl-remove-if-not (lambda (p) (or (cadr p) (cddr p))) names)))))
       (hash "policy/1" (vconcat (cons (vconcat (org-glance-settings:storage settings))
                                       (mapcar (lambda (k) (or (gethash k tree) :null))
                                               '("inboxTag" "headlineHistoryDepth" "repositoryAutosync" "repositoryRemote"
                                                 "googleOAuthClientId" "defaultAgent")))))
       (hash "presentation/2" (vector (cycles (org-glance-settings:cycles settings))
                                      (or (org-glance-settings:template settings) :null)
                                      (vconcat (mapcar (lambda (tag) (vector (car tag)
                                                                             (or (org-glance-tag-config:title (cadr tag)) :null)
                                                                             (or (org-glance-tag-config:template (cadr tag)) :null) (cycles (nth 2 tag)))) tags))
                                      (vconcat (mapcar (lambda (p) (vector (car p) (cdr p)))
                                                       (org-glance-config--pairs (org-glance-settings:views settings))))
                                      (vconcat (mapcar (lambda (p) (vector (car p)
                                                                           (vconcat (mapcar (lambda (q) (vector (car q) (cdr q))) (org-glance-config--pairs (cdr p))))))
                                                       (org-glance-config--pairs (org-glance-settings:colors settings))))))))))

(cl-defstruct (org-glance-org-presentation
               (:constructor org-glance-org-presentation--make)
               (:conc-name org-glance-org-presentation:))
  prefix body origin header)

(cl-defun org-glance-config:present (settings tags source &key (start 0) (end (length source)))
  "Project SETTINGS and TAGS onto SOURCE's half-open canonical START/END Span."
  (unless (and (stringp source) (integerp start) (integerp end)
               (<= 0 start end (length source)))
    (org-glance-config--error "Invalid canonical body Span"))
  (let* ((body (substring source start end))
         (declarations (org-glance-tag-config:todo-declarations source))
         (file (org-glance-tag-config:declaration-keywords declarations))
         (header (org-glance-config:header settings file tags declarations))
         (source (cl-some (lambda (declaration)
                            (and (<= start (car declaration)) (<= (cadr declaration) end))) declarations))
         (prefix (if source "" (if (string-match-p "\r\n" body)
                                   (replace-regexp-in-string "\n" "\r\n" header t t) header))))
    (org-glance-org-presentation--make :prefix prefix :body body :origin start :header header)))

(defun org-glance-config:insert-presentation (presentation)
  "Insert PRESENTATION into an empty buffer with read-only generated context."
  (unless (= (point-min) (point-max)) (org-glance-config--error "Presentation needs an empty buffer"))
  (let* ((header (org-glance-org-presentation:header presentation))
         (tokens (split-string (substring header (length "#+TODO: "))))
         (context (lambda (_) (cons 'sequence tokens))))
    (insert (org-glance-org-presentation:prefix presentation)
            (org-glance-org-presentation:body presentation))
    (let ((org-todo-setup-filter-hook (list context)))
      (delay-mode-hooks (org-mode)))
    (setq-local org-todo-setup-filter-hook (list context))
    (let ((end (+ (point-min) (length (org-glance-org-presentation:prefix presentation)))))
      (add-text-properties (point-min) end
                           '(read-only t front-sticky (read-only) rear-nonsticky (read-only)))
      (set-buffer-modified-p nil))))

(defun org-glance-config:compatible-header (scopes)
  "Return SCOPES' common Org header, refusing incompatible named contexts.
SCOPES contains (NAME . PRESENTATION) pairs."
  (if (null scopes) ""
    (let ((first (org-glance-org-presentation:header (cdar scopes))))
      (dolist (scope (cdr scopes))
        (unless (equal first (org-glance-org-presentation:header (cdr scope)))
          (org-glance-config--error
           (format "Incompatible Org context: %s and %s; active/done keywords, order or metadata differ; export compatible scopes separately"
                   (caar scopes) (car scope)))))
      first)))

(defun org-glance-config:canonical-body (presentation text)
  "Extract PRESENTATION's canonical body from TEXT, refusing a changed prefix."
  (let ((prefix (org-glance-org-presentation:prefix presentation)))
    (unless (string-prefix-p prefix text)
      (org-glance-config--error "Generated Org context is read-only"))
    (substring-no-properties text (length prefix))))

(cl-defun org-glance-config:map-edit (presentation &key start end text)
  "Map PRESENTATION's zero-based START/END edit of TEXT to its source Span."
  (let ((offset (length (org-glance-org-presentation:prefix presentation)))
        (body (org-glance-org-presentation:body presentation))
        (origin (org-glance-org-presentation:origin presentation)))
    (unless (and (integerp start) (integerp end) (<= offset start end)
                 (<= end (+ offset (length body))))
      (org-glance-config--error "Edit crosses the canonical body boundary"))
    (list (+ origin (- start offset)) (+ origin (- end offset)) text)))

(defun org-glance-config:native-keywords (settings file tags &optional declarations)
  "Resolve SETTINGS, FILE and TAGS while preserving native cycle boundaries.
DECLARATIONS retains source-owned cycle metadata."
  (let* ((meaning (org-glance-config:keywords settings file tags))
         (header (org-glance-config:header settings file tags declarations))
         (tokens (split-string (substring header (length "#+TODO: "))))
         (raw (append '((("TODO") . ("DONE")))
                      (org-glance-settings:cycles settings)
                      (mapcan (lambda (tag)
                                (copy-sequence
                                 (nth 2 (assoc (org-glance-config--tag-name tag)
                                               (org-glance-settings:tags settings))))) tags)
                      (mapcar
                       (lambda (declaration)
                         (let* ((words (split-string (nth 2 declaration)))
                                (bar (member "|" words)))
                           (cons (if bar (butlast words (length bar)) words) (cdr bar))))
                       declarations)))
         seen result)
    (cl-labels ((word (token) (car (org-remove-keyword-keys (list token))))
                (decorate (name) (or (cl-find-if (lambda (token) (equal name (word token))) tokens) name))
                (add (active inactive)
                  (when (or active inactive)
                    (setq active (mapcar #'word active) inactive (mapcar #'word inactive))
                    (setq seen (append active inactive seen))
                    (when (and active (null inactive))
                      (setq inactive (list (car (cdr meaning))))
                      (setq inactive (delq nil inactive)))
                    (push (cons 'sequence (append (mapcar #'decorate active) '("|")
                                                  (mapcar #'decorate inactive))) result))))
      (dolist (cycle raw)
        (add (cl-remove-if-not (lambda (token) (member (word token) (car meaning))) (car cycle))
             (cl-remove-if-not (lambda (token) (member (word token) (cdr meaning))) (cdr cycle))))
      (add (cl-set-difference (car meaning) seen :test #'equal)
           (cl-set-difference (cdr meaning) seen :test #'equal)))
    (nreverse result)))

(defvar-local org-glance-config--org-context nil
  "Function returning effective native TODO cycles for this Org buffer.")

(defvar org-glance-config--pending-context nil
  "Native TODO context inherited by scratch Org parsing buffers.")

(defun org-glance-config--collect-keywords (original keywords &rest arguments)
  "Supply derived TODO context to ORIGINAL for requested KEYWORDS and ARGUMENTS."
  (let ((values (apply original keywords arguments))
        (context (or org-glance-config--org-context org-glance-config--pending-context)))
    (if (and context
             (seq-intersection keywords '("TODO" "SEQ_TODO" "TYP_TODO") #'equal))
        (cons (cons "TODO" (mapcar (lambda (cycle) (string-join (cdr cycle) " "))
                                   (funcall context)))
              (cl-remove-if (lambda (entry) (member (car entry) '("TODO" "SEQ_TODO" "TYP_TODO"))) values))
      values)))

(advice-add 'org-collect-keywords :around #'org-glance-config--collect-keywords)

(defun org-glance-config:org-context (graph tags)
  "Return GRAPH and TAGS' native context function, or nil for legacy settings.
TAGS may be a function reading the current buffer's tag names."
  (when (org-glance-config:current graph)
    (lambda ()
      (let* ((settings (org-glance-config:current graph))
             (declarations (org-glance-tag-config:todo-declarations
                            (buffer-substring-no-properties (point-min) (point-max))))
             (file (org-glance-tag-config:declaration-keywords declarations)))
        (org-glance-config:native-keywords settings file
                                          (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) tag))
                                                  (if (functionp tags) (funcall tags) tags))
                                          declarations)))))

(cl-defstruct (org-glance-config--observation
               (:constructor org-glance-config--observe))
  snapshot settings key presentation)

(defvar org-glance-config-after-reload-functions nil
  "Functions called with GRAPH after a valid configuration replacement.")

(defun org-glance-config:current (graph)
  "Return GRAPH's published JSON settings, or nil on the legacy path."
  (when (org-glance-graph? graph)
    (when (eq :unread (org-glance-graph:-config graph))
      (org-glance-config:refresh graph))
    (when-let* ((observation (org-glance-graph:-config graph)))
      (org-glance-config--observation-settings observation))))

(defun org-glance-config:snapshot (graph)
  "Return GRAPH's published JSON write snapshot, or nil."
  (org-glance-config:current graph)
  (when-let* ((observation (org-glance-graph:-config graph)))
    (org-glance-config--observation-snapshot observation)))

(defun org-glance-config:cache-key (graph)
  "Return GRAPH's parsing dependency key, excluding presentation settings."
  (org-glance-config:current graph)
  (when-let* ((observation (org-glance-graph:-config graph)))
    (org-glance-config--observation-key observation)))

(defun org-glance-config:presentation-key (graph)
  "Return GRAPH's prepared presentation fingerprint, or nil for legacy settings."
  (org-glance-config:current graph)
  (when-let* ((observation (org-glance-graph:-config graph)))
    (org-glance-config--observation-presentation observation)))

(defun org-glance-config:refresh (graph)
  "Observe GRAPH's JSON configuration and publish a valid coherent reload.
Cold errors signal.  Reload errors retain the last good settings and warn."
  (let ((previous (org-glance-graph:-config graph))
        (old-key (unless (eq :unread (org-glance-graph:-config graph))
                   (org-glance-config:cache-key graph))))
    (condition-case err
        (let* ((snapshot (org-glance-config-file:read (org-glance-graph:directory graph)))
               (unchanged (and (not (eq previous :unread))
                               (equal (and previous (org-glance-config-file:revision
                                                     (org-glance-config--observation-snapshot previous)))
                                      (org-glance-config-file:revision snapshot))))
               (settings (if unchanged
                             (and previous (org-glance-config--observation-settings previous))
                           (and (org-glance-config-file:revision snapshot)
                                (org-glance-config:decode snapshot))))
               (next (if unchanged previous
                       (and settings
                         (let ((fingerprints (org-glance-config:fingerprints settings)))
                          (org-glance-config--observe
                           :snapshot snapshot :settings settings
                           :presentation (nth 3 fingerprints)
                           :key (secure-hash 'sha256
                                             (prin1-to-string
                                              (cl-subseq fingerprints 0 2)))))))))
          (unless (and (not (eq previous :unread))
                       (equal (and previous (org-glance-config-file:revision
                                             (org-glance-config--observation-snapshot previous)))
                              (and next (org-glance-config-file:revision snapshot))))
            (setf (org-glance-graph:-config graph) next)
            (when (and (not (eq previous :unread))
                       (fboundp 'org-glance-cache:reconcile-inventory)
                       (file-exists-p (org-glance-cache:path graph))
                       (not (equal old-key (org-glance-config:cache-key graph))))
              (org-glance-cache:reconcile-inventory graph t))
            (when next
              (dolist (diagnostic (org-glance-settings:diagnostics settings))
                (display-warning 'org-glance diagnostic)))
            (unless (eq previous :unread)
              (with-demoted-errors "org-glance: configuration notification: %S"
                (run-hook-with-args 'org-glance-config-after-reload-functions graph)
                (run-hook-with-args 'org-glance-graph-after-commit-functions graph :structural))))
          (setf (org-glance-graph:-config-error graph) nil)
          settings)
      (error
       (setf (org-glance-graph:-config graph) previous)
       (if (eq previous :unread) (signal (car err) (cdr err))
         (let ((message (format "Configuration reload failed; previous settings retained: %s"
                                (error-message-string err))))
           (unless (equal message (org-glance-graph:-config-error graph))
             (display-warning 'org-glance message))
           (setf (org-glance-graph:-config-error graph) message)
           (when previous (org-glance-config--observation-settings previous))))))))

(cl-defun org-glance-config:set-tree-setting (graph key value &key expected)
  "Write known tree setting KEY to VALUE in GRAPH at revision EXPECTED."
  (unless (org-glance-config:current graph)
    (user-error "JSON activation requires a published config.json"))
  (unless (member key '("inboxTag" "headlineHistoryDepth" "repositoryAutosync"
                        "repositoryRemote" "googleOAuthClientId" "defaultAgent"))
    (org-glance-config--error "Unknown tree setting"))
  (let ((saved (org-glance-config-file:write
                (org-glance-graph:directory graph) :expected expected
                :change (lambda (object)
                          (let ((tree (gethash "tree" object)))
                            (unless tree
                              (setq tree (make-hash-table :test 'equal))
                              (puthash "tree" tree object))
                            (if (eq value :reset) (remhash key tree) (puthash key value tree))
                            (org-glance-config:decode
                             (org-glance-config-file--make :object object))
                            object)))))
    (org-glance-config:refresh graph)
    (when (org-glance-graph:-config-error graph)
      (user-error "JSON saved but live application failed: %s"
                  (org-glance-graph:-config-error graph)))
    saved))

(defun org-glance-config-set-tree-setting (key json-value)
  "Set KEY to JSON-VALUE in the current tree's JSON configuration."
  (interactive
   (list (completing-read "Tree setting: " '("inboxTag" "headlineHistoryDepth"
                                           "repositoryAutosync" "repositoryRemote"
                                           "googleOAuthClientId" "defaultAgent") nil t)
         (read-string "JSON value (or reset): ")))
  (let* ((graph (org-glance-ensure-init))
         (_ (org-glance-config:current graph))
         (snapshot (org-glance-config:snapshot graph)))
    (org-glance-config:set-tree-setting
     graph key (if (equal json-value "reset") :reset
                 (json-parse-string json-value :null-object :null :false-object :false))
     :expected (and snapshot (org-glance-config-file:revision snapshot)))))

(provide 'org-glance-config)
;;; org-glance-config.el ends here
