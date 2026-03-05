;;; my-find-friend.el --- Find and rank “friend” files near a buffer -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'f)

(defun my/find-friend--project-root (&optional dir)
  "Best-effort project root for DIR (or `default-directory`)."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (or
     (when (fboundp 'projectile-project-root)
       (ignore-errors
         (or (ignore-errors (projectile-project-root dir))
             (ignore-errors (let ((default-directory dir))
                              (projectile-project-root))))))
     (let ((proj (and (fboundp 'project-current) (project-current nil dir))))
       (when (and proj (fboundp 'project-root))
         (file-name-as-directory (project-root proj))))
     (when-let ((git (locate-dominating-file dir ".git")))
       (file-name-as-directory git))
     dir)))

;;;; Small path helpers

(defun my/find-friend--ext (file)
  (downcase (or (file-name-extension file) "")))

(defun my/find-friend--dir-basename (dir)
  (file-name-nondirectory (directory-file-name (file-name-as-directory dir))))

(defun my/find-friend--dir-in-names-p (dir names)
  (member (my/find-friend--dir-basename dir) names))

(defun my/find-friend--in-preferred-dir-p (file root preferred-dirs)
  "Non-nil if FILE is under a directory whose basename is in PREFERRED-DIRS."
  (let* ((rel (f-relative (expand-file-name file) (file-name-as-directory root)))
         (parts (split-string rel "/" t)))
    (-any? (lambda (p) (member p preferred-dirs)) parts)))

(defun my/find-friend--ancestors-up-to (dir stop-dir)
  "Return DIR, its parents, ... up to STOP-DIR (inclusive when reached)."
  (let* ((dir  (file-name-as-directory (expand-file-name dir)))
         (stop (file-name-as-directory (expand-file-name stop-dir)))
         (out nil)
         (cur dir))
    (while cur
      (push cur out)
      (if (equal cur stop)
          (setq cur nil)
        (setq cur (file-name-directory (directory-file-name cur)))))
    (nreverse out)))

(defun my/find-friend--reachable-preferred-dirs (base-dir root preferred-dirs)
  "Return list of preferred directories allowed by the strict rule.

Allowed:
- any directory named in PREFERRED-DIRS at/under BASE-DIR
- any *immediate child* of any ancestor of BASE-DIR (up to ROOT)
  whose basename is in PREFERRED-DIRS."
  (let* ((base-dir (file-name-as-directory (expand-file-name base-dir)))
         (root (file-name-as-directory (expand-file-name root)))
         (ancestors (my/find-friend--ancestors-up-to base-dir root))
         (dirs nil))
    ;; A) preferred dirs at/under base-dir (safe: no sideways)
    (dolist (p (directory-files-recursively base-dir ".*" t))
      (when (and (file-directory-p p)
                 (my/find-friend--dir-in-names-p p preferred-dirs))
        (push (file-name-as-directory p) dirs)))

    ;; B) preferred dirs as immediate children of ancestors
    (dolist (a ancestors)
      (dolist (child (directory-files a t directory-files-no-dot-files-regexp t))
        (when (and (file-directory-p child)
                   (my/find-friend--dir-in-names-p child preferred-dirs))
          (push (file-name-as-directory child) dirs))))

    (-uniq dirs)))

(defun my/find-friend--collect-bucket0-files (base-dir root preferred-dirs)
  "All regular files under reachable preferred dirs, recursively (no filtering)."
  (let* ((dirs (my/find-friend--reachable-preferred-dirs base-dir root preferred-dirs))
         (files (apply #'append
                       (-map (lambda (d) (directory-files-recursively d ".*" t)) dirs))))
    (-uniq (-filter #'file-regular-p files))))

(defun my/find-friend--collect-bucket1-files (base-dir root preferred-dirs preferred-extensions)
  "Non-sideways region: descendants of BASE-DIR recursively + ancestors (non-recursive).
Then filter by PREFERRED-EXTENSIONS, and exclude files under preferred dirs."
  (let* ((base-dir (file-name-as-directory (expand-file-name base-dir)))
         (root (file-name-as-directory (expand-file-name root)))
         (ancestors (my/find-friend--ancestors-up-to base-dir root))
         (files nil))
    (setq files (append files (directory-files-recursively base-dir ".*" t)))
    (dolist (a ancestors)
      (dolist (p (directory-files a t directory-files-no-dot-files-regexp t))
        (when (file-regular-p p)
          (push p files))))

    (setq files (-uniq (-filter #'file-regular-p files)))

    (-filter
     (lambda (ff)
       (and (member (my/find-friend--ext ff) preferred-extensions)
            (not (my/find-friend--in-preferred-dir-p ff root preferred-dirs))))
     files)))

(defun my/find-friend--extension-rank (file extensions)
  "Lower is better. Extensions not in EXTENSIONS get a large rank."
  (let ((idx (-elem-index (my/find-friend--ext file) extensions)))
    (if (numberp idx) idx 999)))

(defconst my/find-friend--score-exact    4000)
(defconst my/find-friend--score-boundary 3000)
(defconst my/find-friend--score-substr   2000)
(defconst my/find-friend--score-fuzzy    1000)

(defun my/find-friend--stem (file)
  "Basename without extension."
  (file-name-base file))

(defun my/find-friend--rel-dir-segments (rel)
  "Directory segments from a root-relative REL path (directory part only)."
  (let* ((dir (or (file-name-directory rel) "")))
    (split-string dir "/" t)))

(defun my/find-friend--boundary-substr-p (needle haystack)
  "Non-nil if NEEDLE occurs in HAYSTACK on token boundaries.
Boundaries are start/end or one of / . _ -."
  (let* ((n (regexp-quote needle)))
    (string-match-p
     (concat "\\(?:^\\|[/._-]\\)" n "\\(?:$\\|[/._-]\\)")
     haystack)))

(defun my/find-friend--fuzzy-threshold (needle)
  "Conservative fuzzy threshold for NEEDLE, or nil if fuzzy disabled.
We disable fuzzy for very short needles to avoid noisy matches."
  (let ((len (length needle)))
    (when (>= len 4)
      (min 4 (/ len 3)))))  ;; integer division

(defun my/find-friend--match-score (needle candidate)
  "Return a plist describing NEEDLE vs CANDIDATE.
Caller must lowercase both.

Return keys:
  :kind   one of exact|boundary|substr|fuzzy|none
  :score  integer, higher is better
  :d      edit distance (fuzzy only)
  :thr    threshold (fuzzy only)"
  (cond
   ((string= needle candidate)
    (list :kind 'exact :score my/find-friend--score-exact))

   ((my/find-friend--boundary-substr-p needle candidate)
    (list :kind 'boundary :score my/find-friend--score-boundary))

   ((string-match-p (regexp-quote needle) candidate)
    (list :kind 'substr :score my/find-friend--score-substr))

   ((and (fboundp 'string-distance)
         (let ((thr (my/find-friend--fuzzy-threshold needle)))
           (when thr
             (let ((d (string-distance needle candidate)))
               (when (<= d thr)
                 (list :kind 'fuzzy
                       :score (+ my/find-friend--score-fuzzy (- thr d))
                       :d d :thr thr)))))))

   (t
    (list :kind 'none :score 0))))

(defun my/find-friend--best-match (needle stem segments)
  "Best match plist for NEEDLE across STEM and SEGMENTS.
Adds :where describing the winning candidate ('stem or segment string)."
  (let* ((n (downcase (or needle "")))
         (stem (downcase (or stem "")))
         (segs (-map #'downcase (or segments '())))
         (best (list :kind 'none :score 0 :where 'none)))
    (unless (s-blank? n)
      (let* ((m (my/find-friend--match-score n stem)))
        (setq best (append m (list :where 'stem))))
      (dolist (seg segs)
        (let* ((m (my/find-friend--match-score n seg))
               (sc (plist-get m :score))
               (best-sc (plist-get best :score)))
          (when (> sc best-sc)
            (setq best (append m (list :where seg)))))))
    best))

(defun my/find-friend--debug-print (bucket-tier total ext-rank keys weights matches per-key-scores contribs rel file)
  "Print one candidate's full scoring breakdown to *Messages*."
  (message "%s"
           (s-join "\n"
                   (append
                    (list (format "bucket=%d total=%d ext-rank=%s" bucket-tier total ext-rank))
                    (-map-indexed
                     (lambda (i k)
                       (let* ((w (nth i weights))
                              (m (nth i matches))
                              (v (nth i per-key-scores))
                              (c (nth i contribs))
                              (kind (plist-get m :kind))
                              (where (plist-get m :where))
                              (d (plist-get m :d))
                              (thr (plist-get m :thr)))
                         (if (eq kind 'fuzzy)
                             (format "  %S: w=%d kind=%s where=%S d=%s thr=%s v=%d c=%d"
                                     k w kind where d thr v c)
                           (format "  %S: w=%d kind=%s where=%S v=%d c=%d"
                                   k w kind where v c))))
                     keys)
                    (list (format "  rel:  %s" rel)
                          (format "  file: %s" file))))))

(defun my/find-friend--score (file root keys preferred-extensions bucket-tier &optional debug)
  "Return a lexicographically comparable key for FILE. Smaller is better.
We compute higher-is-better scores then negate numeric components so that
`my/find-friend--lex<` can remain unchanged."
  (let* ((file (expand-file-name file))
         (rel  (f-relative file (file-name-as-directory root)))
         (stem (my/find-friend--stem file))
         (segs (my/find-friend--rel-dir-segments rel))
         (n (length keys))
         ;; linear weights: N..1
         (weights (-map (lambda (i) (- n i)) (number-sequence 0 (1- n))))
         (matches (-map (lambda (k) (my/find-friend--best-match k stem segs)) keys))
         (per-key-scores (-map (lambda (m) (plist-get m :score)) matches))
         (contribs (-zip-with #'* weights per-key-scores))
         (total (apply #'+ contribs))
         ;; Soft extension preference within preferred dirs (bucket 0 only).
         (ext-rank (if (= bucket-tier 0)
                       (my/find-friend--extension-rank file preferred-extensions)
                     0)))
    (when debug
      (my/find-friend--debug-print bucket-tier total ext-rank
                                   keys weights matches per-key-scores contribs
                                   rel file))
    ;; Lex sort key:
    ;; 1) bucket-tier (0 before 1)
    ;; 2) -total (bigger total => smaller negative => earlier)
    ;; 3) per-key contributions (negated), so earlier keys still matter
    ;; 4) ext-rank (bucket0 only, as before)
    ;; 5) relpath, abspath tie-breaks
    (append (list bucket-tier (- total))
            (-map #'- contribs)
            (list ext-rank rel file))))

(defun my/find-friend--lex< (a b)
  "Return non-nil if list A is lexicographically less than list B."
  (catch 'done
    (while (and a b)
      (let ((x (car a)) (y (car b)))
        (cond
         ((equal x y) nil)
         ((and (numberp x) (numberp y))
          (throw 'done (< x y)))
         ((and (stringp x) (stringp y))
          (throw 'done (string< x y)))
         ;; Mixed/other types: fall back to printed representation (deterministic)
         (t
          (throw 'done (string< (prin1-to-string x) (prin1-to-string y))))))
      (setq a (cdr a) b (cdr b)))
    ;; If all equal up to min length, shorter list wins.
    (< (length a) (length b))))

;;;###autoload
(cl-defun my/find-friend-files
    (&key base-dir preferred-words preferred-dirs preferred-extensions debug)
  "Find and rank ‘friend’ files for BASE-DIR within the project.

PREFERRED-DIRS is a list of directory basenames (e.g. (\"etc\" \"config\")).
PREFERRED-EXTENSIONS is a list of extensions (without dots) whose order is also used as a
soft preference for files under preferred dirs.

Ranking:
1) Files under reachable preferred dirs (bucket 0), then
2) Files in the non-sideways region with PREFERRED-EXTENSIONS (bucket 1).

Within each bucket:
- aggregate weighted matching against preferred words, using basename stem + directory segments,
  with boundary-substring matches ranking above plain substring, and fuzzy matches gated by threshold.
- soft extension preference within bucket 0.

Returns absolute file paths."
  (let* ((base-dir (file-name-as-directory (expand-file-name (or base-dir default-directory))))
         (root (my/find-friend--project-root base-dir))
         (preferred-dirs (or preferred-dirs '()))
         (preferred-extensions (or preferred-extensions '()))
         (keys (-filter (lambda (x) (and (stringp x) (not (s-blank? x))))
                        preferred-words))
         (bucket0 (my/find-friend--collect-bucket0-files base-dir root preferred-dirs))
         (bucket1 (my/find-friend--collect-bucket1-files base-dir root preferred-dirs preferred-extensions))
         (scored nil))
    (dolist (ff bucket0)
      (push (my/find-friend--score ff root keys preferred-extensions 0 debug) scored))
    (dolist (ff bucket1)
      (push (my/find-friend--score ff root keys preferred-extensions 1 debug) scored))
    (let* ((sorted (sort scored #'my/find-friend--lex<)))
      (-map (lambda (k) (car (last k))) sorted))))

(provide 'my-find-friend)
;;; my-find-friend.el ends here
