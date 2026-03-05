;;; my-find-friend.el --- Find and rank “friend” files near a buffer -*- lexical-binding: t; -*-

(require 'cl-lib)   ;; for cl-defun / &key
(require 'dash)
(require 's)
(require 'f)

;;;; Project root (Projectile-first, then project.el, then .git, then DIR)

(defun my/find-friend--project-root (&optional dir)
  "Best-effort project root for DIR (or `default-directory`)."
  (let* ((dir (file-name-as-directory (expand-file-name (or dir default-directory)))))
    (or
     ;; Projectile, if available. Some versions accept a DIR arg; some do not.
     (when (fboundp 'projectile-project-root)
       (ignore-errors
         (or (ignore-errors (projectile-project-root dir))
             (ignore-errors (let ((default-directory dir))
                              (projectile-project-root))))))
     ;; project.el
     (let ((proj (and (fboundp 'project-current) (project-current nil dir))))
       (when (and proj (fboundp 'project-root))
         (file-name-as-directory (project-root proj))))
     ;; fallback: nearest .git
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

;;;; Ancestors up to root

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

;;;; Tree-distance metric

(defun my/find-friend--tree-distance (dir-a dir-b)
  "Distance between DIR-A and DIR-B in directory tree (edge count)."
  (let* ((a (split-string (directory-file-name (expand-file-name dir-a)) "/" t))
         (b (split-string (directory-file-name (expand-file-name dir-b)) "/" t))
         (i 0)
         (max (min (length a) (length b))))
    (while (and (< i max) (string= (nth i a) (nth i b)))
      (setq i (1+ i)))
    (+ (- (length a) i) (- (length b) i))))

;;;; Similarity scoring

(defun my/find-friend--string-score0 (needle haystack)
  "Tuple (tier distance) where smaller is better.
tier: 0 exact token/segment match, 1 substring match, 2 fuzzy distance, 3 empty needle."
  (let* ((n (downcase (or needle "")))
         (h (downcase (or haystack ""))))
    (cond
     ((s-blank? n) (list 3 0))
     ;; exact match on a token boundary
     ((string-match-p (concat "\\(?:^\\|[/._-]\\)" (regexp-quote n) "\\(?:$\\|[/._-]\\)") h)
      (list 0 0))
     ((string-match-p (regexp-quote n) h)
      (list 1 0))
     (t
      (list 2 (if (fboundp 'string-distance)
                  (string-distance n h)
                9999))))))

(defun my/find-friend--string-score (needle haystack)
  (let* ((n (downcase (or needle "")))
         (h (downcase (or haystack ""))))
    (cond
     ((s-blank? n) (list 4 0))
     ((string-match-p
       (concat "\\(?:^\\|[/._-]\\)" (regexp-quote n) "\\(?:$\\|[/._-]\\)")
       h)
      (list 0 0))
     ((string-match-p (regexp-quote n) h)
      (list 1 0))
     ((and (fboundp 'string-distance)
           (let* ((thr (max 2 (/ (max (length n) (length h)) 1.5)))
                  (d (string-distance n h)))
             (and (<= d thr)
                  (list 2 d)))))
     (t (list 3 0)))))

(defun my/find-friend--similarity-tuple (keys basename relpath)
  "Similarity tuple where earlier KEYS dominate.
For each key, filename is scored before path."
  (apply #'append
         (-map (lambda (k)
                 (append (my/find-friend--string-score k basename)
                         (my/find-friend--string-score k relpath)))
               keys)))

;;;; Extension priority (bucket 1 only; derived from EXTENSIONS order)

(defun my/find-friend--extension-rank (file extensions)
  "Lower is better. Extensions not in EXTENSIONS get a large rank."
  (let ((idx (-elem-index (my/find-friend--ext file) extensions)))
    (if (numberp idx) idx 999)))

;;;; Preferred-dir discovery (your strict rule, Interpretation 1)

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

;;;; Candidate collection

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
    ;; Descendants
    (setq files (append files (directory-files-recursively base-dir ".*" t)))

    ;; Ancestors (non-recursive)
    (dolist (a ancestors)
      (dolist (p (directory-files a t directory-files-no-dot-files-regexp t))
        (when (file-regular-p p)
          (push p files))))

    (setq files (-uniq (-filter #'file-regular-p files)))

    ;; Filter to allowed preferred-extensions, and exclude anything under preferred dirs.
    (-filter
     (lambda (f)
       (and (member (my/find-friend--ext f) preferred-extensions)
            (not (my/find-friend--in-preferred-dir-p f root preferred-dirs))))
     files)))

;;;; Ranking + sort comparator

(defun my/find-friend--score (file root base-dir keys preferred-dirs preferred-extensions bucket-tier)
  "Return a lexicographically comparable key for FILE. Smaller is better."
  (let* ((file (expand-file-name file))
         (file-dir (file-name-as-directory (file-name-directory file)))
         (base-dir (file-name-as-directory (expand-file-name base-dir)))
         (rel (f-relative file (file-name-as-directory root)))
         (base (file-name-nondirectory file))
         (dist (my/find-friend--tree-distance base-dir file-dir))
         (sim (my/find-friend--similarity-tuple keys base rel))
         ;; Soft extension preference only for bucket 1:
         (ext-rank (if (= bucket-tier 0)
                       (my/find-friend--extension-rank file preferred-extensions)
                     0)))
    ;; Lexicographic sort key:
    ;; 1) bucket tier (0 preferred-dir files, 1 extension files)
    ;; 2) tree distance
    ;; 3) similarity tuple (per key; filename before path)
    ;; 4) extension rank (bucket 0 only; 0 for bucket 1)
    ;; 5) stable tie-break on relpath
    ;; 6) absolute path as last-resort tie-break
    (append (list bucket-tier dist) sim (list ext-rank rel file))))

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

(defun my/find-friend--debug-message-sorted (sorted keys)
  "Print SORTED score-keys to *Messages* with full scoring detail."
  (let* ((sim-len (* 4 (length keys))))
    (dolist (k sorted)
      ;; score key layout:
      ;; [bucket dist] [sim...] [ext-rank rel file]
      (let* ((bucket (nth 0 k))
             (dist   (nth 1 k))
             (sim    (-slice k 2 (+ 2 sim-len)))
             (ext    (nth (+ 2 sim-len) k))
             (rel    (nth (+ 3 sim-len) k))
             (file   (nth (+ 4 sim-len) k))
             (chunks (-partition 4 sim))
             (sim-lines
              (if keys
                  (-map (lambda (pair)
                          (pcase-let ((`(,word . ,c) pair))
                            (pcase-let ((`(,bt ,bd ,pt ,pd) c))
                              (format "  %S: base(%d,%d) path(%d,%d)"
                                      word bt bd pt pd))))
                        (-zip-pair keys chunks))
                '())))
        (message "%s"
                 (s-join "\n"
                         (append
                          (list (format "bucket=%d tree-dist=%d ext-rank=%s" bucket dist ext))
                          sim-lines
                          (list (format "  rel:  %s" rel)
                                (format "  file: %s" file)))))))))

;;;###autoload
(cl-defun my/find-friend-files
    (&key base-dir preferred-dirs preferred-words preferred-extensions debug)
  "Find and rank ‘friend’ files for BASE-DIR within the project.

PREFERRED-DIRS is a list of directory basenames (e.g. (\"etc\" \"config\")).
PREFERRED-EXTENSIONS is a list of extensions (without dots) whose order is also used as a
soft preference for files under PREFERRED-DIRS.

Ranking:
1) Files under reachable preferred dirs (bucket 0), then
2) Files in the non-sideways region with PREFERRED-EXTENSIONS (bucket 1).

Within each bucket:
- closeness in the directory tree (distance from BASE-DIR),
- then similarity to PREFERRED-WORDS... in order,
  scoring filename before root-relative path,
- with a soft extension preference within bucket 0.

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
    (dolist (f bucket0)
      (push (my/find-friend--score f root base-dir keys preferred-dirs preferred-extensions 0) scored))
    (dolist (f bucket1)
      (push (my/find-friend--score f root base-dir keys preferred-dirs preferred-extensions 1) scored))
    (let* ((sorted (sort scored #'my/find-friend--lex<)))
      (when :debug
        (my/find-friend--debug-message-sorted sorted keys))
      (-map (lambda (k) (car (last k))) sorted))))

(provide 'my-find-friend)
;;; my-find-friend.el ends here
