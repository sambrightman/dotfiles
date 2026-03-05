;;; my-find-friend.el --- Tests for my-find-friend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)   ;; only for a couple of tiny conveniences in tests; easy to remove if you want
(require 'dash)
(require 's)
(require 'f)

(require 'my-find-friend)

;;;; Test helpers

(defun my/find-friend-test--mkdirp (dir)
  (make-directory dir t))

(defun my/find-friend-test--touch (file)
  (f-mkdir-full-path (f-dirname file))
  (f-touch file))

(defun my/find-friend-test--canonical-tree ()
  "Return a list of relative file paths that cover the key traversal + ranking cases."
  '(
    ;; buffer location
    "src/app/main.py"

    ;; preferred dirs reachable from ancestor immediate children
    "etc/global.sh"
    "etc/global.json"
    "etc/global.yaml"
    "etc/dev.json"
    "etc/prod.json"
    "etc/qa.json"
    "etc/username-alice.env"
    "etc/nested/deeper/service.toml"

    "config/global.toml"
    "config/dev.env"
    "config/prod.env"

    ;; preferred dirs below buffer
    "src/app/etc/local.env"
    "src/app/etc/local.sh"
    "src/app/config/local.json"

    ;; bucket1 (non-preferred dir) descendants
    "src/app/settings.toml"
    "src/app/settings.json"
    "src/app/dev.json"
    "src/app/prod.json"
    "src/app/qa.json"
    "src/app/username-alice.json"

    ;; filename-vs-path match: path contains dev, filename doesn't
    "src/app/path/dev/irrelevant.json"
    ;; path contains config, filename contains config too (stronger filename match)
    "src/app/path/other/config-dev.json"

    ;; ancestor file (bucket1 includes ancestors non-recursively)
    "src/ancestor.json"
    "src/ancestor.txt"

    ;; sideways preferred dirs (must be excluded by strict rule)
    "services/serviceA/config/a.json"
    "foo/config/bar.json"
    "tests/config/test.json"

    ;; sideways non-preferred (must be excluded by non-sideways rule)
    "src/util/helpers.json"
    "dev/sideways.env"
    ))

(defmacro my/find-friend-test--with-canonical-project (&rest body)
  (declare (indent 0))
  `(let ((root (make-temp-file "find-friend-" t)))
     (unwind-protect
         (progn
           (dolist (p (my/find-friend-test--canonical-tree))
             (my/find-friend-test--touch (expand-file-name p root)))
           (let ((default-directory (file-name-as-directory root)))
             ;; Make project root deterministic for tests
             (cl-letf (((symbol-function 'my/find-friend--project-root)
                        (lambda (&optional _dir) (file-name-as-directory root))))
               ,@body)))
       ;; Be robust even if a test left buffers open, etc.
       (ignore-errors (delete-directory root t)))))

(defun my/find-friend-test--base-dir ()
  (expand-file-name "src/app/" default-directory))

(defun my/find-friend-test--discover (&rest plist)
  (apply #'my/find-friend-files
         :base-dir (my/find-friend-test--base-dir)
         plist))

(defun my/find-friend-test--names (files)
  (-map #'file-name-nondirectory files))

(defun my/find-friend-test--has-basename (files basename)
  (-any? (lambda (f) (string= (file-name-nondirectory f) basename)) files))

(defun my/find-friend-test--index (files basename)
  (-elem-index basename (my/find-friend-test--names files)))

(defun my/find-friend-test--first-basename (files)
  (car (my/find-friend-test--names files)))

;;;; Default knobs used in most tests

(defconst my/find-friend-test--preferred-dirs '("etc" "config"))
(defconst my/find-friend-test--preferred-extensions
  '("json" "yaml" "yml" "toml" "env" "ini" "cfg" "conf"))

;;;; A. Traversal rules

(ert-deftest find-friend:preferred-ancestor-dirs-included-recursive ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should (my/find-friend-test--has-basename files "global.json"))
      (should (my/find-friend-test--has-basename files "service.toml")))))

(ert-deftest find-friend:preferred-dirs-below-buffer-included ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should (my/find-friend-test--has-basename files "local.env"))
      (should (my/find-friend-test--has-basename files "local.sh"))
      (should (my/find-friend-test--has-basename files "local.json")))))

(ert-deftest find-friend:sideways-preferred-dirs-excluded ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should-not (my/find-friend-test--has-basename files "a.json"))
      (should-not (my/find-friend-test--has-basename files "bar.json"))
      (should-not (my/find-friend-test--has-basename files "test.json")))))

(ert-deftest find-friend:sideways-nonpreferred-files-excluded ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should-not (my/find-friend-test--has-basename files "helpers.json"))
      (should-not (my/find-friend-test--has-basename files "sideways.env")))))

;;;; B. Bucket ordering

(ert-deftest find-friend:preferred-bucket-before-extension-bucket ()
  (my/find-friend-test--with-canonical-project
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("settings")
                   :preferred-dirs my/find-friend-test--preferred-dirs
                   :preferred-extensions my/find-friend-test--preferred-extensions))
           (i-preferred (my/find-friend-test--index files "settings.json"))
           (i-preferred2 (my/find-friend-test--index files "global.json")))
      ;; At least one preferred-dir candidate should come before bucket1 matches.
      (should (and i-preferred i-preferred2))
      (should (< i-preferred2 i-preferred)))))

;;;; C. Extension priority within preferred dirs (soft preference)

(ert-deftest find-friend:preferred-dir-extension-soft-priority ()
  (my/find-friend-test--with-canonical-project
    ;; For the "global.*" family in etc/, prefer json/yaml over sh
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("global")
                   :preferred-dirs my/find-friend-test--preferred-dirs
                   :preferred-extensions my/find-friend-test--preferred-extensions))
           (ij (my/find-friend-test--index files "global.json"))
           (iy (my/find-friend-test--index files "global.yaml"))
           (is (my/find-friend-test--index files "global.sh")))
      (should (and ij iy is))
      (should (< ij is))
      (should (< iy is)))))

(ert-deftest find-friend:extension-priority-does-not-beat-strong-similarity ()
  (my/find-friend-test--with-canonical-project
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("local.sh")
                   :preferred-dirs my/find-friend-test--preferred-dirs
                   :preferred-extensions my/find-friend-test--preferred-extensions))
           (top (my/find-friend-test--first-basename files)))
      (should (string= "local.sh" top)))))

;;;; D. Extension filtering applies only to bucket1

(ert-deftest find-friend:bucket1-extension-filtering ()
  (my/find-friend-test--with-canonical-project
    (let* ((files (my/find-friend-test--discover
                   :preferred-dirs my/find-friend-test--preferred-dirs
                   :preferred-extensions '("json" "toml"))))
      (should (my/find-friend-test--has-basename files "settings.toml"))
      ;; src/ancestor.txt is not in preferred dirs and should be filtered out
      (should-not (my/find-friend-test--has-basename files "ancestor.txt")))))

(ert-deftest find-friend:bucket0-not-filtered-by-extensions ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions '("json"))))
      ;; global.sh is in preferred dir, should still appear
      (should (my/find-friend-test--has-basename files "global.sh")))))

;;;; E. Similarity ordering (table-driven)

(ert-deftest find-friend:similarity-ordering-variants ()
  (my/find-friend-test--with-canonical-project
    ;; Disable preferred dirs so we can focus purely on bucket1 ranking.
    (let ((cases
           '((:args (:preferred-words ("username-alice"))  :expect "username-alice.json")
             (:args (:preferred-words ("alice"))           :expect "username-alice.json")
             (:args (:preferred-words ("dev" "qa" "prod")) :expect "dev.json"))))
      (dolist (case cases)
        (let* ((args   (plist-get case :args))
               (expect (plist-get case :expect))
               (files  (apply #'my/find-friend-files
                              :base-dir (my/find-friend-test--base-dir)
                              :preferred-dirs '()
                              :preferred-extensions my/find-friend-test--preferred-extensions
                              args))
               (top (my/find-friend-test--first-basename files)))
          (should (string= expect top)))))))

;;;; F. Ancestor inclusion in bucket1 (non-recursive)

(ert-deftest find-friend:bucket1-includes-ancestors-nonrecursive ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs '()   ;; focus on bucket1
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should (my/find-friend-test--has-basename files "ancestor.json"))
      ;; but not the sideways stuff already tested
      )))

;;;; G. Determinism + edge behaviors

(ert-deftest find-friend:deterministic ()
  (my/find-friend-test--with-canonical-project
    (let ((a (my/find-friend-test--discover
              :preferred-words '("alice" "dev" "qa" "prod")
              :preferred-dirs my/find-friend-test--preferred-dirs
              :preferred-extensions my/find-friend-test--preferred-extensions))
          (b (my/find-friend-test--discover
              :preferred-words '("alice" "dev" "qa" "prod")
              :preferred-dirs my/find-friend-test--preferred-dirs
              :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should (equal a b)))))

(ert-deftest find-friend:empty-extensions-yields-only-preferred-bucket ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions '())))
      ;; bucket0 should still contribute
      (should (my/find-friend-test--has-basename files "global.json"))
      ;; bucket1 should not contribute (e.g. settings.json is not in preferred dirs)
      (should-not (my/find-friend-test--has-basename files "settings.json")))))

(ert-deftest find-friend:no-preferred-dirs-yields-only-extension-bucket ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs '()
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should (my/find-friend-test--has-basename files "settings.json"))
      ;; preferred-dir files should NOT appear if preferred-dirs is empty
      (should-not (my/find-friend-test--has-basename files "global.json")))))

;; TODO move to right section?
(ert-deftest find-friend:closest-preferred-dir-wins ()
  (let ((my/find-friend-test--preferred-dirs '("etc"))
        (my/find-friend-test--preferred-extensions '("json")))
    (my/find-friend-test--with-canonical-project
      ;; Add a second dev.json in the closer preferred dir below the buffer.
      (my/find-friend-test--touch
       (expand-file-name "src/app/etc/dev.json" default-directory))
      (let ((files (my/find-friend-test--discover
                    :preferred-words '("dev")
                    :preferred-dirs my/find-friend-test--preferred-dirs
                    :preferred-extensions my/find-friend-test--preferred-extensions)))
        (should (string= "dev.json" (my/find-friend-test--first-basename files)))
        ;; And ensure the top one is the nearer path (root-relative).
        (let* ((top (car files))
               (rel (f-relative top default-directory)))
          (should (string-prefix-p "src/app/etc/" rel)))))))

(ert-deftest find-friend:tie-breaks-by-relpath ()
  (my/find-friend-test--with-canonical-project
    ;; Ensure a controlled tie: two JSON files in the same preferred dir, no keys.
    (my/find-friend-test--touch (expand-file-name "etc/tie-a.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "etc/tie-b.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-dirs '("etc")
                   :preferred-extensions '("json")))
           (ia (my/find-friend-test--index files "tie-a.json"))
           (ib (my/find-friend-test--index files "tie-b.json")))
      (should (and ia ib))
      (should (< ia ib)))))

(ert-deftest find-friend:case-insensitive-matching ()
  (my/find-friend-test--with-canonical-project
    (let* ((etc-dir (expand-file-name "etc/" default-directory))
           (lower (expand-file-name "etc/dev.json" default-directory))
           (upper (expand-file-name "etc/DEV.JSON" default-directory)))
      ;; Attempt to create the uppercase variant
      (my/find-friend-test--touch upper)

      ;; On case-insensitive filesystems, LOWER and UPPER are the same entry.
      (let ((entries (directory-files etc-dir nil directory-files-no-dot-files-regexp t)))
        (unless (and (member "dev.json" entries) (member "DEV.JSON" entries))
          (ert-skip "Filesystem is case-insensitive; cannot have dev.json and DEV.JSON simultaneously.")))

      (let* ((files (my/find-friend-test--discover
                     :preferred-words '("dev")
                     :preferred-dirs '("etc")
                     :preferred-extensions '("json")))
             (names (my/find-friend-test--names files)))
        (should (equal (seq-take names 2) '("dev.json" "DEV.JSON")))))))

(ert-deftest find-friend:symlinked-sideways-dir-not-traversed ()
  (my/find-friend-test--with-canonical-project
    ;; Create a sideways target containing a tempting file.
    (my/find-friend-test--touch
     (expand-file-name "services/serviceA/config/through-link.json" default-directory))

    ;; Create a symlink inside the buffer subtree pointing sideways.
    (let* ((link (expand-file-name "src/app/etc-link" default-directory))
           (target (expand-file-name "services/serviceA/config" default-directory)))
      (condition-case _
          (make-symbolic-link target link t)
        (file-error (ert-skip "Symlinks not supported on this system / permissions.")))

      ;; The sideways file must not appear (we should not traverse the symlinked dir).
      (let ((files (my/find-friend-test--discover
                    :preferred-dirs '("etc" "config")
                    :preferred-extensions '("json"))))
        (should-not (my/find-friend-test--has-basename files "through-link.json"))))))

(ert-deftest find-friend:all-nil-returns-nothing ()
  (my/find-friend-test--with-canonical-project
    (should (null (my/find-friend-files
                   :base-dir (my/find-friend-test--base-dir)
                   :preferred-words nil
                   :preferred-dirs nil
                   :preferred-extensions nil)))))

(ert-deftest find-friend:blank-keys-are-ignored ()
  (my/find-friend-test--with-canonical-project
    (let* ((base (my/find-friend-test--discover
                  :preferred-words nil
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions my/find-friend-test--preferred-extensions))
           (blank (my/find-friend-test--discover
                   :preferred-words '("" "   " nil)
                   :preferred-dirs my/find-friend-test--preferred-dirs
                   :preferred-extensions my/find-friend-test--preferred-extensions)))
      ;; Compare a prefix to avoid brittleness if the tail grows.
      (should (equal (-take 15 (my/find-friend-test--names base))
                     (-take 15 (my/find-friend-test--names blank)))))))

(ert-deftest find-friend:preferred-dirs-are-segment-only ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch
     (expand-file-name "src/app/configuration/should-not-match.sh" default-directory))
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs '("config")
                  :preferred-extensions '()))) ;; ensures bucket1 contributes nothing
      (should-not (my/find-friend-test--has-basename files "should-not-match.sh")))))

(ert-deftest find-friend:ancestor-descent-is-immediate-child-only ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch
     (expand-file-name "some/etc/blocked.json" default-directory))
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs '("etc" "config")
                  :preferred-extensions '("json"))))
      (should-not (my/find-friend-test--has-basename files "blocked.json")))))

(ert-deftest find-friend:distance-beats-similarity ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/aaa.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/target.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("target")
                   :preferred-dirs '()          ;; bucket1 only
                   :preferred-extensions '("json")))
           (names (my/find-friend-test--names files)))
      (should (equal (car names) "aaa.json"))
      (should (> (my/find-friend-test--index files "target.json")
                 (my/find-friend-test--index files "aaa.json"))))))

(ert-deftest find-friend:preferred-dir-detection-works-on-windows-paths ()
  (my/find-friend-test--with-canonical-project
    (let* ((root (file-name-as-directory (expand-file-name default-directory)))
           (file (expand-file-name "etc/wincheck.json" root)))
      (my/find-friend-test--touch file)
      (should (my/find-friend--in-preferred-dir-p file root '("etc"))))))

(provide 'my-find-friend-test)
;;; my-find-friend.el ends here
