;;; my-find-friend-test.el --- Tests for my-find-friend -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
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

    ;; bucket1 descendants
    "src/app/settings.toml"
    "src/app/settings.json"
    "src/app/dev.json"
    "src/app/prod.json"
    "src/app/qa.json"
    "src/app/username-alice.json"

    ;; filename-vs-path match
    "src/app/path/dev/irrelevant.json"
    "src/app/path/other/config-dev.json"

    ;; ancestor file
    "src/ancestor.json"
    "src/ancestor.txt"

    ;; sideways preferred dirs
    "services/serviceA/config/a.json"
    "foo/config/bar.json"
    "tests/config/test.json"

    ;; sideways non-preferred
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
             (cl-letf (((symbol-function 'my/find-friend--project-root)
                        (lambda (&optional _) (file-name-as-directory root))))
               ,@body)))
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

(defun my/find-friend-test--rel-index (files rel)
  (-elem-index rel (-map (lambda (p) (f-relative p default-directory)) files)))

(defun my/find-friend-test--should-come-before (files a b)
  (let ((ia (my/find-friend-test--index files a))
        (ib (my/find-friend-test--index files b)))
    (should (and ia ib))
    (should (< ia ib))))

;;;; Default knobs

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
           (i-settings (my/find-friend-test--index files "settings.json"))
           (i-global (my/find-friend-test--index files "global.json")))
      (should (and i-settings i-global))
      (should (< i-global i-settings)))))

;;;; C. Extension priority within preferred dirs

(ert-deftest find-friend:preferred-dir-extension-soft-priority ()
  (my/find-friend-test--with-canonical-project
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

;;;; D. Extension filtering

(ert-deftest find-friend:bucket1-extension-filtering ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions '("json" "toml"))))
      (should (my/find-friend-test--has-basename files "settings.toml"))
      (should-not (my/find-friend-test--has-basename files "ancestor.txt")))))

(ert-deftest find-friend:bucket0-not-filtered-by-extensions ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs my/find-friend-test--preferred-dirs
                  :preferred-extensions '("json"))))
      (should (my/find-friend-test--has-basename files "global.sh")))))

;;;; E. Similarity ordering

(ert-deftest find-friend:similarity-ordering-variants ()
  (my/find-friend-test--with-canonical-project
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
                              args)))
          (should (string= expect (my/find-friend-test--first-basename files))))))))

;;;; F. Ancestor inclusion

(ert-deftest find-friend:bucket1-includes-ancestors-nonrecursive ()
  (my/find-friend-test--with-canonical-project
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs '()
                  :preferred-extensions my/find-friend-test--preferred-extensions)))
      (should (my/find-friend-test--has-basename files "ancestor.json")))))

;;;; G. Determinism + edge behaviours

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

(ert-deftest find-friend:tie-breaks-by-relpath ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "etc/tie-a.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "etc/tie-b.json" default-directory))
    (let ((files (my/find-friend-test--discover
                  :preferred-dirs '("etc")
                  :preferred-extensions '("json"))))
      (my/find-friend-test--should-come-before files "tie-a.json" "tie-b.json"))))

;;;; H. New scoring semantics

(ert-deftest find-friend:v2-segment-match-can-win-with-generic-filename ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/http_server/run.cfg" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/app/other/run.cfg" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("http_server")
                   :preferred-dirs '()
                   :preferred-extensions '("cfg")))
           (top (car files)))
      (should (string-prefix-p "src/app/http_server/" (f-relative top default-directory))))))

(ert-deftest find-friend:v2-boundary-substring-beats-plain-substring ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/dev/settings.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/app/development/settings.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("settings" "dev")
                   :preferred-dirs '()
                   :preferred-extensions '("json")))
           (top (car files)))
      (should (string= "src/app/dev/settings.json"
                       (f-relative top default-directory))))))

(ert-deftest find-friend:v2-many-later-matches-can-beat-single-early-match ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/x-alpha-x.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/app/gamma/beta.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("alpha" "beta" "gamma")
                   :preferred-dirs '()
                   :preferred-extensions '("json")))
           (top (car files)))
      (should (string= "src/app/gamma/beta.json"
                       (f-relative top default-directory))))))

;;;; I. Additional scoring guarantees

(ert-deftest find-friend:v2-substring-beats-fuzzy ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/http_server/zzprec.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/app/httpXserver/zzprec.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("zzprec" "http_server")
                   :preferred-dirs '()
                   :preferred-extensions '("json")))
           (rels (-map (lambda (p) (f-relative p default-directory)) files)))
      (should (< (-elem-index "src/app/http_server/zzprec.json" rels)
                 (-elem-index "src/app/httpXserver/zzprec.json" rels))))))

(ert-deftest find-friend:v2-fuzzy-distance-orders-within-fuzzy ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/sever/zzfuz.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/app/saver/zzfuz.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("zzfuz" "server")
                   :preferred-dirs '()
                   :preferred-extensions '("json")))
           (rels (-map (lambda (p) (f-relative p default-directory)) files)))
      (should (< (-elem-index "src/app/sever/zzfuz.json" rels)
                 (-elem-index "src/app/saver/zzfuz.json" rels))))))

(ert-deftest find-friend:v2-does-not-match-across-path-boundaries ()
  (my/find-friend-test--with-canonical-project
    (my/find-friend-test--touch (expand-file-name "src/app/ab/cd/zzpath.json" default-directory))
    (my/find-friend-test--touch (expand-file-name "src/app/xx/yy/zzpath.json" default-directory))
    (let* ((files (my/find-friend-test--discover
                   :preferred-words '("zzpath" "bc")
                   :preferred-dirs '()
                   :preferred-extensions '("json")))
           (rels (-map (lambda (p) (f-relative p default-directory)) files)))
      (should (< (-elem-index "src/app/ab/cd/zzpath.json" rels)
                 (-elem-index "src/app/xx/yy/zzpath.json" rels))))))

(provide 'my-find-friend-test)
;;; my-find-friend-test.el ends here
