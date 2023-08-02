;;; my-defuns.el --- General purpose functions
;;; Commentary:
;;
;;; Code:

(require 'git)
(require 'shut-up)
(require 's)
(require 'conda)

(defun my/cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

;; potentially bind to C-x C-e
;; use C-M-x to evaluate normally (but only whole defun...)
(defun my/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defvar my/default-emacs-source-directory (my/join-path (getenv "DEV_DIR") "emacs"))

(defun my/setup-emacs-source-directory-interact ()
  "Interactively obtain inputs for `my/setup-emacs-source-directory' if necessary."
  (cond
   ((and (not (null current-prefix-arg))
         (= 4 (car current-prefix-arg)))
    (list my/default-emacs-source-directory t))
   ((and (not (null current-prefix-arg))
         (< 4 (car current-prefix-arg)))
    (require 'ido)
    (list
     (ido-read-directory-name "Enter repository path: " my/default-emacs-source-directory nil t)
     (let ((choices '(t nil)))
       (intern (completing-read (format "Force? (%s) " choices) choices nil t)))))
   (t (list my/default-emacs-source-directory nil))))

(defun my/setup-emacs-source-directory (&optional path force)
  "Set the Emacs source code directory to PATH.
The branch or tag will be checked against `emacs-major-version'
and `emacs-minor-version' unless FORCE is non-nil.
One prefix argument to FORCE, two prefix arguments to prompt for PATH and FORCE."
  (interactive (funcall 'my/setup-emacs-source-directory-interact))
  (let* ((path (or path my/default-emacs-source-directory))
         (version (format "emacs-%d.%d"
                          emacs-major-version
                          emacs-minor-version))
         (current-revision (my/git-branch-or-tag path)))
    (if (or force (equal current-revision version))
        (setq source-directory path)
      (warn "Emacs version is %s but %s is on %s" version path current-revision))))

(defun my/git-branch-or-tag (path)
  "Current branch of working tree PATH if not detached, or else current tag."
  (when (git-repo? path)
    (condition-case err
        (let* ((git-repo path)
               (current-branch (git-on-branch))
               (current-tag (git--clean (git-run "describe" "--tags"))))
          (if (equal current-branch "HEAD")
              current-tag
            current-branch))
      (git-error
       (warn "Repository not initialized")
       nil))))

(defun my/function--shut-up (func &rest args)
  "Silence FUNC(ARGS) with advice."
  (shut-up
    (apply func args)))

(defun my/silence-function (func)
  "Silences FUNC using `shut-up'."
  (interactive "aFunction: ")
  (advice-add func :around #'my/function--shut-up))

(defun my/map-and-set-key (map key command &optional prefix suffix)
  "Using keymap MAP `my/map-key' KEY then `define-key' to COMMAND.
PREFIX or SUFFIX can wrap the key when passing to `define-key'."
  (my/map-key key)
  (define-key map (kbd (concat prefix key suffix)) command))

(defun my/global-map-and-set-key (&rest args)
  "`my/map-and-set-key' in `current-global-map', passing ARGS."
  (apply 'my/map-and-set-key (current-global-map) args))

(defun my/map-key (key)
  "Map KEY from escape sequence \"\e[emacs-KEY\"."
  (define-key function-key-map (concat "\e[emacs-" key) (kbd key)))

;; currently doesn't really work, because of argument list mis-match
;; and e.g. misearch.el overrides the search function to the default.
(defun my/toggle-vr-global ()
  "Toggle use of `visual-regexp-steroids' in place of all normal regexp functions."
  (interactive)
  ;; (setq replace-re-search-function nil)
    (let ((replacements '((query-replace-regexp . vr/query-replace)
                          (replace-regexp . vr/replace))))
    (dolist (replacement replacements)
      (let ((orig (car replacement))
            (new (cdr replacement)))
        (if (advice-member-p new orig)
            (advice-remove orig new)
          (advice-add orig :override new)))))
    (if isearch-search-fun-function
        (setq isearch-search-fun-function nil)
      (setq isearch-search-fun-function 'vr--isearch-search-fun-function)))


;; platform-specific
(defun my/dash-at-point ()
  "Run `dash-at-point' or `zeal-at-point' depending on platform."
  (interactive)
  (cond
   ((eq system-type 'darwin)
    (dash-at-point))
   ((eq system-type 'gnu/linux)
    (zeal-at-point))
   (t nil)))


(defvar my/lsp-clangd-build-dir-globs '("build*" "out*")
  "Globs under `lsp-workspace-root' for candidate build directories.")

(defvar my/lsp-clangd-build-dir-find-ascending nil
  "Ascend from the current buffer's directory to find candidate build directories.
Terminates at `lsp-workspace-root'.  If nil, only `lsp-workspace-root' is used.")

(defvar my/lsp-clangd-build-dir-find-max-depth 2
  "Maximum depth to descend into candidate build directories.")

(defvar my/lsp-clangd--workspace-build-dirs (make-hash-table :test #'equal)
  "Hash table `lsp-workspace-root' -> selected build directory.")

(defun my/lsp-clangd--find-compile-commands-dir ()
  "Find the full directory path that contains compile_commands.json.
Descend into directories that match one of `my/lsp-clangd-build-dir-globs'
until `my/lsp-clangd-build-dir-find-max-depth'.  If nothing is found in
`lsp-workspace-root' and `my/lsp-clangd-build-dir-find-ascending' is non-nil,
ascend from buffer directory to `lsp-workspace-root' checking at each level.
Return nil if nothing is found."
  ;; TODO optionally ascend from (file-name-directory buffer-file-name) to (lsp-workspace-root). only if nothing found in root, or return all for user choice?
  (if-let ((workspace-root (lsp-workspace-root))
           (initial-dir (if my/lsp-clangd-build-dir-find-ascending
                            (file-name-directory buffer-file-name)
                          workspace-root)))
      (--mapcat (let ((candidate-depth (f-depth it)))
                  (directory-files-recursively it "\\`compile_commands\\.json\\'"
                                               nil
                                               (lambda (dir) (let ((depth (- (f-depth dir) candidate-depth)))
                                                               (<= depth my/lsp-clangd-build-dir-find-max-depth)))))
                (--mapcat (f-glob it initial-dir) my/lsp-clangd-build-dir-globs))
    (progn
      (lsp--warn "lsp-workspace-root is not set yet")
      nil)))

(defun my/lsp-clangd--select-compile-commands-dir ()
  "Select the full directory path that contains compile_commands.json.
Prompt user for multiple matches or return nil if not found."
  (or (gethash (lsp-workspace-root) my/lsp-clangd--workspace-build-dirs)
      (puthash (lsp-workspace-root)
               (let ((candidate-dirs (my/lsp-clangd--find-compile-commands-dir)))
                 (pcase candidate-dirs
                   ('nil nil)
                   (`(,unique-compile-commands-dir) (file-name-directory unique-compile-commands-dir))
                   (multiple-dirs (file-name-directory (completing-read
                                                        "Found compile_commands.json in several directories. Consider setting lsp-clients-clangd-args to avoid this lookup in the future. Which one do you want to use? "
                                                        multiple-dirs)))))
               my/lsp-clangd--workspace-build-dirs)))

(defun my/update-lsp-clangd-args-with-compile-commands-dir (orig-fun &rest args)
  "Modify `lsp-clients-clang-args' to locate compile_commands.json if necessary.
If --compile-commands-dir is already set, trust it.
Otherwise run a potentially interactive search for the directory
that contains compile_commands.json and then modify the variable during advice."
  (if-let ((not-already-set (--none? (s-starts-with? "--compile-commands-dir=" it) lsp-clients-clangd-args))
           (selected-dir (my/lsp-clangd--select-compile-commands-dir))
           (lsp-clients-clangd-args (cons (concat "--compile-commands-dir=" selected-dir) lsp-clients-clangd-args)))
      (progn
        (lsp--info "using compile commands from %s" selected-dir)
        (apply orig-fun args))
    (progn
      ;; (or (not-already-set (lsp--info "--compile-commands-dir set in lsp-clients-clangd-args")))
      ;; (or (selected-dir (lsp--warn "clangd without compile commands might not be able to handle complex projects")))
      (let ((lsp-clients-clangd-args lsp-clients-clangd-args))
        (apply orig-fun args)))))

(defun my/update-lsp-clangd-args-with-query-driver (orig-fun &rest args)
  "Modify `lsp-clients-clang-args' to locate compile_commands.json if necessary.
If --query-driver is already set, trust it.
Otherwise add CONDA_PREFIX/bin/* to it."
  ;; TODO allow adding to the comma-separated list
  (if-let ((not-already-set (--none? (s-starts-with? "--query-driver=" it) lsp-clients-clangd-args))
           (conda-prefix (getenv "CONDA_PREFIX"))
           (conda-env-bin-glob (f-join conda-prefix "bin" "*"))
           (lsp-clients-clangd-args (cons (concat "--query-driver=" conda-env-bin-glob) lsp-clients-clangd-args)))
      (progn
        (lsp--info "using query drivers %s" conda-env-bin-glob)
        (apply orig-fun args))
    (progn
      ;; (or (not-already-set (lsp--info "--query-driver set in lsp-clients-clangd-args")))
      ;; (or (conda-env (lsp--info "not in a conda environment")))
      (let ((lsp-clients-clangd-args lsp-clients-clangd-args))
        (apply orig-fun args)))))

(defun my/conda--infer-env-from-buffer (fallback result)
  "Fallback to guessing Conda environment name from project directory, or FALLBACK."
  (if (equal result "base")
      (or (my/conda-env-for-project-dir (project-root (project-current))) fallback result)
    result))

(defun my/conda-env-for-project-dir (project-root)
  "Find a matching Conda environment for a given PROJECT-ROOT."
  (let ((project-name (f-filename project-root)))
    (--first (cond
              ((equal it project-name) t)
              ((equal it (s-downcase project-name)) t))
             (conda-env-candidates))))

(defun my/jedi-fix-environment (&optional no-reset)
  "Restart Jedi server with correct Python environment."
  (progn
    (jedi:stop-server)
    (unless no-reset
      (setq conda-project-env-path nil))
    (conda-env-activate-for-buffer)
    (setq jedi:server-args nil)
    (jedi:import-python-el-settings-setup)
    (jedi:start-server)
    (jedi-mode 1)))

(defun my/lsp-clangd-restart-if-conda-env ()
  "Look for a matching conda environment, activate it and restart lsp."
  (if-let ((env (my/conda-env-for-project-dir (lsp-workspace-root))))
      (progn
        (conda-env-activate env)
        ;; https://github.com/emacs-lsp/lsp-mode/issues/2932
        ;; (setq lsp--session nil)
        ;; (setq lsp--request-cleanup-hooks (ht))
        (lsp-workspace-restart (lsp--read-workspace)))))

(defun my/remove-all-advice (symbol)
  "Remove all advice from SYMBOL."
  (advice-mapc
   (lambda (advice properties)
     (advice-remove symbol advice))
   symbol))


(provide 'my-defuns)
;;; my-defuns.el ends here
