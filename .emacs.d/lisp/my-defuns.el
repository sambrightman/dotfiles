;;; my-defuns.el --- General purpose functions
;;; Commentary:
;;
;;; Code:

(require 'git)
(require 'shut-up)
(require 's)

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


(provide 'my-defuns)
;;; my-defuns.el ends here
