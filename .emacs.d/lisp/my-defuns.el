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

(defvar my/default-emacs-source-directory (my/join-path (getenv "CODE_DIR") "emacs"))

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
  (require 'git)
  (when (git-repo? path)
    (let* ((git-repo path)
           (current-branch (git-on-branch))
           (current-tag (condition-case err
                            (git--clean (git-run "describe" "--tags"))
                          (git-error
                           (git-error "Repository not initialized"))))
           )
      (if (equal current-branch "HEAD")
          current-tag
        current-branch))))


(provide 'my-defuns)
