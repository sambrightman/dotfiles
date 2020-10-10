;;; my-theme.el --- Theming
;;; Commentary:
;;
;;; Code:

(tool-bar-mode -1)

(defvar my/theme 'solarized)
(defvar my/theme-window-loaded nil)
(defvar my/theme-terminal-loaded nil)

(defconst my/theme-mode
  (if (file-exists-p (my/join-path my/lisp-directory "light"))
      'light
      'dark))

(defun my/set-background-mode (mode &optional frame)
  "Set MODE as the background mode of FRAME.
MODE should be 'dark', 'light' or 'auto'.
If FRAME is omitted or nil it defaults to `selected-frame'."
  (interactive "SEnter 'dark', 'light' or 'auto': ")
  (if (not (member mode '(dark light auto)))
      (error "Invalid mode %s" mode)
    (when (eq mode 'auto)
      (setq mode nil))
    (or frame (setq frame (selected-frame)))
    (set-terminal-parameter frame 'background-mode mode)
    (setq frame-background-mode mode)
    (frame-set-background-mode frame)
    (setq-default vc-annotate-background-mode nil)
    (setq-default vc-annotate-background nil)))

(defun my/load-my-theme (&optional frame)
  "Load theme `solarized' and apply extra definitions.
If FRAME is omitted or nil it defaults to `selected-frame'."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (if (window-system frame)
      (unless my/theme-window-loaded
        (if my/theme-terminal-loaded
            (enable-theme my/theme)
          (load-theme my/theme t))
        (setq my/theme-window-loaded t))
    (unless my/theme-terminal-loaded
      (if my/theme-window-loaded
          (enable-theme my/theme)
        (load-theme my/theme t))
      (setq my/theme-terminal-loaded t)))
  (require 'solarized-extra-definitions) ;; remove once re-installing the package
  (solarized-apply-definitions my/solarized-extra-definitions 'solarized))

(defun my/after-make-frame-functions-hook (frame)
  "Customization to apply theme to new FRAME."
  (with-selected-frame frame
    (my/set-background-mode my/theme-mode frame)
    (my/load-my-theme frame)))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'my/after-make-frame-functions-hook)
  (my/after-make-frame-functions-hook (selected-frame)))

(provide 'my-theme)
;;; my-theme.el ends here
