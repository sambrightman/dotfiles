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
    (frame-set-background-mode frame)))

(defun my/load-my-theme ()
  "Load theme `solarized' and apply extra definitions."
  (interactive)
  (require 'color-theme)
  (require 'color-theme-solarized)
  (load-theme 'solarized t)
  (require 'solarized-extra-definitions) ;; remove once re-installing the package
  (solarized-apply-definitions my/solarized-extra-definitions 'solarized))

(defun my/after-make-frame-functions-hook (frame)
  "Customization to apply theme to new FRAME."
  (with-selected-frame frame
    (unless window-system
      (my/set-background-mode my/theme-mode frame)
      (my/load-my-theme))))

(provide 'my-theme)
