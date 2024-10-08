;;; homebrew-mode-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from homebrew-mode.el

(autoload 'homebrew-tap "homebrew-mode" "\
Visit the Formula directory of TAP using Dired.

(fn TAP)" t)
(autoload 'homebrew-mode "homebrew-mode" "\
Helper functions for editing Homebrew formulae

This is a minor mode.  If called interactively, toggle the
`HomeBrew mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `homebrew-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(put 'global-homebrew-mode 'globalized-minor-mode t)
(defvar global-homebrew-mode nil "\
Non-nil if Global Homebrew mode is enabled.
See the `global-homebrew-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-homebrew-mode'.")
(custom-autoload 'global-homebrew-mode "homebrew-mode" nil)
(autoload 'global-homebrew-mode "homebrew-mode" "\
Toggle Homebrew mode in all buffers.
With prefix ARG, enable Global Homebrew mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Homebrew mode is enabled in all buffers where `(lambda nil (if
(homebrew--formula-file-p (current-buffer)) (homebrew-mode)))' would
do it.

See `homebrew-mode' for more information on Homebrew mode.

(fn &optional ARG)" t)
(register-definition-prefixes "homebrew-mode" '("homebrew-"))

;;; End of scraped data

(provide 'homebrew-mode-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; homebrew-mode-autoloads.el ends here
