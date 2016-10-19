;;; .emacs --- Personal Emacs configuration

;;; Commentary:

;;; For Emacs 24.3 and up using Cask package management.
;;; Toggle to non-Cask below.
;;; Remove with-eval-after-load definition when on 24.4.

;;; Code:

;; Emacs 24.4 has this, 24.3 does not
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))


;; Packaging
(setq load-prefer-newer t)
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(pallet-mode t)
;; prevents automatic addition of this line
;;(package-initialize)


;; load-path
(defvar my/lisp-directory (expand-file-name
                           (concat user-emacs-directory
                                   (convert-standard-filename "lisp/"))))
(let ((default-directory my/lisp-directory))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))


;; platform-specific
(cond
 ((eq system-type 'darwin)
  (if (executable-find "gls")
      (setq insert-directory-program "gls"))
  (setq-default ns-function-modifier 'hyper))
 (t nil))


;; Theme
(defconst my/theme-mode 'dark)
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
    (set-frame-parameter frame 'background-mode mode)
    (set-terminal-parameter frame 'background-mode mode)
    (when (called-interactively-p 'any)
      (frame-set-background-mode (selected-frame)))))

(defun my/load-my-theme ()
  "Load theme `solarized' and apply extra definitions."
  (interactive)
  (load-theme 'solarized t)
  (require 'solarized-extra-definitions)
  (solarized-apply-definitions my/solarized-extra-definitions 'solarized))

(defun my/after-make-frame-functions-hook (frame)
  "Customization to apply theme to new FRAME."
  (with-selected-frame frame
    (unless window-system
      (my/set-background-mode my/theme-mode frame)
      (my/load-my-theme))))

(if (daemonp)
    (add-hook 'after-make-frame-functions 'my/after-make-frame-functions-hook))
(my/set-background-mode my/theme-mode (selected-frame))
(my/load-my-theme)

;; after theme so that theme is loaded before compiling extras (no after-theme-hook)
(shut-up
  (byte-recompile-directory my/lisp-directory 0))


;; General
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq backup-inhibited t)
(setq transient-mark-mode t)
(setq require-final-newline 'query)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(column-number-mode)
(show-paren-mode)
(setq-default show-paren-style 'mixed)
(electric-pair-mode)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(global-auto-revert-mode)
(global-hi-lock-mode)
;; after-init-hook prevents a warning about advice redefinition
(add-hook 'after-init-hook #'rxt-global-mode)

;; (defun my/function--accept-ad-redefinition (func &rest args)
;;   "Call FUNC on ARGS with `ad-redefinition-action' set to `accept'."
;;   (let ((ad-redefinition-action 'accept))
;;     (apply func args)))

;; (defun my/silence-ad-redefinition (func)
;;   "Silence advice redefinition messages for FUNC."
;;   (interactive "aFunction: ")
;;   (advice-add func :around #'my/function--accept-ad-redefinition))

;; (my/silence-ad-redefinition 'turn-on-rxt-mode)

;;(global-hl-line-mode)
(setq-default auto-revert-interval 2)
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
(setq diff-switches "-u")
(setq vc-follow-symlinks t)

(defun my/function--shut-up (func &rest args)
  "Silence FUNC(ARGS) with advice."
  (require 'shut-up)
  (shut-up
    (apply func args)))

(defun my/silence-function (func)
  "Silences FUNC using `shut-up'."
  (interactive "aFunction: ")
  (advice-add func :around #'my/function--shut-up))

(my/silence-function 'vc-refresh-state)
(setq gc-cons-threshold (* 20 (* 1024 1024)))

(defun my/help-mode-revert-buffer--noconfirm (&rest args)
  "Don't confirm when reverting *Help* buffers with ARGS."
  (list (car args) :noconfirm))
(advice-add 'help-mode-revert-buffer :filter-args #'my/help-mode-revert-buffer--noconfirm)

(global-set-key (kbd "C-M-w") 'er/expand-region)

;; remove after Emacs 25.1 everywhere
(unless (fboundp 'xref-find-definitions)
  (defun my/find-tag-no-prompt ()
    "Jump to the tag at point without prompting."
    (interactive)
    (find-tag (find-tag-default)))
  (global-set-key (kbd "M-.") 'my/find-tag-no-prompt))

(defun my/cycle-spacing ()
  "Call `cycle-spacing' in fast mode with newline chomping."
  (interactive)
  (cycle-spacing -1 t "fast"))
(substitute-key-definition 'just-one-space 'my/cycle-spacing (current-global-map))

(setq custom-file (convert-standard-filename
                   (expand-file-name (concat
                                      (file-name-as-directory user-emacs-directory)
                                      "custom.el"))))
(shut-up
  (load custom-file))


;; Magit
(with-eval-after-load 'magit-remote
  (magit-define-popup-action 'magit-push-popup ?P
    'magit-push-implicitly--desc
    'magit-push-implicitly ?p t))


;; Ido
(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-ignore-extensions t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; auto-compile
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)


;; Web
(add-to-list 'auto-mode-alist '(".*\.tmpl\\'" . nxml-mode))
(with-eval-after-load 'nxml-mode
  (setq-default nxml-child-indent 4))


;; YAML
(defun my/yaml-mode-hook ()
  "Customization for `yaml-mode'."
  (interactive)
  (local-set-key (kbd "C-m") 'newline-and-indent))
(add-hook 'yaml-mode-hook 'my/yaml-mode-hook)
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/eel/.*\\.conf$") . yaml-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/ox/.*\\.conf$") . yaml-mode))


;; Perl
(defalias 'perl-mode 'cperl-mode)
(defun my/cperl-indent-exp ()
  "Re-indent surrounding expression."
  (interactive)
  (save-excursion
    (backward-up-list)
    (cperl-indent-exp)))
(defun my/cperl-mode-hook ()
  "Customization for `cperl-mode'."
  (interactive)
  (cperl-set-style "PerlStyle")
  (setq-default cperl-continued-brace-offset -4)
  (setq-default cperl-indent-parens-as-block t)
  (setq-default cperl-close-paren-offset -4)
  (setq-default cperl-autoindent-on-semi t)
  (local-set-key (kbd "M-C-q") 'my/cperl-indent-exp))
(add-hook 'cperl-mode-hook 'my/cperl-mode-hook)
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
(require 'penvwrapper)


;; Python
(with-eval-after-load 'python
  (setq-default epy-load-yasnippet-p t)
  (require 'epy-init)
;;  (epy-setup-checker (concat (expand-file-name "~/dev/pycheckers.sh") " %f"))
  (epy-django-snippets)
  (epy-setup-ipython)
  (require 'highlight-indentation)
  (linum-mode 0)
  (setq-default skeleton-pair nil))
(add-hook 'python-mode-hook 'highlight-indentation)


;; Shell
(setq-default sh-indent-comment t)
(setq-default sh-basic-offset 4)
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/shrek/") . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash.*" . sh-mode))


;; MATLAB
;; (require 'matlab-load)
;; (matlab-cedet-setup)


;; R/S-plus
(require 'ess-site)


;; Groovy
(defun my/groovy-mode-hook ()
  "Customization for `groovy-mode'."
  (interactive)
  (require 'groovy-electric)
  (groovy-electric-mode))
(add-hook 'groovy-mode-hook 'my/groovy-mode-hook)


;; Go
(with-eval-after-load 'go-mode
  (require 'go-flycheck))
(defun my/go-mode-hook ()
  "Customization for `go-mode'."
  (interactive)
  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
  (local-set-key (kbd "C-c i") 'go-goto-imports)
  (local-set-key (kbd "M-.") 'godef-jump)
  (go-eldoc-setup)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save nil t))
(add-hook 'go-mode-hook 'my/go-mode-hook)


;; Rust
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)


;; IDE-like
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Company
(add-hook 'after-init-hook #'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-go)
  (setq-default company-tooltip-limit 20)
  (setq-default company-idle-delay .2)
  (setq-default company-show-numbers t))


;; YAS
(setq yas-verbosity 2)
(add-hook 'after-init-hook #'yas-global-mode)
(with-eval-after-load 'yasnippet
  (let ((base-dir (concat (file-name-as-directory user-emacs-directory) "snippets")))
    (unless (file-directory-p base-dir)
      (make-directory base-dir))
    (add-to-list 'yas-snippet-dirs base-dir)
    (dolist (f (directory-files base-dir))
      (let ((filename (concat (file-name-as-directory base-dir) f)))
        (when (and (file-directory-p filename)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (add-to-list 'yas-snippet-dirs filename))))))


;; Ace Jump
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)


;; Jump Char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-S-m") 'jump-char-backward)


;; phi-search (multiple-cursors compatible)
(with-eval-after-load 'phi-search
  (setq-default phi-search-case-sensitive 'guess))
(global-set-key (kbd "C-s") 'phi-search)
(global-set-key (kbd "C-r") 'phi-search-backward)
(global-set-key (kbd "M-%") 'phi-replace-query)



;; Multiple cursors
;; note: C-' narrow to occurrences (not working)
;; note: C-j for a newline

;; maybe should be (does not work):
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/unmark-next-like-this)
(global-set-key (kbd "C->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-.") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/skip-to-previous-like-this)

(global-set-key (kbd "C-M-m") 'mc/mark-all-dwim)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; should be H-p to avoid history clash
(setq-default aw-scope 'frame)
(global-set-key (kbd "M-p") 'ace-window)


;; Mwim
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)


;; visual-regexp-steroids
(global-set-key (kbd "C-c q") 'vr/query-replace)
;;(global-set-key (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s


;; Utilities
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

;;; .emacs ends here
