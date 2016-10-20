;;; .emacs --- Personal Emacs configuration

;;; Commentary:

;;; For Emacs 24.3 and up using Cask package management.
;;; Toggle to non-Cask below.
;;; Remove with-eval-after-load definition when on 24.4.

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'my-bootstrap (locate-user-emacs-file "my-bootstrap.el"))

(require 'my-security)
(require 'my-tokens)

;; Packaging

(setq load-prefer-newer t)
(require 'cask (my/join-path "~" ".cask" "cask.el"))
(cask-initialize)
(pallet-mode t)

(require 'my-defuns)
(require 'my-theme)
(if (daemonp)
    (add-hook 'after-make-frame-functions 'my/after-make-frame-functions-hook))
(my/set-background-mode my/theme-mode (selected-frame))
(my/load-my-theme)

;; theme loaded before compiling extra definitions (no after-theme-hook)
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
(global-page-break-lines-mode)
(setq-default custom-unlispify-menu-entries nil)
(setq-default custom-unlispify-tag-names nil)

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
(global-set-key (kbd "C-c d") 'dash-at-point)

(defun my/cycle-spacing ()
  "Call `cycle-spacing' in fast mode with newline chomping."
  (interactive)
  (cycle-spacing -1 t "fast"))
(substitute-key-definition 'just-one-space 'my/cycle-spacing (current-global-map))

(setq custom-file (my/join-path user-emacs-directory "custom.el"))
(shut-up
  (load custom-file))


;; references
(setq-default grep-program "zgrep")
;; remove after Emacs 25.1 everywhere
(unless (fboundp 'xref-find-definitions)
  (defun my/find-tag-no-prompt ()
    "Jump to the tag at point without prompting."
    (interactive)
    (find-tag (find-tag-default)))
  (global-set-key (kbd "M-.") 'my/find-tag-no-prompt))

(with-eval-after-load 'xref
  (add-to-list 'xref-prompt-for-identifier #'xref-find-references t 'eq))
(defun my/xref-mode-hook ()
  "Customization for `xref-mode'."
  (interactive)
  (hl-line-mode))
(add-hook 'xref--xref-buffer-mode-hook 'my/xref-mode-hook)

(defun my/filepatterns--include-compressed (orig)
  "Add compression suffixes to filenames returned in ORIG."
  (require 'dash)
  (let* ((middle (cdr (butlast orig)))
         (orig-predicates (-flatten (-split-on "-o" middle)))
         (orig-expressions (-flatten (-split-on "-name" (or orig-predicates orig))))
         (not-compressed (--remove (equal "gz" (file-name-extension it)) orig-expressions))
         (compressed (--map (concat it ".gz") not-compressed))
         (new-compressed (--remove (member it orig-expressions) compressed))
         (new-patterns (--map `("-name" ,it) `(,@orig-expressions ,@new-compressed)))
         (new-predicate (-flatten (-interpose "-o" new-patterns))))
    `("(" ,@new-predicate ")")))
(advice-add 'semantic-symref-derive-find-filepatterns
            :filter-return #'my/filepatterns--include-compressed)


;; Magit
(with-eval-after-load 'magit-remote
  (magit-define-popup-action 'magit-push-popup ?P
    'magit-push-implicitly--desc
    'magit-push-implicitly ?p t))
(setq-default magit-repository-directories (list
                                            (getenv "CODE_DIR")
                                            (getenv "DEV_DIR")))
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

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
;; (global-set-key (kbd "C-s") 'phi-search)
;; (global-set-key (kbd "C-r") 'phi-search-backward)
;; (global-set-key (kbd "M-%") 'phi-replace-query)



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
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-c q") 'vr/query-replace)
(global-set-key (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s


;; paradox
(require 'paradox)
(setq-default paradox-automatically-star t)
(setq-default paradox-display-download-count t)
(setq-default paradox-homepage-button-string "home")
(setq-default paradox-column-width-package 25)
(setq-default paradox-column-width-version 15)
(shut-up
  (paradox-enable))

;;; .emacs ends here
