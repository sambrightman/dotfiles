;;; .emacs --- Personal Emacs configuration

;;; Commentary:

;;; An incrementally more clean and complete init file.
;;; Uses Cask for package management.

;;; Code:

(require 'my-bootstrap (locate-user-emacs-file "my-bootstrap.el"))
(require 'my-security)
(require 'my-tokens)
(require 'my-packaging)
(require 'my-defuns)
(require 'my-theme)

;; theme loaded before compiling extra definitions (no after-theme-hook)
(shut-up
  (byte-recompile-directory my/lisp-directory 0))

;; General
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq backup-inhibited t)
(transient-mark-mode)
(setq require-final-newline 'query)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(column-number-mode)
(show-paren-mode)
(setq-default show-paren-style 'mixed)
(delete-selection-mode)
(electric-pair-mode)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(global-hi-lock-mode)
(global-page-break-lines-mode)
(setq-default custom-unlispify-menu-entries nil)
(setq-default custom-unlispify-tag-names nil)
(my/setup-emacs-source-directory)

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
(global-auto-revert-mode)
(setq-default auto-revert-interval 5)
(setq-default auto-revert-use-notify t)
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
(setq diff-switches "-u")
(setq vc-follow-symlinks t)
(my/silence-function 'vc-refresh-state)
(setq gc-cons-threshold (* 80 (* 1024 1024)))
(setq read-process-output-max (* 1024 1024))

(defun my/help-mode-revert-buffer--noconfirm (&rest args)
  "Don't confirm when reverting *Help* buffers with ARGS."
  (list (car args) :noconfirm))
(advice-add 'help-mode-revert-buffer :filter-args #'my/help-mode-revert-buffer--noconfirm)

(global-set-key (kbd "C-M-w") 'er/expand-region)
(global-set-key (kbd "C-c d") 'my/dash-at-point)
;; remove if zeal-at-point is updated, maybe add some intelligence
(with-eval-after-load 'zeal-at-point
  (add-to-list 'zeal-at-point-mode-alist '(python-mode . "python2")))
(my/global-map-and-set-key "H-o" 'describe-symbol)
 ;; need to unbind from tab switching
(my/global-map-and-set-key "H-1" 'delete-other-windows)

(defun my/cycle-spacing ()
  "Call `cycle-spacing' in fast mode with newline chomping."
  (interactive)
  (cycle-spacing -1 t "fast"))
(substitute-key-definition 'just-one-space 'my/cycle-spacing (current-global-map))

(setq custom-file (my/join-path user-emacs-directory "custom.el"))
(shut-up
  (load custom-file))

(defun my/colorize-compilation-buffer ()
  "Enable ANSI escape sequences on compilation."
  (read-only-mode 0)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode 1))
(add-hook 'compilation-filter-hook 'my/colorize-compilation-buffer)
(setq-default compilation-scroll-output 'first-error)

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

;; Nyquist
(add-to-list 'auto-mode-alist '("\\.ny" . lisp-mode))

;; Magit
(require 'magit)
(require 'magit-popup)
(with-eval-after-load 'magit-remote
  (add-to-list 'magit-no-message "Turning on magit-auto-revert-mode")
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
(setq ido-auto-merge-delay-time 2)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)


;; auto-compile
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)


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
(my/map-key "C-M-|") ;; cperl-lineup
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))
(require 'penvwrapper)


;; Python
(jedi:install-server)
;; needs changes to both, need to auto-switch somehow
;; (setq traad-environment-name "traad2")
;; (traad-install-server "python2")
(setq-default traad-environment-name "traad3")
;; (traad-install-server "python3")
(traad-install-server)
(defun my/python-mode-hook ()
  "Customization for `python-mode'."
  (highlight-indentation-mode)
  (auto-virtualenvwrapper-activate)
  (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
  (setq-default flycheck-python-pycompile-executable "python")
  (setq-default flycheck-python-pylint-executable "python")
  (setq-default flycheck-python-flake8-executable "python")
  (setq-default flycheck-flake8rc ".flake8")
  (jedi:setup)
  (setq-default jedi:complete-on-dot t)
  (setq-default jedi:get-in-function-call-delay 400)
  (setq-default jedi:use-shortcuts t)

  ;; (traad-open default-directory) ;; https://github.com/abingham/emacs-traad/pull/11
  (local-set-key (kbd "C-c t r") 'traad-rename)
  (local-set-key (kbd "C-c t R") 'traad-rename-current-file)
  (local-set-key (kbd "C-c t i") 'traad-imports-super-smackdown)
  (local-set-key (kbd "C-c t u") 'traad-undo)
  (local-set-key (kbd "C-c t v") 'traad-extract-variable)
  (local-set-key (kbd "C-c t f") 'traad-extract-function)
  (local-set-key (kbd "C-c t n") 'traad-inline)
  (local-set-key (kbd "C-c t d") 'traad-remove-argument)
  (local-set-key (kbd "C-c t ,") 'traad-display-occurrences)
  (local-set-key (kbd "C-c t /") 'traad-display-implementations) ;; broken?
  )
(add-hook 'python-mode-hook 'my/python-mode-hook)


;; Ruby
(defun my/ruby-mode-hook ()
  "Customization for `ruby-mode'."
  (robe-mode)
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby)))
(add-hook 'ruby-mode-hook 'my/ruby-mode-hook)


;; Lua
(defun my/lua-mode-hook ()
  "Customization for `lua-mode'."
  (setq-default lua-indent-level 2))
(add-hook 'lua-mode-hook 'my/lua-mode-hook)


;; Shell
(setq-default sh-indent-comment t)
(setq-default sh-basic-offset 4)
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


;; C/C++
(defun my/c++-mode-hook ()
  "Customization for `c++-mode'."
  (c-set-offset 'innamespace 0)
  (c-set-offset 'inlambda 0)
  (c-set-offset 'stream-op '+)
  (c-set-offset 'case-label '+))
(add-hook 'c++-mode-hook #'my/c++-mode-hook)
(defun my/likely-c++-header ()
  "Check if the current buffer is likely to be a C++ header."
  (let* ((filename (buffer-file-name))
         (source-filename (concat (file-name-sans-extension filename) ".cpp")))
    (and (string-match-p "\\.h\\'" filename)
         (file-exists-p source-filename))))
(add-to-list 'magic-mode-alist '(my/likely-c++-header . c++-mode))


;; D
(add-hook 'd-mode-hook 'company-dcd-mode)


;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Company
(add-hook 'after-init-hook #'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-go)
  (add-to-list 'company-backends 'company-jedi)
  (add-to-list 'company-backends 'company-lua)
  (add-to-list 'company-backends 'company-latex-commands)
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-racer)
  (add-to-list 'company-backends 'company-robe)
  (add-to-list 'company-backends 'company-rtags)
  (company-flx-mode)
  (setq-default company-tooltip-limit 20)
  (setq-default company-idle-delay .2)
  (setq-default company-show-numbers t)
  (setq-default company-dabbrev-downcase nil)
  (setq-default company-tooltip-align-annotations t))


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
;; kicks-in during multiple cursors, no need to be default
;; (global-set-key (kbd "C-s") 'phi-search)
;; (global-set-key (kbd "C-r") 'phi-search-backward)
;; (global-set-key (kbd "M-%") 'phi-replace-query)


;; Multiple cursors
(my/map-key "C-'") ;; mc-hide-unmatched-lines-mode
(my/global-map-and-set-key "C-\." 'mc/mark-next-like-this)
(my/global-map-and-set-key "C-\," 'mc/unmark-next-like-this)
(my/global-map-and-set-key "C-<" 'mc/mark-previous-like-this)
(my/global-map-and-set-key "C->" 'mc/unmark-previous-like-this)
(my/global-map-and-set-key "C-\." 'mc/skip-to-next-like-this "C-c ")
(my/global-map-and-set-key "C-\," 'mc/skip-to-previous-like-this "C-c ")
(global-set-key (kbd "C-M-m") 'mc/mark-all-dwim)
(my/global-map-and-set-key "C-H-SPC" 'set-rectangular-region-anchor)
;; C-j for a newline


;; ace-window

;; should be H-p to avoid history clash
(setq-default aw-scope 'frame)
(my/global-map-and-set-key "H-p" 'ace-window)

;; Mwim
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)


;; visual-regexp-steroids
(require 'visual-regexp-steroids)
(global-set-key (kbd "C-c q") 'vr/query-replace)
(global-set-key (kbd "C-c m") 'vr/mc-mark)
(define-key esc-map (kbd "C-r") 'vr/isearch-backward) ;; C-M-r
(define-key esc-map (kbd "C-s") 'vr/isearch-forward) ;; C-M-s
(setq-default vr/match-separator-use-custom-face t)
(my/map-key "C-M-%") ;; query-replace-regexp


;; paradox
(require 'paradox)
(setq-default paradox-automatically-star t)
(setq-default paradox-display-download-count t)
(setq-default paradox-homepage-button-string "home")
(setq-default paradox-column-width-package 25)
(setq-default paradox-column-width-version 15)
(setq-default paradox-execute-asynchronously t)
(shut-up
  (paradox-enable))


;; flyspell
(defun my/flyspell-mode-hook ()
  "Customization for function `flyspell-mode'."
  (require 'flyspell-correct-popup)
  (my/map-and-set-key flyspell-mode-map "C-\;" 'flyspell-correct-previous-word-generic)
  (define-key flyspell-mode-map (kbd "C-\.") nil))
(add-hook 'flyspell-mode-hook 'my/flyspell-mode-hook)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; language servers
(require 'lsp-mode)
(add-hook 'vue-mode-hook 'lsp-deferred)
(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'js3-mode-hook 'lsp-deferred)
(add-hook 'rjsx-mode 'lsp-deferred)

(require 'cquery)
(defun my/cquery-enable ()
  "Enable cquery with no errors."
  (condition-case nil
      (progn
        (lsp)
        (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
        (setq cquery-extra-init-params '(:completion (:detailedLabel t))))
    (user-error nil)))
;; (add-hook 'c-mode-common-hook #'my/cquery-enable)

(setq-default rtags-autostart-diagnostics t)
(setq-default rtags-completions-enabled t)
(defun my/rtags-setup ()
  "Customization for `rtags'."
  (rtags-enable-standard-keybindings)
  (require 'flycheck-rtags)
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays?
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-hook #'my/rtags-setup)
(add-hook 'c++-mode-hook #'my/rtags-setup)
(add-hook 'objc-mode-hook #'my/rtags-setup)

(defun my/js-company-transformer (candidates)
  "Transform CANDIDATES per https://github.com/emacs-lsp/lsp-javascript."
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

(defun my/js-mode-hook ()
  "Customization for `js-mode' and related modes."
  (make-local-variable 'company-transformers)
  (push 'my/js-company-transformer company-transformers))
(add-hook 'js-mode-hook 'my/js-mode-hook)
(add-hook 'typescript-mode-hook 'my/js-mode-hook)
(add-hook 'js3-mode-hook 'my/js-mode-hook)
(add-hook 'rjsx-mode 'my/js-mode-hook)

(defun my/use-eslint-from-node-modules ()
  "Enable ESLint via node_modules."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


;;; .emacs ends here
