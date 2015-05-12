;;; .emacs --- Personal Emacs configuration

;;; Commentary:

;;; For Emacs 24.3 and up using Cask package management.
;;; Toggle to non-Cask below.
;;; Remove with-eval-after-load definition when on 24.4.

;;; Code:

;; load-path
(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))
(setq custom-theme-load-path load-path)
(byte-recompile-directory user-emacs-directory 0)


;; Emacs 24.4 has this, 24.3 does not
(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))


;; before packaging to ensure updates are loaded over .elc
(setq load-prefer-newer t)


;; Packaging

;; built-in

;; (require 'package)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;; (package-initialize)

;; cask-based

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(pallet-mode t)


;; Theme
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (let ((mode 'dark))
                    (set-frame-parameter frame 'background-mode mode)
                    (set-terminal-parameter frame 'background-mode mode))
                  (load-theme 'solarized t))))
  (load-theme 'solarized t))
;;(global-hl-line-mode t)
;;(set-face-background 'hl-line "orange")


;; General
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq backup-inhibited t)
(setq transient-mark-mode t)
(setq require-final-newline 'query)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(column-number-mode)
(show-paren-mode)
(electric-pair-mode)
(global-auto-revert-mode 1)
(setq-default auto-revert-interval 2)
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
(setq diff-switches "-u")
(setq vc-follow-symlinks t)
(setq gc-cons-threshold (* 20 (* 1024 1024)))
;; suggested key is C-=, this overwrites
(global-set-key (kbd "C-M-w") 'er/expand-region)
(defun find-tag-no-prompt ()
  "Jump to the tag at point without prompting."
  (interactive)
  (find-tag (find-tag-default)))
(global-set-key (kbd "M-.") 'find-tag-no-prompt)


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
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/eel/.*\\.conf$") . yaml-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/ox/.*\\.conf$") . yaml-mode))


;; Perl
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook (lambda ()
                             (setq mode-name "CPerl-mama")
                             (cperl-set-style "gnu")
                             (setq cperl-indent-level 0)
                             (setq cperl-brace-offset 3)
                             (setq cperl-continued-brace-offset 0)
                             ;; (setq cperl-label-offset -2)
                             (setq cperl-continued-statement-offset 0)
                             ;; (setq cperl-merge-trailing-else nil)
                             ;; (setq cperl-extra-newline-before-brace t)
                             ;; (setq cperl-extra-newline-before-brace-multiline nil)
                             (setq cperl-autoindent-on-semi t)
                             (setq indent-tabs-mode t)
                             (custom-set-faces
                              '(cperl-array-face ((t (:weight normal))) t)
                              '(cperl-hash-face ((t (:weight normal))) t))))


;; Python
(with-eval-after-load 'python
  (setq-default epy-load-yasnippet-p t)
  (require 'epy-init)
  (epy-setup-checker (concat (expand-file-name "~/dev/pycheckers.sh") " %f"))
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


;; Groovy
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))


;; Go
(with-eval-after-load 'go-mode
  (require 'go-flycheck))
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c i") 'go-goto-imports)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (go-eldoc-setup)
                          (setq gofmt-command "goimports")
                          (add-hook 'before-save-hook 'gofmt-before-save nil t)))


;; IDE-like
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Company
(add-hook 'after-init-hook #'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-go)
  (setq-default company-tooltip-limit 20)
  (setq-default company-idle-delay .2)
  (setq-default company-show-numbers t)
  (custom-set-faces
   '(company-tooltip
     ((t (:background "blue" :foreground "black"))))))

;; YAS
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


;; Magit
(global-set-key (kbd "C-c C-m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")


;; Ace Jump
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)


;; Jump Char
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-S-m") 'jump-char-backward)

;; FIXME: jump char, mc and er (above)
;; shift, angle bracket and hyper not working

;; Multiple cursors
;; suggested key is C-S-c C-S-c, this overwrites
(global-set-key (kbd "C-M-m") 'mc/mark-all-dwim)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)

;; Mwim
(global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
(global-set-key (kbd "C-e") 'mwim-end-of-code-or-line)


;;; .emacs ends here
