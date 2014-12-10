;; .emacs

(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory "~/.emacs.d/"
    "Directory beneath which additional per-user Emacs-specific files are placed. Various programs in Emacs store information in this directory. Note that this should end with a directory separator. See also 'locate-user-emacs-file'."))

(let ((default-directory user-emacs-directory))
      (normal-top-level-add-subdirs-to-load-path))
(setq custom-theme-load-path load-path)
(byte-recompile-directory user-emacs-directory 0)

(setq vc-follow-symlinks t)

;; from home/on Emacs 24 maybe not necessary?
;; (setq url-proxy-services '(("no_proxy" . "trading.imc.intra")
;;                            ("http" . "unixproxy:3128")))

;;(global-hl-line-mode t)
;;(set-face-background 'hl-line "orange") 

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org-mode" . "http://orgmode.org/elpa/"))
(package-initialize)

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(setq backup-inhibited t)
(setq transient-mark-mode t)
(setq require-final-newline 'query)
(column-number-mode)
(global-auto-revert-mode 1)
(setq auto-revert-interval 2)
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))
(setq diff-switches "-u")
(electric-pair-mode)

;; Web
;; what the advantage?
;;(require 'nxhtml)
(add-to-list 'auto-mode-alist '(".*\.tmpl\\'" . nxml-mode))
(setq nxml-child-indent 4)

(require 'ido)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-ignore-extensions t)

(require 'yaml-mode)
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/eel/.*\\.conf$") . yaml-mode))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/ox/.*\\.conf$") . yaml-mode))

;; Perl
;; always use cperl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;; fix weird bold colours
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:weight normal))) t)
 '(cperl-hash-face ((t (:weight normal))) t))
;; closer match with Marcin, not perfect
(defun mama-perl-indent-setup ()
  (cperl-set-style "gnu")
  (setq cperl-indent-level 0)
  (setq cperl-brace-offset 3)
  (setq cperl-continued-brace-offset 0)
  ;; (setq cperl-label-offset -2)
  (setq cperl-continued-statement-offset 0)
  ;; (setq cperl-merge-trailing-else nil)
  ;; (setq cperl-extra-newline-before-brace t)
  ;; (setq cperl-extra-newline-before-brace-multiline nil)
  ;; extra
  (setq cperl-autoindent-on-semi t)
  (setq indent-tabs-mode t))
(add-hook 'cperl-mode-hook 'mama-perl-indent-setup)

;; Python
(setq epy-load-yasnippet-p t)
(require 'epy-init)
(defun sabr-python-mode ()
  (interactive)
  (epy-setup-checker (concat (expand-file-name "~/dev/pycheckers.sh") " %f"))
  (epy-setup-ipython)
  (epy-django-snippets)
  (linum-mode 0)
  ;; using electric-pair-mode instead
  (setq skeleton-pair nil)
  (require 'highlight-indentation)
  (highlight-indentation)
  )
(add-hook 'python-mode-hook 'sabr-python-mode)

;; Shell
(setq sh-indent-comment t)
(setq sh-basic-offset 4)

;; see M-: (info "(elisp) Backquote")
(defun shrek-mode ()
  "hack because i'm bad at elisp"
  (interactive)
  (sh-mode)
  (setq indent-tabs-mode nil))
(add-to-list 'auto-mode-alist `(,(expand-file-name "~/code/shrek/") . shrek-mode))

;; MATLAB
(require 'matlab-load)
;(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(matlab-cedet-setup)

;; Groovy
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))

;; Cscope
(require 'ascope)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ;; Theme
(defun load-default-theme ()
  (interactive)
  (load-theme 'solarized-dark t))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;; 	      (lambda (frame)
;; 		(message "Hello")
;; 		(load-theme 'solarized-dark)))
;;   (load-default-theme)

;; (load-default-theme)
