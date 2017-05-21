;;; my-bootstrap.el --- Minimum functionality for other configuration
;;; Commentary:
;;
;;; Code:

;; compatibility
(unless (fboundp 'with-eval-after-load) ;; available from 24.4
  (defmacro with-eval-after-load (file &rest body)
    `(eval-after-load ,file
       `(funcall (function ,(lambda () ,@body))))))

;; platform-specific
(cond
 ((eq system-type 'darwin)
  (if (executable-find "gls")
      (setq insert-directory-program "gls"))
  (setq-default ns-function-modifier 'hyper))
 (t nil))

(defun my/add-to-load-path-recursively (path)
  "Add PATH and its subdirectories to `load-path'."
  (interactive "D")
  (let ((default-directory path))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))

(defun my/normalize-path (path)
  "Normalize PATH into a canonical, OS-normalized path."
  (interactive "D")
  (convert-standard-filename
   (expand-file-name path)))

(defun my/join-path (&rest path-parts)
  "Join PATH-PARTS into a single normalized path."
  (ignore-errors
    (my/normalize-path (apply 'concat
                              `(,@(mapcar 'file-name-as-directory (butlast path-parts))
                                ,@(last path-parts))))))

(defvar my/lisp-directory (locate-user-emacs-file "lisp/"))
(my/add-to-load-path-recursively my/lisp-directory)

(provide 'my-bootstrap)
;;; my-bootstrap.el ends here
