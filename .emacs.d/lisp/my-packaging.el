;;; my-packaging.el --- Packing-related functionality
;;; Commentary:
;;
;;; Code:

(setq-default package--init-file-ensured t) ;; removed in Emacs 27
(setq load-prefer-newer t)
(require 'cask (my/join-path "~" ".cask" "cask.el"))
;; cask and epl manually edited to manually (setq package--initialized nil) before (package-initialize)
(cask-initialize)
;; pallet mode manually edited to (or package--initialized (package-initialize))
(pallet-mode t)

(provide 'my-packaging)
;;; my-packaging.el ends here
