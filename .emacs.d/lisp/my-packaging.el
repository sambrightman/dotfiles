;;; my-packaging.el --- Packing-related functionality
;;; Commentary:
;;
;;; Code:

(setq-default package--init-file-ensured t) ;; removed in Emacs 27
(setq-default warning-suppress-log-types '((package reinitialization)))
(require 'cask (my/join-path "~" ".cask" "cask.el"))
;; cask and epl can be edited to (setq package--initialized nil) before (package-initialize)
(cask-initialize)
;; pallet mode can be edited to (or package--initialized (package-initialize))
(pallet-mode t)

(provide 'my-packaging)
;;; my-packaging.el ends here
