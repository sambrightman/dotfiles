(setq-default package--init-file-ensured t)
(setq load-prefer-newer t)
(require 'cask (my/join-path "~" ".cask" "cask.el"))
(cask-initialize)
(pallet-mode t)

(provide 'my-packaging)
