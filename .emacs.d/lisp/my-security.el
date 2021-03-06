;;; my-security.el --- Security-related functionality
;;; Commentary:
;;
;;; Code:

;; include Homebrew certificates, generated from Keychain
(with-eval-after-load 'gnutls
  (let ((brew-prefix (substring (shell-command-to-string "brew --prefix") 0 -1)))
    (add-to-list 'gnutls-trustfiles (my/join-path brew-prefix "etc" "openssl" "cert.pem"))))

;; actually verify
(setq-default tls-checktrust t)
(setq-default gnutls-verify-error t)

(provide 'my-security)
;;; my-security.el ends here
