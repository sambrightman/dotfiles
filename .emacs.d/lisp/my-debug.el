;;; my-debug.el --- Debug-specific functions -*- lexical-binding: t; -*-
;;; Commentary:
;; Functions to assist with debugging functionality
;;; Code:


(require 'dap-mode)
(require 'dap-variables)

(defun my/plists-to-hash-by-id (plists)
  "Convert a list of PLISTS into a hash table keyed by :id."
  (let ((ht (make-hash-table :test 'equal)))
    (mapc (lambda (plist)
            (let ((id (plist-get plist :id)))
              (when id (puthash id plist ht))))
          plists)
    ht))

;; corresponds to "inputs" task configuration, to be read from config?
(defconst my/dap-variables-input-variables-config
  '((:type promptString :id "args" :description "Enter arguments: " :default "")
    (:type promptString :id "module" :description "Module to run: ")
    (:type command :id "sum" :command (lambda (l) (message "ran command") (apply #'+ l)) :args (1 2))
    (:type pickString :id "runType" :description "Type of run: " :options ("basic" "advanced") :default "basic")))

(defun my/dap-variables-input-variables ()
  "Convert the input variables configuration to a hash by :id."
  (my/plists-to-hash-by-id my/dap-variables-input-variables-config))

(defvar my/dap-variables-command-cache (make-hash-table :test #'equal)
  "Cache of results for :type command input variables, keyed by VAR.")

(defconst my/dap-variables--cache-miss 'my/dap-variables--cache-miss)

(defun my/dap-variables--clear-command-cache (_config)
  "Clear the command-result cache after a template expansion run."
  (clrhash my/dap-variables-command-cache)
  (remove-hook 'dap-variables-post-expand-hook #'my/dap-variables--clear-command-cache))

(defun my/dap-variables-input-variable (var)
  "Get user input for input variable substitution into `dap-mode' debug templates.
VAR is the input variable ID from '${input:variableID}'."
  (let* ((config (gethash var (my/dap-variables-input-variables)))
         (type (plist-get config :type))
         (description (or (plist-get config :description) ""))
         (default (plist-get config :default))
         (options (plist-get config :options))
         (password (plist-get config :password))
         (command (plist-get config :command))
         (args (plist-get config :args))
         (history-symbol (intern (concat "my/dap-variables-input-variable-history-" var)))
         (history (or (and (boundp history-symbol) (symbol-value history-symbol))
                      (set history-symbol nil)))
         (initial-input (and history (car history))))
    (cl-case type
      (promptString (if password (read-passwd description nil default)
                      (read-string description initial-input history-symbol default)))
      (pickString (completing-read description options nil t initial-input history-symbol default))
      (command
       (let ((cached (gethash var my/dap-variables-command-cache my/dap-variables--cache-miss)))
         (cond
          ((eq cached my/dap-variables--cache-miss)
           (add-hook 'dap-variables-post-expand-hook #'my/dap-variables--clear-command-cache)
           (puthash var (funcall command args) my/dap-variables-command-cache))
          (t cached)))))))

(with-eval-after-load 'dap-variables
  (add-to-list 'dap-variables-standard-variables '("\\`input:\\(.*\\)\\'" . my/dap-variables-input-variable)))

(with-eval-after-load 'dap-mode
  (dap-register-debug-template "Python :: Run file with arguments from project directory"
                               (list :type "python"
                                     :args "${input:args}"
                                     :cwd "${workspaceFolder}"
                                     :module nil
                                     :program nil
                                     :request "launch"))
  (dap-register-debug-template "Python :: Run module with arguments from project directory"
                               (list :type "python"
                                     :args "${input:args}"
                                     :cwd "${workspaceFolder}"
                                     :module "${input:module}"
                                     :program "" ;; current buffer pushed to front of arguments if nil
                                     :request "launch")))

(provide 'my-debug)
;;; my-debug.el ends here
