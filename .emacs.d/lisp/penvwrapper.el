;;; penvwrapper.el --- a minimal penv.pl tool for Emacs

;; Copyright (C) 2016 Sam Brightman

;; Author: Sam Brightman <sam.brightman@gmail.com>
;; URL: http://github.com/sambrightman/penvwrapper.el
;; Version: 20161001
;; Keywords: perl, virtualenv, virtualenvwrapper, penv
;; Package-Requires: ((virtualenvwrapper "20160929.1113"))

;;; Commentary:

;; An anaemic Perl virtualenv tool for Emacs.  Emulates a little of the
;; functionality of Doug Hellmann's
;; [virtualenvwrapper](https://bitbucket.org/dhellmann/virtualenvwrapper/) but
;; based on [penv.pl](https://github.com/jtopjian/penv).  Abuses
;; [virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el).
;; See also https://github.com/sambrightman/penvwrapper for command-line version.


;; TODO:
;; pull out function for add/remove
;; add other environment variables
;; add other functions
;; automatically load when launched
;; tests

;;; Code:

(require 'dash)
(require 's)
(require 'virtualenvwrapper)

(defvar penv-location
  (expand-file-name (or (getenv "PWORKON_HOME") "~/.pvirtualenvs/"))
  "The location(s) of your Perl virtualenvs (penvs).
This can be either a string, which indicates a single directory
in which you keep all your virutalenvs, or a list of strings, in
which case it specifies disparate locations in which all your
virtualenvs are kept.")

(defvar penv-libraries-dir
  (file-name-as-directory (concat (file-name-as-directory "lib") "perl5"))
  "The name of the directory containing libraries.  Is it system dependent?")

;;;###autoload
(defun penv-workon (&optional name)
  "Switch to penv NAME.  Prompt for NAME if called interactively."
  (interactive)
  (let ((venv-location penv-location))
    (venv-workon name)
    (add-to-env-path-variable penv-libraries-dir "PERL5LIB")
    (add-to-env-path-variable "" "PERL_LOCAL_LIB_ROOT")))

;;;###autoload
(defun penv-deactivate ()
  "Deactivate the current penv."
  (interactive)
  (let ((venv-location penv-location))
    (remove-from-env-path-variable penv-libraries-dir "PERL5LIB")
    (remove-from-env-path-variable "" "PERL_LOCAL_LIB_ROOT")
    (venv-deactivate)))

(defun path-join (paths)
  "Join list PATHS using variable `path-separator'."
  (s-join path-separator paths))

(defun path-split (paths)
  "Split string PATHS using variable `path-separator'."
  (s-split path-separator paths))

(defun add-to-env-path-variable (subdir variable)
  "Add SUBDIR of current virtual environment to environment variable VARIABLE.
Not added if already present."
  (let ((current-value (getenv variable)))
    (unless (and current-value
                 (-contains? (path-split current-value) subdir))
      (setenv variable (concat venv-current-dir
                               subdir
                               path-separator
                               current-value)))))

(defun remove-from-env-path-variable (subdir variable)
  "Remove SUBDIR of current virtual environment from environment variable VARIABLE."
  (let ((current-value (getenv variable))
        (venv-executables-dir subdir))
    (unless (null current-value)
      (setenv variable
              (path-join (venv-get-stripped-path
                          (path-split current-value)))))))

(provide 'penvwrapper)

;;; penvwrapper.el ends here
