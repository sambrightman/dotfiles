;;; my-debug.el --- Debug-specific functions -*- lexical-binding: t; -*-
;;; Commentary:
;; Functions to assist with debugging functionality
;;; Code:

(require 'dap-mode)
(require 'dap-variables)
(require 'dash)
(require 'f)
(require 'project)
(require 's)
(require 'toml)


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


(defcustom my/dap-allow-setup-py-metadata nil
  "If non-nil, allow running setuptools metadata generation.
If other methods do not succeed, we can generate metadata via setup.py
in order to discover console_scripts.  This can run arbitrary code but
does not run installation."
  :type 'boolean
  :group 'dap-python)

(defun my/call-python (code &optional cwd)
  "Run Python CODE (optional in CWD), returning output.
Warn using output (but not standard error) if exit code is non-zero."
  (let ((default-directory (or cwd default-directory)))
    (with-temp-buffer
      (let ((exit-code (call-process "python" nil t nil "-c" code)))
        (if (and (integerp exit-code) (= exit-code 0))
            (buffer-string)
          (warn (buffer-string))
          nil)))))

(defun my/package-root ()
  "Try to discover the root of the package."
  (or (when (and (bound-and-true-p lsp-mode)
                 (fboundp 'lsp-workspace-root))
        (lsp-workspace-root))
      (-when-let* ((proj (project-current nil)))
        (project-root proj))
      (locate-dominating-file
       default-directory
       (lambda (dir)
         (or (f-exists? (f-expand "pyproject.toml" dir))
             (f-exists? (f-expand "setup.cfg" dir))
             (f-exists? (f-expand "setup.py" dir)))))
      default-directory))

(defun my/pyproject-path (root)
  "Return the path to pyproject.toml given the package ROOT."
  (let ((path (f-join root "pyproject.toml")))
    (when (f-exists? path) path)))

(defun my/setup-cfg-path (root)
  "Return the path to setup.cfg given the package ROOT."
  (let ((path (f-join root "setup.cfg")))
    (when (f-exists? path) path)))

(defun my/setup-py-path (root)
  "Return the path to setup.py given the package ROOT."
  (let ((path (f-join root "setup.py")))
    (when (f-exists? path) path)))

(defun my/alist-get (key alist)
  (alist-get key alist nil nil #'string=))

(defun my/pyproject-config (root)
  "Return pyproject.toml configuration from ROOT as an alist."
  (-when-let* ((path (my/pyproject-path root)))
    (toml:read-from-file path)))

(defun my/pyproject-dist-name (root)
  "Return pyproject.toml distribution name from ROOT.
Tries project.name and tool.poetry.name"
  (-when-let* ((config (my/pyproject-config root)))
    (or (-when-let* ((project (my/alist-get "project" config)))
          (my/alist-get "name" project))
        (-when-let* ((tool (my/alist-get "tool" config))
                     (poetry (my/alist-get "poetry" tool)))
          (my/alist-get "name" poetry)))))

(defun my/pyproject-scripts (root)
  "Return scripts that can be discovered from pyproject.toml in ROOT.
Unique entries from project.scripts, project.entry-points.console_scripts
and tool.poetry.scripts."
  (-when-let* ((config (my/pyproject-config root)))
    (let* ((dist-name (my/pyproject-dist-name root))
           (project (my/alist-get "project" config))
           (tool (my/alist-get "tool" config))
           (poetry (and tool (my/alist-get "poetry" tool)))
           (scripts (and project (my/alist-get "scripts" project)))
           (entry-points (and project (my/alist-get "entry-points" project)))
           (cs-table (and entry-points (my/alist-get "console_scripts" entry-points)))
           (poetry-scripts (and poetry (my/alist-get "scripts" poetry))))
      (delete-dups
       (append
        (--map (cons dist-name (car it)) scripts)
        (--map (cons dist-name (car it)) cs-table)
        (--map (cons dist-name (car it)) poetry-scripts))))))

(defun my/setup-cfg-dist-name (root)
  "Return setup.cfg distribution name from ROOT."
  (-when-let* ((setup-cfg (my/setup-cfg-path root)))
    (let* ((code
            (s-join "\n"
                    `("from pathlib import Path"
                      ,(format "path = Path(%S)" (file-relative-name setup-cfg root))
                      "try:"
                      "    from setuptools.config.setupcfg import read_configuration"
                      "except Exception:"
                      "    from setuptools.config import read_configuration"
                      "config = read_configuration(path)"
                      "print(config.setdefault('metadata', {}).get('name'))")))
           (output (my/call-python code root))
           (name (and output (s-trim output))))
      (unless (s-blank? name) name))))

(defun my/normalize-dist-name (dist-name)
  "Normalize DIST-NAME as packaging would."
  (when dist-name
    (let* ((code
            (s-join "\n"
                    `("import re"
                      ,(format "name = %S" dist-name)
                      "try:"
                      "    from packaging.utils import canonicalize_name"
                      "    name = canonicalize_name(name)"
                      "except Exception:"
                      "    try:"
                      "        from importlib.metadata import Prepared"
                      "        name = Prepared.normalize(name)"
                      "    except Exception:"
                      "        name = re.sub(r'[-_.]+', '-', name).lower()"
                      "print(name)")))
           (output (my/call-python code))
           (name (and output (s-trim output))))
      (unless (s-blank? name) name))))

(defun my/dist-name (root)
  "Return distribution name from pyproject.toml or setup.cfg in ROOT."
  (my/normalize-dist-name (or (my/pyproject-dist-name root)
                              (my/setup-cfg-dist-name root))))

(defun my/parse-entry-points (dists-and-entry-points)
  "Return a list of cons cells from parsing DISTS-AND-ENTRY-POINTS.
The text should be one line per entry point, with distribution name and
entry point being space-separated."
  (when (and dists-and-entry-points (not (s-blank? (s-trim dists-and-entry-points))))
    (let ((lines (s-lines (s-trim dists-and-entry-points))))
      (delete-dups
       (--map (-let [(dist script) (s-split-up-to " " it 1)]
                (cons dist script))
              lines)))))

(defun my/installed-dist-entry-points (root dist-name)
  "Return entry points as text from installed DIST-NAME, running in ROOT."
  (my/call-python
   (s-join "\n"
           `("try:"
             "  from importlib import metadata"
             "except Exception:"
             "  import importlib_metadata as metadata"
             ,(format "name = %S" dist-name)
             "try:"
             "    distribution = metadata.distribution(name)"
             "    for entry_point in distribution.entry_points:"
             "        if entry_point.group == 'console_scripts':"
             "            print(f'{distribution.name} {entry_point.name}')"
             "except metadata.PackageNotFoundError:"
             "    pass"))
   root))

(defun my/setuptools-metadata-entry-points (root)
  "Return entry points as text from setuptools-built metadata in ROOT."
  (my/call-python
   (s-join "\n"
           `("import contextlib, io, subprocess, sys, tempfile"
             "from pathlib import Path"
             ,(format "root = Path(%S).resolve()" root)
             "with tempfile.TemporaryDirectory() as tmp_dir:"
             "    tmp_path = Path(tmp_dir)"
             "    distribution_path = None"
             "    with contextlib.redirect_stdout(io.StringIO()):"
             "        try:"
             "            from importlib import metadata"
             "            from setuptools import build_meta"
             "            distinfo_dir = build_meta.prepare_metadata_for_build_wheel(tmp_path)"
             "            distribution_path = tmp_path / distinfo_dir"
             "        except Exception:"
             "            if (root / 'setup.py').exists():"
             "                subprocess.run([sys.executable, 'setup.py', 'egg_info'], check=False, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)"
             "                egg_infos = sorted(root.glob('*.egg-info'), key=lambda p: p.stat().st_mtime, reverse=True)"
             "                if egg_infos:"
             "                    distribution_path = egg_infos[0]"
             "    if distribution_path:"
             "        distribution = metadata.Distribution.at(distribution_path)"
             "        for entry_point in distribution.entry_points:"
             "            if entry_point.group == 'console_scripts':"
             "                print(f'{distribution.name} {entry_point.name}')"))
   root))

(defun my/installed-dist-scripts (root)
  "Find and parse entry points for installed distribution in ROOT."
  (-when-let* ((dist (my/dist-name root)))
    (my/parse-entry-points
     (my/installed-dist-entry-points root dist))))

(defun my/setuptools-metadata-scripts (root)
  "Find and parse entry points by generating metadata in ROOT."
  (my/parse-entry-points
   (my/setuptools-metadata-entry-points root)))

(defun my/setuptools-backend-p (root)
  "Return t if ROOT is using setuptools as the build backend."
  (when-let* ((config (my/pyproject-config root))
              (build-system (my/alist-get "build-system" config))
              (build-backend (and build-system (my/alist-get "build-backend" build-system))))
    (and (stringp build-backend)
         (string-prefix-p "setuptools.build_meta" build-backend))))

(defun my/project-scripts ()
  "Try various methods to get the current buffer's project scripts.

Set `my/dap-allow-setup-py-metadata' to allow running setuptools/setup.py."
  (let* ((root (file-name-as-directory (my/package-root)))
         (setup-py (my/setup-py-path root))
         (pyproject (my/pyproject-path root))
         (can-run-metadata (or setup-py
                               (and pyproject (my/setuptools-backend-p root))))
         (from-pyproject (my/pyproject-scripts root))
         (from-installed (my/installed-dist-scripts root))
         (base (delete-dups (append from-pyproject from-installed))))
    (cond
     (base base)
     ((and can-run-metadata (not my/dap-allow-setup-py-metadata))
      (when setup-py
        (message "No scripts found via installed metadata or pyproject.toml. setup.py exists; set my/dap-allow-setup-py-metadata to non-nil to run metadata generation (and any other code!)."))
      (when (and (not setup-py) pyproject (my/setuptools-backend-p root))
        (message "No scripts found via installed metadata or pyproject.toml. Set my/dap-allow-setup-py-metadata to non-nil to run setuptools metadata generation (and any other code!)."))
      nil)
     ((and can-run-metadata my/dap-allow-setup-py-metadata)
      (delete-dups (my/setuptools-metadata-scripts root)))
     (t nil))))

(defun my/dap-register-project-scripts ()
  "Register `dap-mode' templates for all discoverable entry points.

An environment must be activated and the buffer must be using either
`lsp-mode' or nother `project'-supported method."
  (interactive)
  (let* ((scripts (my/project-scripts))
         (resolved '()))
    (dolist (script scripts)
      (-when-let* ((dist-name (car script))
                   (cmd (cdr script))
                   (path (executable-find cmd)))
        (push script resolved)
        (let ((name (format "Python Script :: Run %s: %s" dist-name cmd)))
          (dap-register-debug-template
           name
           `(:type "python"
             :request "launch"
             :program ,path
             :cwd "${workspaceFolder}"
             :console "integratedTerminal"
             :justMyCode :json-false)))))
    (let* ((missing (-difference scripts resolved)))
      (when missing
        (message "Some scripts were discovered but are missing from PATH (exec-path): %s"
                 (s-join ", " (--map (format "%s: %s" (car it) (cdr it)) missing)))))))

(provide 'my-debug)
;;; my-debug.el ends here
