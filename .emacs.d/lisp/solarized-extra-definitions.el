;;; solarized-extra-definitions.el --- Extra Solarized face definitions

;;; Commentary:

;;; Definitions that have not yet made it into https://github.com/sellout/emacs-color-theme-solarized
;;; Macro to apply them, currently duplicated from the source.

;;; Code:

(defmacro solarized-with-color-variables (&rest body)
  "`let' bind all colors available for use in `solarized' around BODY."
  (declare (indent 0))
  `(let ((bold        (if solarized-bold 'bold        'unspecified))
         (bright-bold (if solarized-bold 'unspecified 'bold))
         (underline   (if solarized-underline t 'unspecified))
         (opt-under   'unspecified)
         (italic      (if solarized-italic 'italic 'unspecified)))
     (cond ((eq 'high solarized-contrast)
            (let ((orig-base3 base3))
              (rotatef base01 base00 base0 base1 base2 base3)
              (setf base3 orig-base3)))
           ((eq 'low solarized-contrast)
            (setf back      base02
                  opt-under t)))
     (let ((bg-back   '(:background back))
           (bg-base03 '(:background base03))
           (bg-base02 '(:background base02))
           (bg-base01 '(:background base01))
           (bg-base00 '(:background base00))
           (bg-base0 '(:background base0))
           (bg-base1 '(:background base1))
           (bg-base2 '(:background base2))
           (bg-base3 '(:background base3))
           (bg-green '(:background green))
           (bg-yellow '(:background yellow))
           (bg-orange '(:background orange))
           (bg-red '(:background red))
           (bg-magenta '(:background magenta))
           (bg-violet '(:background violet))
           (bg-blue '(:background blue))
           (bg-cyan '(:background cyan))

           (fg-base03 '(:foreground base03))
           (fg-base02 '(:foreground base02))
           (fg-base01 '(:foreground base01))
           (fg-base00 '(:foreground base00))
           (fg-base0 '(:foreground base0))
           (fg-base1 '(:foreground base1))
           (fg-base2 '(:foreground base2))
           (fg-base3 '(:foreground base3))
           (fg-green '(:foreground green))
           (fg-yellow '(:foreground yellow))
           (fg-orange '(:foreground orange))
           (fg-red '(:foreground red))
           (fg-magenta '(:foreground magenta))
           (fg-violet '(:foreground violet))
           (fg-blue '(:foreground blue))
           (fg-cyan '(:foreground cyan))

           (fmt-none   `())
           (fmt-bold   `(:weight ,bold))
           (fmt-bldi   `(:weight ,bold :slant ,italic))
           (fmt-undr   `(                             :underline ,underline))
           (fmt-undb   `(:weight ,bold                :underline ,underline))
           (fmt-undi   `(              :slant ,italic :underline ,underline))
           (fmt-uopt   `(                             :underline ,opt-under))
           ;; FIXME: don’t hardcode the SRGB color names
           (fmt-curl-red    `(                        :underline (:color "#dc322f" :style wave)))
           (fmt-curl-yellow `(                        :underline (:color "#b58900" :style wave)))
           (fmt-curl-magenta `(                       :underline (:color "#d33682" :style wave)))
           (fmt-curl-cyan `(                          :underline (:color "#2aa198" :style wave)))
           (fmt-ital   `(              :slant ,italic))
           ;; FIXME: not quite the same
           (fmt-stnd   `(                                                   :inverse-video t))
           (fmt-revr   `(                                                   :inverse-video t))
           (fmt-revb   `(:weight ,bold                                      :inverse-video t))
           (fmt-revbb  `(:weight ,bright-bold                               :inverse-video t))
           (fmt-revbbu `(:weight ,bright-bold         :underline ,underline :inverse-video t)))
       ,@body)))

(defun solarized-apply-definitions (definitions theme)
  "Apply face DEFINITIONS to THEME."
  (apply 'custom-theme-set-faces
	 theme
	 (mapcar (lambda (face) (apply 'create-face-spec face))
		 definitions)))

(defvar my/solarized-extra-definitions)
(setq-default my/solarized-extra-definitions
  (solarized-with-color-variables
    `(;; cperl
      (cperl-array-face (,@fg-blue))
      (cperl-hash-face (,@fg-blue))
      (cperl-nonoverridable-face (,@fg-magenta))
      ;; magit
      (magit-branch-current (,@fg-violet))
      (magit-branch-local (,@fg-blue))
      (magit-branch-remote (,@fg-green))
      (magit-diff-added (:inherit diff-added))
      (magit-diff-added-highlight (:inherit (highlight diff-added)))
      (magit-diff-context (:inherit default))
      (magit-diff-context-highlight (:inherit highlight))
      (magit-diff-file-heading (,@fmt-bold :inherit default))
      (magit-diff-file-heading-highlight (,@fmt-bold :inherit highlight))
      (magit-diff-hunk-heading (,@fg-blue))
      (magit-diff-hunk-heading-highlight (,@fg-blue :inherit highlight))
      (magit-diff-removed (:inherit diff-removed))
      (magit-diff-removed-highlight (:inherit (highlight diff-removed)))
      (magit-diffstat-added (:inherit diff-added))
      (magit-diffstat-removed (:inherit diff-removed))
      (magit-dimmed (:inherit shadow))
      (magit-filename (:inherit default))
      (magit-hash (,@fg-red))
      (magit-head (,@fg-cyan))
      (magit-header-line (:inherit magit-section-title))
      (magit-log-author (,@fg-cyan))
      (magit-log-date (,@fg-magenta))
      (magit-log-graph (,@fg-blue))
      (magit-section-heading (,@fmt-bold :inherit magit-section-title))
      (magit-section-heading-selection (,@fg-blue))
      (magit-section-highlight (:inherit highlight))
      (magit-section-secondary-heading (,@fg-base1))
      (magit-tag (,@fg-yellow))
      ;; hi-lock
      (hi-yellow (,@fg-yellow ,@bg-back))
      (hi-pink (,@fg-magenta ,@bg-back))
      (hi-green (,@fg-green ,@bg-back))
      (hi-blue (,@fg-blue ,@bg-back))
      (hi-black-b (,@fmt-bold ,@fg-base1 ,@bg-back))
      (hi-blue-b (,@fg-violet ,@bg-back))
      (hi-red-b (,@fg-red ,@bg-back))
      (hi-green-b (,@fg-cyan ,@bg-back))
      (hi-black-hb (,@fmt-bold ,@fg-base1 ,@bg-base02))
      ;; phi-search
      (phi-search-match-face (:inherit match))
      (phi-search-selection-face (:inherit isearch))
      ;; visual-regexp
      (vr/match-separator-face (:inherit match ,@fmt-revr))
      (vr/match-0 (:inherit match))
      (vr/match-1 (:inherit match))
      (vr/group-0 (,@fg-violet))
      (vr/group-1 (,@fg-cyan))
      (vr/group-2 (,@fg-green))
      ;; tty-menu
      (tty-menu-disabled-face (,@fg-base01 ,@bg-base02))
      (tty-menu-enabled-face (,@fg-base0 ,@bg-base02))
      (tty-menu-selected-face (,@fg-base02 ,@bg-base0))
      ;; paradox
      (paradox-commit-tag-face (,@fg-yellow ,@bg-back))
      (paradox-comment-face (,@fg-base1 ,@bg-back))
      (paradox-mode-line-face (:inherit mode-line-buffer-id))
      )))

(provide 'solarized-extra-definitions)

;;; solarized-extra-definitions.el ends here
