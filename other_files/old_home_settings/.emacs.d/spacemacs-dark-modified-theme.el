(deftheme spacemacs-dark-modified
  "Slightly modified spacemacs-dark theme.")

(custom-theme-set-faces
 'spacemacs-dark-modified
 '(cursor ((((class color) (min-colors 89)) (:background "#e3dedd"))))
 '(custom-button ((((class color) (min-colors 89)) :background "#212026" :foreground "#b2b2b2" :box (:line-width 2 :style released-button))))
 '(error ((((class color) (min-colors 89)) (:foreground "#e0211d"))))
 '(eval-sexp-fu-flash ((((class color) (min-colors 89)) (:background "#86dc2f" :foreground "#292b2e"))))
 '(eval-sexp-fu-flash-error ((((class color) (min-colors 89)) (:background "#e0211d" :foreground "#292b2e"))))
 '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "#4f97d7"))))
 '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "#2aa1ae" :background "#292e34"))))
 '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "#a45bad"))))
 '(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "#2aa1ae"))))
 '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "#bc6ec5" :bold t))))
 '(font-lock-keyword-face ((((class color) (min-colors 89)) (:bold ((class color) (min-colors 89)) :foreground "#4f97d7"))))
 '(font-lock-negation-char-face ((((class color) (min-colors 89)) (:foreground "#a45bad"))))
 '(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "#bc6ec5"))))
 '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "#2d9574"))))
 '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "#ce537a" :bold t))))
 '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "#7590db"))))
 '(font-lock-warning-face ((((class color) (min-colors 89)) (:foreground "#dc752f" :background "#292b2e"))))
 '(fringe ((((class color) (min-colors 89)) (:background "#292b2e" :foreground "#b2b2b2"))))
 '(highlight ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#100a14"))))
 '(hl-line ((((class color) (min-colors 89)) (:background "#212026"))))
 '(isearch ((((class color) (min-colors 89)) (:bold t :foreground "#292b2e" :background "#4f97d7"))))
 '(lazy-highlight ((((class color) (min-colors 89)) (:foreground "#292b2e" :background "#4f97d7" :weight normal))))
 '(link ((((class color) (min-colors 89)) (:foreground "#2aa1ae" :underline t))))
 '(link-visited ((((class color) (min-colors 89)) (:foreground "#c56ec3" :underline t))))
 '(match ((((class color) (min-colors 89)) (:background "#292b2e" :foreground "#4f97d7" :weight bold))))
 '(minibuffer-prompt ((((class color) (min-colors 89)) (:bold t :foreground "#4f97d7"))))
 '(page-break-lines ((((class color) (min-colors 89)) (:foreground "#5d4d7a"))))
 '(region ((((class color) (min-colors 89)) (:background "#333c45"))))
 '(secondary-selection ((((class color) (min-colors 89)) (:background "#100a14"))))
 '(show-paren-match ((t (:background "gray8"))))
 '(success ((((class color) (min-colors 89)) (:foreground "#86dc2f"))))
 '(tooltip ((((class color) (min-colors 89)) (:background "#6b5d85" :foreground "#b2b2b2" :bold nil :italic nil :underline nil))))
 '(vertical-border ((((class color) (min-colors 89)) (:foreground "#0a0814"))))
 '(warning ((((class color) (min-colors 89)) (:foreground "#dc752f"))))
 '(anzu-mode-line ((((class color) (min-colors 89)) (:foreground "#b1951d" :weight bold))))
 '(dired-directory ((((class color) (min-colors 89)) (:foreground "#4f97d7" :background "#292b2e" :weight bold))))
 '(dired-flagged ((((class color) (min-colors 89)) (:foreground "#f2241f"))))
 '(dired-header ((((class color) (min-colors 89)) (:foreground "#c56ec3" :weight bold))))
 '(dired-ignored ((((class color) (min-colors 89)) (:inherit shadow))))
 '(dired-mark ((((class color) (min-colors 89)) (:foreground "#c56ec3" :weight bold))))
 '(dired-marked ((((class color) (min-colors 89)) (:foreground "#a31db1" :weight bold))))
 '(dired-perm-write ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :underline t))))
 '(dired-symlink ((((class color) (min-colors 89)) (:foreground "#28def0" :background "#292b2e" :weight bold))))
 '(dired-warning ((((class color) (min-colors 89)) (:foreground "#dc752f"))))
 '(eldoc-highlight-function-argument ((((class color) (min-colors 89)) (:foreground "#86dc2f" :bold t))))
 '(helm-bookmark-directory ((((class color) (min-colors 89)) (:inherit helm-ff-directory))))
 '(helm-bookmark-file ((((class color) (min-colors 89)) (:foreground "#b2b2b2"))))
 '(helm-bookmark-gnus ((((class color) (min-colors 89)) (:foreground "#c56ec3"))))
 '(helm-bookmark-info ((((class color) (min-colors 89)) (:foreground "#c56ec3"))))
 '(helm-bookmark-man ((((class color) (min-colors 89)) (:foreground "#c56ec3"))))
 '(helm-bookmark-w3m ((((class color) (min-colors 89)) (:foreground "#c56ec3"))))
 '(helm-buffer-directory ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-buffer-file ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-buffer-not-saved ((((class color) (min-colors 89)) (:foreground "#c56ec3" :background "#292b2e"))))
 '(helm-buffer-process ((((class color) (min-colors 89)) (:foreground "#4f97d7" :background "#292b2e"))))
 '(helm-buffer-saved-out ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-buffer-size ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-candidate-number ((((class color) (min-colors 89)) (:background "#292b2e" :foreground "#4f97d7" :bold t))))
 '(helm-ff-directory ((((class color) (min-colors 89)) (:foreground "#4f97d7" :background "#292b2e" :weight bold))))
 '(helm-ff-dotted-directory ((((class color) (min-colors 89)) (:foreground "#4f97d7" :background "#292b2e" :weight bold))))
 '(helm-ff-executable ((((class color) (min-colors 89)) (:foreground "#86dc2f" :background "#292b2e" :weight normal))))
 '(helm-ff-file ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e" :weight normal))))
 '(helm-ff-invalid-symlink ((((class color) (min-colors 89)) (:foreground "#f2241f" :background "#292b2e" :weight bold))))
 '(helm-ff-prefix ((((class color) (min-colors 89)) (:foreground "#292b2e" :background "#4f97d7" :weight normal))))
 '(helm-ff-symlink ((((class color) (min-colors 89)) (:foreground "#28def0" :background "#292b2e" :weight bold))))
 '(helm-grep-cmd-line ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-grep-file ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-grep-finish ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-grep-lineno ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e"))))
 '(helm-grep-match ((((class color) (min-colors 89)) (:foreground nil :background nil :inherit helm-match))))
 '(helm-header ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e" :underline nil :box nil))))
 '(helm-header-line-left-margin ((((class color) (min-colors 89)) (:foreground "#4f97d7" :background nil))))
 '(helm-match ((((class color) (min-colors 89)) (:inherit match))))
 '(helm-match-item ((((class color) (min-colors 89)) (:inherit match))))
 '(helm-moccur-buffer ((((class color) (min-colors 89)) (:foreground "#bc6ec5" :background "#292b2e"))))
 '(helm-selection ((((class color) (min-colors 89)) (:background "#333c45"))))
 '(helm-selection-line ((((class color) (min-colors 89)) (:background "#212026"))))
 '(helm-separator ((((class color) (min-colors 89)) (:foreground "#c56ec3" :background "#292b2e"))))
 '(helm-source-header ((((class color) (min-colors 89)) (:background "#c56ec3" :foreground "#292b2e" :bold t))))
 '(helm-visible-mark ((((class color) (min-colors 89)) (:foreground "#4f97d7" :background "#100a14"))))
 '(ido-first-match ((((class color) (min-colors 89)) (:foreground "#c56ec3" :bold t))))
 '(ido-only-match ((((class color) (min-colors 89)) (:foreground "#86dc2f" :bold t))))
 '(ido-subdir ((((class color) (min-colors 89)) (:foreground "#4f97d7"))))
 '(ido-vertical-match-face ((((class color) (min-colors 89)) (:foreground "#c56ec3" :underline nil))))
 '(info-header-xref ((((class color) (min-colors 89)) (:foreground "#bc6ec5" :underline t))))
 '(info-menu ((((class color) (min-colors 89)) (:foreground "#86dc2f"))))
 '(info-node ((((class color) (min-colors 89)) (:foreground "#bc6ec5" :bold t))))
 '(info-quoted-name ((((class color) (min-colors 89)) (:foreground "#4f97d7"))))
 '(info-reference-item ((((class color) (min-colors 89)) (:background nil :underline t :bold t))))
 '(info-string ((((class color) (min-colors 89)) (:foreground "#2d9574"))))
 '(info-title-1 ((((class color) (min-colors 89)) (:height 1.4 :bold t))))
 '(info-title-2 ((((class color) (min-colors 89)) (:height 1.3 :bold t))))
 '(info-title-3 ((((class color) (min-colors 89)) (:height 1.3))))
 '(info-title-4 ((((class color) (min-colors 89)) (:height 1.2))))
 '(linum ((((class color) (min-colors 89)) (:foreground "#44505c" :background "#212026"))))
 '(linum-relative-current-face ((((class color) (min-colors 89)) (:foreground "#c56ec3"))))
 '(mode-line ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#222226" :box (:color "#5d4d7a" :line-width 1)))))
 '(mode-line-inactive ((((class color) (min-colors 89)) (:foreground "#b2b2b2" :background "#292b2e" :box (:color "#5d4d7a" :line-width 1)))))
 '(mode-line-buffer-id ((((class color) (min-colors 89)) (:bold t :foreground "#bc6ec5"))))
 '(org-kbd ((((class color) (min-colors 89)) (:inherit region :foreground "#b2b2b2" :box (:line-width 1 :style released-button)))))
 '(powerline-active1 ((((class color) (min-colors 89)) (:background "#5d4d7a" :foreground "#b2b2b2"))))
 '(powerline-active2 ((((class color) (min-colors 89)) (:background "#5d4d7a" :foreground "#b2b2b2"))))
 '(powerline-inactive1 ((((class color) (min-colors 89)) (:background "#212026" :foreground "#b2b2b2"))))
 '(powerline-inactive2 ((((class color) (min-colors 89)) (:background "#212026" :foreground "#b2b2b2"))))
 '(rainbow-delimiters-depth-1-face ((((class color) (min-colors 89)) :foreground "#4f97d7")))
 '(rainbow-delimiters-depth-2-face ((((class color) (min-colors 89)) :foreground "#bc6ec5")))
 '(rainbow-delimiters-depth-3-face ((((class color) (min-colors 89)) :foreground "#2d9574")))
 '(rainbow-delimiters-depth-4-face ((((class color) (min-colors 89)) :foreground "#67b11d")))
 '(rainbow-delimiters-depth-5-face ((((class color) (min-colors 89)) :foreground "#b1951d")))
 '(rainbow-delimiters-depth-6-face ((((class color) (min-colors 89)) :foreground "#4f97d7")))
 '(rainbow-delimiters-depth-7-face ((((class color) (min-colors 89)) :foreground "#bc6ec5")))
 '(rainbow-delimiters-depth-8-face ((((class color) (min-colors 89)) :foreground "#2d9574")))
 '(rainbow-delimiters-unmatched-face ((((class color) (min-colors 89)) :foreground "#e0211d" :overline t)))
 '(rainbow-delimiters-mismatched-face ((((class color) (min-colors 89)) :foreground "#e0211d" :overline t)))
 '(sp-pair-overlay-face ((((class color) (min-colors 89)) (:background "#333c45" :foreground nil))))
 '(sp-show-pair-match-face ((((class color) (min-colors 89)) (:foreground "#86dc2f" :weight bold :underline t))))
 '(which-key-command-description-face ((((class color) (min-colors 89)) (:foreground "#b2b2b2"))))
 '(which-key-group-description-face ((((class color) (min-colors 89)) (:foreground "#4f97d7"))))
 '(which-key-key-face ((((class color) (min-colors 89)) (:foreground "#bc6ec5" :bold t))))
 '(which-key-separator-face ((((class color) (min-colors 89)) (:background nil :foreground "#2d9574"))))
 '(which-key-special-key-face ((((class color) (min-colors 89)) (:background "#bc6ec5" :foreground "#292b2e"))))
 '(ffap ((((class color) (min-colors 89)) (:foreground "#b2b2b2"))))
 '(flx-highlight-face ((((class color) (min-colors 89)) (:foreground "#c56ec3" :underline nil))))
 '(trailing-whitespace ((((class color) (min-colors 89)) :foreground nil :background "#e0211d")))
 '(undo-tree-visualizer-current-face ((((class color) (min-colors 89)) :foreground "#4f97d7")))
 '(undo-tree-visualizer-default-face ((((class color) (min-colors 89)) :foreground "#b2b2b2")))
 '(undo-tree-visualizer-register-face ((((class color) (min-colors 89)) :foreground "#c56ec3")))
 '(undo-tree-visualizer-unmodified-face ((((class color) (min-colors 89)) :foreground "#7590db")))
 '(default ((((class color) (min-colors 89)) (:background "#292b2e" :foreground "#b2b2b2")))))

(provide-theme 'spacemacs-dark-modified)