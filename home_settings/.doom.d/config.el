;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Viacheslav Volkov"
      user-mail-address "viacheslav.volkov.1@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Inconsolata LGC" :size 20))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type `relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

  (defun my--error (&rest args)
    "Print error message using (user-error)."
    (user-error "[Error] %s: %s" this-command (apply #'format-message args)))

  (defun my--copy-to-clipboard (data)
    "Copy data to clipboard. Based on function xclip-set-selection from xclip-1.3.el."
    (let* ((process-connection-type nil)
            (proc (cond ((getenv "DISPLAY")
                        (start-file-process-shell-command "my_clipboard" nil "clipboard.sh")))))
      (when proc
        (process-send-string proc data)
        (process-send-eof proc))
      data))

  (defun my--preserve-clipboard ()
    "Preserve clipboard contents (using clipboard.sh)."
    (when (x-get-clipboard)
      (when (not (get-text-property 0 'foreign-selection (x-get-clipboard)))
        (my--copy-to-clipboard (x-get-clipboard)))))

  (advice-add 'delete-frame :before #'my--preserve-clipboard)

  (defun my--close-window-or-frame ()
    "Close current window or frame."
    (interactive)
    (when (buffer-file-name)
      ;; a buffer has associated file name
      (when (and (buffer-modified-p) (= 1 (safe-length (get-buffer-window-list nil t t))))
        (my--error "No write since last change (buffer is modified)")))
    ;; (evil-execute-macro 1 ":q") ;; may cause accidental hangs (especially if shell is opened)
    (condition-case nil (delete-window) (error (delete-frame))))

  (defun my--eval-last-sexp-end-of-line ()
    "My implementation for \'leader \"me\"\':
evaluate the last sexp at the end of the current line."
    (interactive)
    (save-excursion
      (end-of-line)
      (eval-last-sexp nil)))

  (defun my--save-all-buffers ()
    "Silently save all buffers."
    (interactive)
    (save-some-buffers 't)
    (message "all buffers are saved"))

  ;; Do not ask for confirmation when killing emacs daemon (SPC q k):
  (setq confirm-kill-emacs nil) ;; original value: 'doom-quit-p

  (define-key evil-motion-state-map (kbd "C-d") 'my--close-window-or-frame)
  (define-key evil-normal-state-map (kbd "C-d") 'my--close-window-or-frame)
  (evil-define-key 'motion help-mode-map (kbd "C-d") 'my--close-window-or-frame)
  (evil-define-key 'motion Man-mode-map (kbd "C-d") 'my--close-window-or-frame)
  (evil-define-key 'motion Man-mode-map (kbd "q") 'my--close-window-or-frame)

  ;; (map! :leader :desc "Kill Emacs (and daemon)" "qk" #'save-buffers-kill-emacs)
  (map! :leader :desc "Kill Emacs (and daemon)" "qk" '(lambda () (interactive) (my--save-all-buffers) (kill-emacs)))

  ;; See in emacs-lisp-mode-map:
  ;; (:prefix ("e" . "eval")
  ;;   "b" #'eval-buffer
  ;;   "d" #'eval-defun
  ;;   "e" #'eval-last-sexp
  ;;   "r" #'eval-region
  ;;   "l" #'load-library)
  (map! :leader :desc "Kill Emacs (and daemon)" "me" #'my--eval-last-sexp-end-of-line)

  ;; elisp debugging: d - step into. c - step out.
  ;; (debug-on-entry 'read-file-name)
  ;; (cancel-debug-on-entry 'read-file-name)
  ;; Get backtrace when I press C-g: M-x (toggle-debug-on-quit). Now do whatever and press C-g.

  (define-key evil-insert-state-map (kbd "<tab>") (kbd "C-q <tab>"))
