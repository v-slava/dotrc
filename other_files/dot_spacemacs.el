;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs-base
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     vimscript
     lua
     python
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ivy
     ;; auto-completion
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     ;; better-defaults
     emacs-lisp
     git
     markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     syntax-checking
     ;; version-control
     evil-commentary
     semantic
     nlinum
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; dotspacemacs-additional-packages '(evil-visual-mark-mode)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Inconsolata LGC"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq dotspacemacs-major-mode-leader-key nil) ;; allows to use ',' in normal mode when editin elisp.
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (defun lisp-state-eval-sexp-end-of-line ()
    "My implementation for \'evil-leader \"mel\"\':
evaluate the last sexp at the end of the current line.
TODO: respect comments."
    (interactive)
    (save-excursion
      (end-of-line)
      (eval-last-sexp nil)))

  (defun my-text-scale-increase ()
    (interactive)
    (text-scale-increase 1)
    )

  (defun my-text-scale-decrease ()
    (interactive)
    (text-scale-decrease 1)
    )

  (defun my-set-tab-width (tab_width)
    "My set tab width."
    (interactive)
    (when (< tab_width 2) (error "Wrong input argument: tab_width = %d (should be >= 2)" tab_width))
    (setq-default tab-width tab_width) ;; view tab as this number of spaces
    (setq tab-width tab_width)
    (setq c-basic-offset tab_width) ;; use this number of spaces as indent
    ;; show each <tab> as a string:
    ;; (standard-display-ascii ?\t "\xBB   ")
    ;; (standard-display-ascii ?\t "--->")
    (standard-display-ascii ?\t (concat "\xBB " (make-string (- tab_width 2) ? ))))

  (defun my-close-window-or-frame ()
    "Close current window or frame."
	(interactive)
	(if (buffer-file-name)
		;; a buffer has associated file name
		(if (and (buffer-modified-p) (= 1 (safe-length (get-buffer-window-list nil t t))))
			(error "No write since last change (buffer is modified)")
		  ;; (evil-execute-macro 1 ":q") ;; may cause accidental hangs (especially if shell is opened)
          (condition-case nil (delete-window) (error (my-delete-frame)))
		  )
	  ;; a buffer hasn't associated file name
	  (condition-case nil (delete-window) (error (my-delete-frame)))
      ))

  (defun my-execute-macro (reg)
	"Execute vim macro from a given register on visualy selected region."
	(interactive "s:'<,'>normal @")
	(evil-execute-macro 1 (concat ":normal @" reg)))

  (defun my-indent-buffer ()
	(interactive)
	(save-excursion
	  (indent-region (point-min) (point-max) nil)))

  (defun my-close-temporary-windows ()
	"Close temporary windows (compile results, help, etc)."
	(interactive)
	(dolist (cur_window (window-list))
	  (when (not (buffer-file-name (window-buffer cur_window)))
		;; a buffer hasn't associated file name
		(delete-window cur_window))))

  (defun my-copy-to-clipboard (data)
	"Copy data to clipboard. Based on function xclip-set-selection from xclip-1.3.el."
	(let* ((process-connection-type nil) (proc (cond ((getenv "DISPLAY")
                                                      (start-file-process-shell-command "my_clipboard" nil "clipboard.sh")))))
	  (when proc
		(process-send-string proc data)
		(process-send-eof proc))
	  data))

  (defun my-delete-frame ()
	"Preserve clipboard contents (using clipboard.sh) and delete frame."
	(when (x-get-clipboard)
	  (when (not (get-text-property 0 'foreign-selection (x-get-clipboard)))
		(my-copy-to-clipboard (x-get-clipboard))))
	(delete-frame)
	)

  (defun my-clear-current-line ()
    "Clear current line."
    (interactive)
    (evil-execute-macro 1 "0\"zD"))

  (defun my-save-all-buffers ()
    "Silently save all buffers."
    (interactive)
    (save-some-buffers 't)
    )

  (defun my-disable-semantic-stickyfunc-mode ()
    "Disable semantic-stickyfunc-mode."
    (if (boundp 'global-semantic-stickyfunc-mode)
        (if global-semantic-stickyfunc-mode (global-semantic-stickyfunc-mode -1))))

  ;; TODO delete?
  ;; (defun my-shell-command (cmd)
  ;;   "Run shell command. Display output and exit code."
  ;;   ;; TODO
  ;;   ;; (interactive)
  ;;   (compile "make -C /home/volkov/prj/ -j1 clean")
  ;;   (compile "make -C /home/volkov/prj/ -j1 main")
  ;;   ;; (message "me")
  ;;   ;; (shell-command cmd t)
  ;;   )
  ;; (my-shell-command "ls -l && pwd")
  ;;
  ;; (defun my-buffer-to-string (buffer)
  ;;   "Return buffer contents as a string."
  ;;   (with-current-buffer buffer
  ;;     (save-restriction
  ;;   	(widen)
  ;;   	(buffer-substring-no-properties (point-min) (point-max)))))
  ;;
  ;; (defun my-create-shell-frame-if-required ()
  ;;   "Create a shell frame if it is not already created."
  ;;   (shell))
  ;;   ;; (require 'frame-fns)
  ;;   ;; (when (not (get-a-frame "*shell*"))
  ;;   ;;   (shell)
  ;;   ;;   (sleep-for 0 100) ;; wait for shell greeting (TODO fix)
  ;;   ;;   ))
  ;;
  ;; (defun my-execute-command-in-shell-frame (cmd)
  ;;   "Execute given command in a shell frame."
  ;;   (my-create-shell-frame-if-required)
  ;;   (comint-send-string "*shell*" (concat cmd "\n")))


  ;; In compilation buffer jump to proper line (buffer local variable): (goto-char compilation-next-error)
  ;; Get current line (into myLine variable):
  ;; (setq myLine
  ;;       (buffer-substring-no-properties
  ;;        (line-beginning-position)
  ;;        (line-end-position)
  ;;        ))

  (defun my-next-error ()
	"Visit next error in source code."
	(interactive)
	(spacemacs/next-error))

    ;; (with-current-buffer buffer (write-file file_name)))
    ;; (save-excursion
    ;;   (end-of-line)
    ;;   (eval-last-sexp nil)))

  ;; TODO To run two compilations at once, start the first one, then rename the *compilation* buffer
  ;; (perhaps using rename-uniquely; see Misc Buffer), then switch buffers and start the other compilation.
  ;; This will create a new *compilation* buffer. See also (rename-buffer)


  (setq my-compilation-buffer "orig")
  ;; (make-variable-frame-local 'my-compilation-buffer)
  ;; (setq my-compilation-buffer "modified")
  ;; (modify-frame-parameters (selected-frame) '((name . "my frame name!")))
  ;; (set-frame-parameter (selected-frame) 'name "my frame name 2!")
  ;; (frame-parameter (selected-frame) 'name)
  ;; (frame-parameters (selected-frame))
  ;; (set-frame-parameter (selected-frame) 'my_param "my param value can be veeeeeeeeeeeeeeeeery long !!!!!!!!!!")
  ;; (frame-parameter (selected-frame) 'my_param)

  (defun my-previous-error ()
	"Visit previous error in source code."
	(interactive)
	(spacemacs/previous-error))

  (defun my-this-error()
	"Visit this (current) error in source code."
	(interactive)
	(evil-goto-error nil))

  (defun my-first-error()
	"Visit this (current) error in source code."
	(interactive)
	(first-error nil))

  (setq compilation-error-regexp-alist '(bash gcc-include gnu))
  ;; (setq compilation-skip-threshold 2) ;; iterate only through errors (skip warnings).
  (setq compilation-skip-threshold 0) ;; iterate through everything (including notes).
  (setq compilation-auto-jump-to-first-error t)
  ;; Treat column numbers as character positions instead of screen columns in compilation errors.
  ;; Note: this adds error navigation bug: (next-error) and (prev-error) point to one line above actual error.
  ;; (setq compilation-error-screen-columns nil)
  ;; TODO auto scroll: see variable: compilation-scroll-output

  (kill-buffer "*spacemacs*") ;; less blinking on screen
  (setq-default evil-symbol-word-search t) ;; * searches for a symbol (not a word)
  (add-hook 'buffer-list-update-hook 'my-disable-semantic-stickyfunc-mode)

  (setq nlinum-format "%d") ;; setup relative line numbers. Another format example: "%4d "
  ;; nlinum has bug (at least for emacs 24) - it doesn't work with emacs daemon (can't open frame). Workaround:
  ;; (defvar frame-ready nil)
  ;; (add-hook 'after-make-frame-functions (lambda (frame) (set-frame-parameter frame 'frame-ready t)))
  ;; (add-hook 'after-init-hook (lambda () (set-frame-parameter nil 'frame-ready t)))
  ;; (eval-after-load "nlinum"
  ;;   '(defun nlinum--setup-window ()
  ;;      (let ((width (if (and frame-ready (display-graphic-p)) ;; <-- Here
  ;;                       (ceiling
  ;;                        (let ((width (nlinum--face-width 'linum)))
  ;;                          (if width
  ;;                              (/ (* nlinum--width 1.0 width)
  ;;                                 (frame-char-width))
  ;;                            (/ (* nlinum--width 1.0
  ;;                                  (nlinum--face-height 'linum))
  ;;                               (frame-char-height)))))
  ;;                     nlinum--width)))
  ;;        (set-window-margins nil (if nlinum-mode width)
  ;;                            (cdr (window-margins)))))
  ;;   )

  ;; Highlight eLisp expression (inside braces)
  (setq show-paren-style 'expression)
  (show-paren-mode)

  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))) ;; search results for "word1 word2" are equal to "word2 word1"
  (setq ivy-initial-inputs-alist nil) ;; do not automatically prepend "^" in ivy input
  ;; Disable all automatically loading variables:
  (setq enable-dir-local-variables nil)
  (setq enable-local-variables nil)
  (setq local-enable-local-variables nil)
  (setq enable-local-eval nil)

  ;; tab settings:
  ;; permanently, force TAB to insert just one TAB:
  ;; (global-set-key (kbd "TAB") 'self-insert-command);
  ;; (define-key evil-insert-state-map (kbd "<tab>") (kbd "C-q <tab>"))
  (setq c-default-style "linux") ;; see also variable c-style-alist
  (my-set-tab-width 4)
  ;; use tabs instead of spaces:
  ;; (setq-default indent-tabs-mode t)
  ;; (setq indent-tabs-mode t)
  ;; (add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode t)))
  ;; (setq c-backspace-function 'backward-delete-char) ;; use backspace to delete tab in c-mode

  (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; quit on ESC
  (define-key evil-visual-state-map "2" 'my-execute-macro)
  (define-key evil-normal-state-map (kbd "C-d") 'my-close-window-or-frame)
  (evil-define-key 'motion help-mode-map (kbd "C-d") 'my-close-window-or-frame)
  (add-hook 'compilation-mode-hook '(lambda () (local-set-key "\C-d" 'my-close-window-or-frame)))
  (evil-leader/set-key "SPC" 'avy-goto-char)
  (spacemacs/set-leader-keys "qm" 'my-close-window-or-frame)
  (spacemacs/set-leader-keys "dbs" 'bookmark-set)
  (spacemacs/set-leader-keys "dbd" 'bookmark-delete)
  (spacemacs/set-leader-keys "dq" 'my-close-temporary-windows)
  (spacemacs/set-leader-keys "di" 'my-indent-buffer)
  (spacemacs/set-leader-keys "ds" 'my-save-all-buffers)
  (spacemacs/set-leader-keys "oe" 'my-clear-current-line)
  ;; The following is standard spacemacs hotkeys (for elisp mode).
  ;; Need to define them here in order to be able to use them in non-elisp buffers.
  (spacemacs/set-leader-keys "mel" 'lisp-state-eval-sexp-end-of-line) ;; evaluate lisp expression at the end of the current line
  (spacemacs/set-leader-keys "mef" 'eval-defun) ;; evaluate lisp function (the function our cursor is in)
  (spacemacs/set-leader-keys "mee" 'eval-last-sexp) ;; evaluate lisp expression at cursor
  (evil-leader/set-key "en" 'my-next-error)
  (evil-leader/set-key "ep" 'my-previous-error)
  (evil-leader/set-key "eN" 'spacemacs/next-error)
  (evil-leader/set-key "eP" 'spacemacs/previous-error)
  (evil-leader/set-key "et" 'my-this-error)
  (evil-leader/set-key "ef" 'my-first-error)

  (global-set-key (kbd "C-=") 'my-text-scale-increase)
  (global-set-key (kbd "C--") 'my-text-scale-decrease)
  (global-set-key (kbd "<C-mouse-4>") 'my-text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'my-text-scale-decrease)
  (global-set-key (kbd "M-=") '(lambda () (interactive) (text-scale-adjust 0))) ;; go back to default scaling

  ;; Leader hotkeys:
  ;; ? search for a hotkey (counsel-descbinds)
  ;; rl - resume the last completion session (ivy-resume)
  ;; zx - change font (spacemacs/scale-font-transient-state/body)
  ;; u SPC en - go to first error (next-error 1 t)
  ;; ec - current error (my hotkey) TODO
  ;; nf - narrow the buffer to the current function (narrow-to-defun)
  ;; np	- narrow the buffer to the visible page (narrow-to-page)
  ;; nr	- narrow the buffer to the selected text (narrow-to-region)
  ;; nw	- widen, i.e show the whole buffer again (widen)
  ;; qq - kill emacs (spacemacs/prompt-kill-emacs)
  ;; qz - kill frame (spacemacs/frame-killer)
  ;; fb - jump to bookmark (command bookmark-jump)
  ;; ff - open file (counsel-find-file)
  ;; fed = edit .spacemacs file (spacemacs/find-dotfile)
  ;; fs - save current file (save-buffer)
  ;; hdk - help key binding (describe-key)
  ;; hdf - help function (counsel-describe-function)
  ;; hdv - help variable (counsel-describe-variable)
  ;; hdm - describe mode (spacemacs/describe-mode)
  ;; tn - toggle line numbers (spacemacs/toggle-line-numbers)
  ;; bR - revert buffer (like :q!, but without exit) (spacemacs/safe-revert-buffer)
  ;; bd - kill this buffer (spacemacs/kill-this-buffer)

  ;; ivy hotkeys:
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  ;; insert from clipboard: "C-y"
  ;; delete all input: "C-k" (ivy-kill-line)
  ;; cancel search: "C-g" (keyboard-escape-quit)
  ;; jump to one of the current ivy candidates <c-'> (ivy-avy)
  ;; freeze candidates list and search again among them <s-SPC> (ivy-restrict-to-matches)
  ;; next candidate "C-n" (ivy-next-line)
  ;; previous candidate: "C-p" (ivy-previous-line)
  ;; resume (repeat) last (previous) completion session: "SPC r l" (ivy-resume)

  ;; TODO research iedit mode
  ;; TODO add evil-exchange
  ;; TODO spacemacs documentation 10.
  ;; TODO kill (try to kill) buffers before closing frame.
  ;; (kill-compilation) ;; SPC ck

  ;; I won't use projectile too much. I will have my targets in elisp .dir-locals.el.
  ;; Need a function to select current (default) target from list by name (using ivy). Use hotkey: SPC o c
  ;; Need a function to display default target. Use hotkey: SPC o d
  ;; Need a function to run current (default) target (see above, but now without ivy). Use hotkey: SPC o f
  ;; To run some predefined target use something like:
  ;; (compile "make -C /some/directory/which/contains/main/Makefile your_target")
  ;; TODO: .dir-locals.el, (projectile-save-project-buffers). See how (helm-make-projectile) has been implemented.
  ;; TODO ctags / TAGS / cscope.out integration (google for "mural").

  ;; TODO projectile:
  ;; projectile-switch-project
  ;; projectile-tags-command
  ;; projectile-edit-dir-locals
  ;; projectile-project-compilation-cmd
  ;; projectile-project-test-cmd
  ;; projectile-project-run-cmd


  ;; (message "hello")
  ;; (setq-default helm-make-build-dir "/home/volkov/out")

  ;; (compile "make -C /home/volkov/prj/ -j1 main")
  ;; make -j2 SOME_TARGET:
  ;; (helm-make 2)
  ;; (helm-make-projectile 2)
  ;; make "main" target:
  ;; (helm--make-action "main")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.