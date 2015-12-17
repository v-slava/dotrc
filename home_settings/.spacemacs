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
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     ;; auto-completion
     ;; better-defaults
     emacs-lisp
     ;; git
     ;; markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
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
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.

   ;; dotspacemacs-default-font '("Inconsolata LGC"
   ;;                             :size 14
   ;;                             :weight normal
   ;;                             :width normal
   ;;                             :powerline-scale 1.1)

   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
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
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ))

(defun dotspacemacs/user-init ()
	"Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  ;; (defun zoom-value (zoom_factor)
  ;;   "Zoom emacs for a given zoom factor"
  ;;   (while (> zoom_factor 0)
  ;;     (setq zoom_factor (1- zoom_factor))
  ;;     (zoom-frm-in)
  ;;     )
  ;;   (while (< zoom_factor 0)
  ;;     (setq zoom_factor (1+ zoom_factor))
  ;;     (zoom-frm-out)
  ;;     )
  ;;   )

  ;; (defun frame-config ()
	;; "Frame configuration function (should be called for each new frame)"
	;; (zoom-value 5)
	;; )

  ;; (defun my-after-make-frame-hook (frame)
	;; "A hook to be added to after-make-frame-functions. It calls (frame-config)."
	;; (select-frame frame)
	;; (frame-config)
	;; )

  (defun my-display-major-mode ()
    "Display current major mode"
    (interactive)
    (message "%s" major-mode)
    )

  (defun my-set-tab-width (tab_width)
    "Set tab width"
	(interactive)
	(if (< tab_width 2)
		(error "Wrong input argument: tab_width = %d (should be >= 2)" tab_width)
	  )
	(setq-default tab-width tab_width) ;; view tab as this number of spaces
	(setq tab-width tab_width)
	(setq c-basic-offset tab_width) ;; use this number of spaces as indent
	;; show each <tab> as a string:
	;; (standard-display-ascii ?\t "\xBB   ")
	;; (standard-display-ascii ?\t "--->")
	(standard-display-ascii ?\t (concat "\xBB " (make-string (- tab_width 2) ? )))
	)

  (defun my-execute-macro (reg)
	"Execute vim macro from a given register on visualy selected region"
	(interactive "s:'<,'>normal @")
	(evil-execute-macro 1 (concat ":normal @" reg))
	)

  (defun my-ctrl-d ()
	"My Ctrl-d handler function"
	(interactive)
	(if (buffer-modified-p)
		(message "No write since last change (buffer is modified)")
	  ;; (kill-buffer)
	  (evil-execute-macro 1 ":q")
	  )
	)
)

(defun dotspacemacs/user-config ()
	"Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

	;; (frame-config)
	;; (add-hook 'after-make-frame-functions 'my-after-make-frame-hook)

	(switch-to-buffer "*scratch*") ;; less blinking on screen
	;; (setq show-trailing-whitespace nil) ;; do not highlight whitespace at end of lines
	(setq compilation-read-command nil) ;; do not prompt for compile command
	;; (setq compilation-scroll-output 'first-error)
	(setq compilation-scroll-output t)

	;; Put this in .dir-locals.el (also file .projectile may be required):
	;; ((nil . ((eval .
	;; 			   (puthash (projectile-project-root)
	;; 						  "echo \"no compile command defined\""
	;; 						  projectile-compilation-cmd-map)
	;; 			   ))))

	(spacemacs/toggle-line-numbers-on)
	(with-eval-after-load 'linum (linum-relative-toggle)) ;; make linums relative by default

	;; tab settings:
	;; permanently, force TAB to insert just one TAB:
	(define-key evil-insert-state-map (kbd "<tab>") (kbd "C-q <tab>"))
	;; (global-set-key (kbd "TAB") 'self-insert-command);
	(setq c-default-style "linux") ;; see also variable c-style-alist
	(my-set-tab-width 4)
	;; use tabs instead of spaces
	(setq-default indent-tabs-mode t)
	(setq indent-tabs-mode t)
	(add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode t)))
	(setq c-backspace-function 'backward-delete-char) ;; use backspace to delete tab in c-mode

	;; zoom hotkeys:
	(global-set-key (kbd "C-=") 'zoom-frm-in)
	(global-set-key (kbd "C--") 'zoom-frm-out)
	(global-set-key (kbd "<C-mouse-4>") 'zoom-frm-in)
	(global-set-key (kbd "<C-mouse-5>") 'zoom-frm-out)

	;; other hotkeys:
	(evil-leader/set-key "dm" 'describe-mode)
	(evil-leader/set-key "dM" 'my-display-major-mode)
	(evil-leader/set-key "de" 'eval-last-sexp)
	(evil-leader/set-key "SPC" 'evil-avy-goto-char)
	(define-key evil-normal-state-map (kbd "C-d") 'my-ctrl-d)
	(define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; quit on ESC
	;; Apply macro to selected lines (vmap 2 :normal @):
	(define-key evil-visual-state-map "2" 'my-execute-macro)

	;; TODO:
	;; diff
	;; vim configs
	;; smarttabs

	;; bR - revert buffer (like :q!, but without exit)
	;; sc - do not highlight search results
	;; cl - comment/uncomment lines
	;; tn - toggle line numbers
	;; wh - move cursor to left window
	;; wc - delete window
	;; ff - open file
	;; feh = help
	;; fed = edit .spacemacs file
	;; fs - save current file
	;; qq - kill emacs
	;; qz - kill frame
	;; zx - change font
	;; zf - change zoom
	;; hdk - help key binding
	;; hdf - help function
	;; bd - kill this buffer

	;; C-c C-l - toggle electric indentation for C/C++
	;; M-x whitespace-mode  (variable: whitespace-stile)
	;; M-x tabify / untabify
	;; S-M-: evaluate expression

	;; .spacemacs:
	;; dotspacemacs-smartparens-strict-mode nil
	;; dotspacemacs-persistent-server nil

	;; Map opening a link to <Leader> o l only in org-mode
	;; (evil-leader/set-key-for-mode 'org-mode
	;;   "ol" 'org-open-at-point)
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
