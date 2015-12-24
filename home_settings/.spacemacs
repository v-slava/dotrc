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
   dotspacemacs-additional-packages '(evil-visual-mark-mode)
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
   dotspacemacs-themes '(spacemacs-dark-modified
                         spacemacs-dark
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

  ;; (defun my-zoom-value (zoom_factor)
  ;;   "Zoom emacs for a given zoom factor."
  ;;   (while (> zoom_factor 0)
  ;;     (setq zoom_factor (1- zoom_factor))
  ;;     (zoom-frm-in)
  ;;     )
  ;;   (while (< zoom_factor 0)
  ;;     (setq zoom_factor (1+ zoom_factor))
  ;;     (zoom-frm-out)
  ;;     )
  ;;   )

  ;; (defun my-frame-config ()
  ;; "Frame configuration function (should be called for each new frame)."
  ;; (my-zoom-value 5)
  ;; )

  ;; (defun my-after-make-frame-hook (frame)
  ;; "A hook to be added to after-make-frame-functions. It calls (my-frame-config)."
  ;; (select-frame frame)
  ;; (my-frame-config)
  ;; )

  (defun my-indent-buffer ()
	(interactive)
	(save-excursion
	  (indent-region (point-min) (point-max) nil)))

  (defun my-write-string-to-file (str file_name)
	"Write given string to a file."
	(append-to-file str nil file_name))

  (defun my-write-buffer-to-file (buffer file_name)
	"Write given buffer to a file."
	(with-current-buffer buffer (write-file file_name)))

  (defun my-buffer-to-string (buffer)
	"Return buffer contents as a string."
	(with-current-buffer buffer
	  (save-restriction
		(widen)
		(buffer-substring-no-properties (point-min) (point-max)))))

  (defun my-display-major-mode ()
	"Display current major mode."
	(interactive)
	(message "%s" major-mode))

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

  (defun my-execute-macro (reg)
	"Execute vim macro from a given register on visualy selected region."
	(interactive "s:'<,'>normal @")
	(evil-execute-macro 1 (concat ":normal @" reg)))

  (defun my-ctrl-d ()
	"My Ctrl-d handler function."
	(interactive)
	(if (buffer-file-name)
		;; a buffer has associated file name
		(if (and (buffer-modified-p) (= 1 (safe-length (get-buffer-window-list nil t t))))
			(error "No write since last change (buffer is modified)")
		  ;; (kill-buffer)
		  (evil-execute-macro 1 ":q")
		  )
	  ;; a buffer hasn't associated file name
	  (evil-execute-macro 1 ":q!")))

  (defun my-close-temporary-windows ()
	"Close temporary windows (compile results, help, etc)."
	(interactive)
	(dolist (cur_window (window-list))
	  (when (not (buffer-file-name (window-buffer cur_window)))
		;; a buffer hasn't associated file name
		(delete-window cur_window))))

  (defun my-modify-run-cmd (orig_cmd)
	"Modify run command before execution."
	;; orig_cmd
	(concat "echo -e \"\\e[32mOutput begin:\\e[0m\" && " orig_cmd " ; echo -e \"\\e[32mRunning completed. Exit code: $?\\e[0m\"")
	;; (concat "echo -e \"\\e[36mCommand to be executed:\\e[0m\" && echo " orig_cmd " && echo -e \"\\e[32mOutput begin:\\e[0m\" && " orig_cmd " ; echo -e \"\\e[32mRunning completed. Exit code: $?\\e[0m\"")
	)

  (defun my-build-project ()
	"Build project (use projectile)."
	(projectile-save-project-buffers)
	(my-reload-dir-locals-for-current-buffer)
	(setq my_final_run_cmd (my-modify-run-cmd my_run_cmd))
	;; (projectile-compile-project (projectile-project-root)) ;; need to confirm compile command
	(projectile-compile-project nil)
	;; TODO warnings filter: build cmd modification required: gcc main.c 2>&1 | filter_warnings. Another way - filter compilation buffer
	)

  (defun my-system (cmd)
	"Execute shell command. Exit on failure."
	(with-temp-buffer
	  (when (/= 0 (call-process-shell-command cmd nil (current-buffer) nil))
		(error (buffer-string)))))

  (defun my-build-run-file ()
	"Build or run single file."
	(setq my_run_cmd (shell-command-to-string (concat
											   "~/os_settings/other_files/get_default_run_cmd.sh "
											   (file-name-nondirectory (buffer-file-name)))))
	(setq my_final_run_cmd (my-modify-run-cmd my_run_cmd))
	(spacemacs/write-file)
	(my-system "rm -rf /tmp/vim_ide_dir && mkdir /tmp/vim_ide_dir")
	(setq my_build_cmd (shell-command-to-string (concat
												 "~/os_settings/other_files/get_default_build_cmd.sh "
												 (file-name-nondirectory (buffer-file-name)))))
	(if (string= (concat "chmod +x \"" (file-name-nondirectory (buffer-file-name)) "\"") my_build_cmd)
		(progn
		  (my-system my_build_cmd)
		  (my-execute-command-in-shell-frame my_final_run_cmd))
	  (compilation-start my_build_cmd)))

  (defun my-single-file-mode ()
	"Return t if we are in single file build mode and nil otherwise (we are in project mode - \"projectile\")."
	(string= (projectile-project-name) "-"))

  (defun my-build-run ()
	"Build and run project or single file."
	(interactive)
	(when (not (buffer-file-name)) (error "Can't build: current buffer is not associated with any file"))
	(if (my-single-file-mode) (my-build-run-file) (my-build-project)))

  (defun my-create-shell-frame-if-required ()
	"Create a shell frame if it is not already created."
	(require 'frame-fns)
	(when (not (get-a-frame "*shell*"))
	  (shell)
	  (sleep-for 0 100) ;; wait for shell greeting (TODO fix)
	  ))

  (defun my-execute-command-in-shell-frame (cmd)
	"Execute given command in a shell frame."
	(my-create-shell-frame-if-required)
	(with-current-buffer "*shell*" ;; save-excursion
	  (goto-char (point-max))
	  (insert cmd)
	  (comint-send-input)))

  (defun my-filter-compilation-results ()
	"Filter compilation results"
	(with-current-buffer "*compilation*"
	  ;; (delete-matching-lines "Compilation started")
	  ;; (delete-matching-lines "6:2")
	  )
	)

  (defun my-next-error ()
	"Visit next error in source code."
	(interactive)
	(switch-to-buffer-other-window "*compilation*")
	(condition-case nil (spacemacs/next-error) (error (evil-goto-error nil))))

  (defun my-previous-error ()
	"Visit previous error in source code."
	(interactive)
	(switch-to-buffer-other-window "*compilation*")
	(condition-case nil (spacemacs/previous-error) (error (evil-goto-error nil))))

  (defun my-current-error()
	"Visit current error in source code."
	(interactive)
	(switch-to-buffer-other-window "*compilation*")
	(evil-goto-error nil))

  (defun my-first-error ()
	"Visit first error in source code."
	(my-next-error))

  (defun my-display-compilation-results ()
	"Display compilation results in new window."
	(my-filter-compilation-results)
	(switch-to-buffer-other-window "*compilation*")
	(other-window -1)
	(my-first-error))

  (defun my-compilation-finish (buffer msg)
	"Executed when compilation process finishes.
Run my_final_run_cmd if compilation succeeded.
Close the *compilation* buffer if the compilation succeeded and there is no warnings or notifications.
Otherwise show first error or warning."
	(if (string-match "^finished" msg)
		(progn
		  ;; Successfully compiled
		  (if (or (string-match ": warning: " (my-buffer-to-string buffer))
				  (string-match ": note: " (my-buffer-to-string buffer)))
			  (my-display-compilation-results)
			(delete-windows-on buffer))
		  (my-execute-command-in-shell-frame my_final_run_cmd))
	  (my-display-compilation-results)))

  (defun my-reload-dir-locals-for-current-buffer ()
	"Reload dir locals for the current buffer."
	(interactive)
	(let ((enable-local-variables :all)) (hack-dir-local-variables-non-file-buffer)))

  (defun my-eval-end-of-line ()
	"Eval lisp expression at the end of current line."
	(interactive)
	(evil-execute-macro 1 "$")
	(evil-execute-macro 1 (kbd "SPC deh")))

  (defun my-eval-mark-end-of-line ()
	"Jump to evil mark \"e\", go to end of line and evaluate lisp expression."
	(interactive)
	(evil-execute-macro 1 "'e")
	(my-eval-end-of-line))

  (defun my-display-installed-packages ()
	"Show installed (activated) packages."
	(interactive)
	(split-window-below-and-focus)
	(switch-to-buffer (make-temp-name "temp_")) ;; TODO do not use make-temp-name
	(message "See list of installed packages in current buffer")
	(print package-activated-list (current-buffer)))

  (defun my-delete-help-window-if-any ()
	"Close help window(s) if any."
	(when (get-buffer-window "*Help*" t) (delete-window (get-buffer-window "*Help*" t))))

  ;; TODO replace with macro
  (defun my-describe-function ()
	"Close help window if any and call spacemacs/describe-function."
	(interactive)
	(my-delete-help-window-if-any)
	(spacemacs/describe-function))
  (defun my-describe-variable ()
	"Close help window if any and call spacemacs/describe-variable."
	(interactive)
	(my-delete-help-window-if-any)
	(spacemacs/describe-variable))
  (defun my-describe-key ()
	"Close help window if any and call spacemacs/describe-key."
	(interactive)
	(my-delete-help-window-if-any)
	(spacemacs/describe-key))

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; (my-frame-config)
  ;; (add-hook 'after-make-frame-functions 'my-after-make-frame-hook)

  (kill-buffer "*spacemacs*") ;; less blinking on screen
  ;; (switch-to-buffer "*scratch*") ;; less blinking on screen
  (setq show-paren-style 'expression) ; Highlight eLisp expression (inside braces)
  (show-paren-mode)
  ;; (setq show-trailing-whitespace nil) ;; do not highlight whitespace at end of lines
  (evil-visual-mark-mode)
  (spacemacs/toggle-line-numbers-on)
  (with-eval-after-load 'linum (linum-relative-toggle)) ;; make linums relative by default
  (setq compilation-read-command nil) ;; do not prompt for compile command
  (add-to-list 'compilation-finish-functions 'my-compilation-finish)
  (add-to-list 'display-buffer-alist '("\\*shell\\*" display-buffer-pop-up-frame)) ;; always open shell in new frame
  (add-to-list 'display-buffer-alist '("\\*compilation\\*" display-buffer-no-window)) ;; do not display compilation buffer by default

  ;; Put this in .dir-locals.el (also file .projectile may be required):
  ;; ((nil . ((eval . (progn
  ;;     (setq my_run_cmd "/tmp/vim_ide_dir/main.out")
  ;;     (setq my_build_cmd "gcc main.c -Wall -Wextra -o /tmp/vim_ide_dir/main.out")
  ;;     (require 'projectile)
  ;; 	   (puthash (projectile-project-root)
  ;; 	       my_build_cmd
  ;; 	       projectile-compilation-cmd-map)
  ;; 				   )))))

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

  ;; overloaded spacemacs functions:
  (evil-leader/set-key "en" 'my-next-error)
  (evil-leader/set-key "ep" 'my-previous-error)
  (evil-leader/set-key "ec" 'my-current-error)
  (evil-leader/set-key "hdf" 'my-describe-function)
  (evil-leader/set-key "hdv" 'my-describe-variable)
  (evil-leader/set-key "hdk" 'my-describe-key)

  ;; other hotkeys:
  (evil-leader/set-key "dr" 'my-reload-dir-locals-for-current-buffer)
  (evil-leader/set-key "dm" 'my-display-major-mode)
  (evil-leader/set-key "dpi" 'my-display-installed-packages)
  (evil-leader/set-key "dpl" 'list-packages)
  (evil-leader/set-key "deh" 'eval-last-sexp)
  (evil-leader/set-key "dee" 'my-eval-end-of-line)
  (evil-leader/set-key "dem" 'my-eval-mark-end-of-line)
  (evil-leader/set-key "dq" 'my-close-temporary-windows)
  (evil-leader/set-key "ob" 'my-build-run)
  ;; (evil-leader/set-key "or" 'my-rebuild-run)
  ;; (evil-leader/set-key "oc" 'my-configure)
  (evil-leader/set-key "SPC" 'evil-avy-goto-char)
  (define-key evil-normal-state-map (kbd "C-d") 'my-ctrl-d)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; quit on ESC
  ;; Apply macro to selected lines (vmap 2 :normal @):
  (define-key evil-visual-state-map "2" 'my-execute-macro)

  ;; TODO:
  ;; warnings filter (compilation-filter)
  ;; compile on build server
  ;; multiple configurations (use configuration names, helm to select by name)
  ;; configure
  ;; rebuild
  ;; load dir-locals.el without prompt http://emacs.stackexchange.com/questions/14753/white-list-of-dir-locals-el
  ;; grep through the project
  ;; rtags
  ;; ctags / cscope
  ;; diff
  ;; hotkey to wrap selected region in braces / quotes / ...
  ;; smarttabs
  ;; vim configs
  ;; save project session (including frames positions, marks) on exit (see session management)
  ;; always open help in current frame (not in another one where it was opened earlier) and do not close help in another frames:
  ;; (add-to-list 'same-window-buffer-names "*Help*")
  ;; (add-to-list 'same-window-regexps ".*elp.*")
  ;; special-display-buffer-names

  ;; en - next error
  ;; ep - previous error
  ;; bR - revert buffer (like :q!, but without exit)
  ;; bd - kill this buffer
  ;; sc - do not highlight search results
  ;; cl - comment/uncomment lines
  ;; tn - toggle line numbers
  ;; wh - move cursor to left window
  ;; wc - delete window
  ;; qq - kill emacs
  ;; qz - kill frame
  ;; zx - change font
  ;; zf - change zoom
  ;; ff - open file
  ;; fed = edit .spacemacs file
  ;; fs - save current file
  ;; feh - spacemacs help (layers, etc)
  ;; hdk - help key binding
  ;; hdf - help function
  ;; hdv - help variable
  ;; hdm - describe minor mode

  ;; C-c C-l - toggle electric indentation for C/C++
  ;; M-x whitespace-mode  (variable: whitespace-stile)
  ;; M-x toggle-truncate-lines - set wrap!
  ;; M-x tabify / untabify
  ;; S-M-: evaluate expression

  ;; .spacemacs:
  ;; dotspacemacs-smartparens-strict-mode nil

  ;; For available packages see M-x list-packages
  ;; (or package-list-packages)

  ;; How to create own color theme based on existed one:
  ;; M-x customize-create-theme
  ;; do not include basic face customization
  ;; press "visit theme" (answer "yes" to all questions)
  ;; change "show paren match face" (use "gray 8")
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
