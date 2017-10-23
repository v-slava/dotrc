;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  ;; (setq debug-on-error t)
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   ;; dotspacemacs-distribution 'spacemacs-bootstrap
   ;; dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-distribution 'spacemacs
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
     emacs-lisp
     spacemacs-org ;; need for git layer below
     git
     markdown
     ;; org
     ;; (shell :variables
     ;;        shell-default-height 30
     ;;        shell-default-position 'bottom)
     ;; spell-checking
     ;; version-control
     evil-commentary

     ;; better-defaults
     ;; cscope
     (auto-completion :variables
                      ;; auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     ;; ycmd
     syntax-checking
     ;; semantic
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      ;; key-chord
                                      ninja-mode
                                      xcscope
                                      ag
                                      ivy-rtags
                                      )
   ;; dotspacemacs-additional-packages '(evil-visual-mark-mode)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(
                                    evil-escape
                                    ;; counsel-projectile
                                    ;; projectile
                                    )
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
   dotspacemacs-default-font '(
                               ("Inconsolata LGC" :size 18 :weight normal :width normal :powerline-scale 1.1)
                               ("Source Code Pro" :size 18 :weight normal :width normal :powerline-scale 1.1)
                               )
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
   ;; dotspacemacs-line-numbers 'relative
   dotspacemacs-line-numbers '(:relative t :enabled-for-modes fundamental-mode
                                         prog-mode conf-mode
                                         text-mode)
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
  (setq dotspacemacs-major-mode-leader-key nil) ;; allows to use ',' in normal mode when editing elisp.
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (defun my--error (&rest args)
    "Print error message using (user-error)."
    (user-error "[Error] %s: %s" this-command (apply #'format-message args)))

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
    (text-scale-increase 1))

  (defun my-text-scale-decrease ()
    (interactive)
    (text-scale-decrease 1))

  (defun my-text-scale-default ()
    (interactive)
    (text-scale-adjust 0))

  (defun my-set-tab-width (tab_width)
    (interactive)
    (with-demoted-errors "%s"
      (when (< tab_width 2) (my--error "Wrong input argument: tab_width = %d (should be >= 2)" tab_width))
      (setq-default tab-width tab_width) ;; view tab as this number of spaces
      (setq tab-width tab_width)
      (setq python-indent-guess-indent-offset nil)
      ;; (setq-default python-indent-offset tab_width)
      ;; (setq c-basic-offset tab_width) ;; use this number of spaces as indent
      ;; show each <tab> as a string:
      ;; (standard-display-ascii ?\t "\xBB   ")
      ;; (standard-display-ascii ?\t "--->")
      (standard-display-ascii ?\t (concat "\xBB " (make-string (- tab_width 2) ? )))))

  (defun my-close-window-or-frame ()
    "Close current window or frame."
    (interactive)
    (when (buffer-file-name)
      ;; a buffer has associated file name
      (when (and (buffer-modified-p) (= 1 (safe-length (get-buffer-window-list nil t t))))
        (my--error "No write since last change (buffer is modified)")))
    ;; (evil-execute-macro 1 ":q") ;; may cause accidental hangs (especially if shell is opened)
    (condition-case nil (delete-window) (error (my--delete-frame))))

  (defun my-execute-macro (reg)
    "Execute vim macro from a given register on visualy selected region."
    (interactive "s:'<,'>normal @")
    (evil-execute-macro 1 (concat ":normal @" reg)))

  (defun my--modify-line-internal (line prepend append fill last-column)
    "Modify current line (prepend and append given text)."
    (let* ((line-no-newline (replace-regexp-in-string "\n$" "" line))
           (append-length (length append))
           (line-no-newline-length (length line-no-newline))
           (line-ending (if (> line-no-newline-length append-length) (substring line-no-newline (- append-length) nil) nil))
           (line-no-append (if (string= line-ending append) (substring line-no-newline 0 (- line-no-newline-length append-length)) line-no-newline))
           (result (concat prepend (replace-regexp-in-string "^[ \t]*" "" (replace-regexp-in-string "[ \t]*$" "" line-no-append))))
           (length-diff (- last-column (length result) append-length))
           (need-to-add-length (if (< length-diff 0) 0 length-diff))
           (to-add (make-string need-to-add-length fill))
           (ret (concat result to-add append)))
      ret))

  (defun my-modify-lines ()
    "Modify current line."
    (interactive)
    ;; TODO operate on visual selection, vim movement block ...
    (let* ((current-line (thing-at-point 'line t))
           (updated-line (my--modify-line-internal current-line "        " " \\" ?  80)))
      (my-clear-current-line)
      (insert updated-line)
      (beginning-of-line)))

  (defun my-switch-keyboard-layout ()
    (interactive)
    (let* ((in-insert-state (string= evil-state "insert")))
      (if in-insert-state (evil-normal-state))
      (if evil-input-method
          (progn (setq evil-input-method nil)
                 (message "Switched to english keyboard layout"))
        (setq evil-input-method "russian-computer") ;; See M-x (set-input-method)
        (message "Switched to russian keyboard layout"))
      (if in-insert-state (evil-append 1))))

  (defun my-close-temporary-windows ()
    "Close temporary windows (compile results, help, etc)."
    (interactive)
    (set-frame-parameter (selected-frame) 'my--window-configuration (current-window-configuration))
    (dolist (cur_window (window-list))
      (when (not (buffer-file-name (window-buffer cur_window)))
        ;; a buffer hasn't associated file name
        (delete-window cur_window))))

  (defun my-split-and-open-buffer-below ()
    "Split current window and switch to buffer below."
    (interactive)
    (split-window-below-and-focus)
    (switch-to-buffer "*scratch*")
    (ivy-switch-buffer))

  (defun my--copy-to-clipboard (data)
    "Copy data to clipboard. Based on function xclip-set-selection from xclip-1.3.el."
    (let* ((process-connection-type nil)
           (proc (cond ((getenv "DISPLAY")
                        (start-file-process-shell-command "my_clipboard" nil "clipboard.sh")))))
      (when proc
        (process-send-string proc data)
        (process-send-eof proc))
      data))

  (defun my--delete-frame ()
    "Preserve clipboard contents (using clipboard.sh) and delete frame."
    (when (x-get-clipboard)
      (when (not (get-text-property 0 'foreign-selection (x-get-clipboard)))
        (my--copy-to-clipboard (x-get-clipboard))))
    (delete-frame))

  (defun my-clear-current-line ()
    "Clear current line."
    (interactive)
    (evil-execute-macro 1 "0\"zD"))

  (defun my-save-all-buffers ()
    "Silently save all buffers."
    (interactive)
    (save-some-buffers 't)
    (message "all buffers are saved"))

  (defun my--disable-semantic-stickyfunc-mode ()
    "Disable semantic-stickyfunc-mode."
    (when (boundp 'global-semantic-stickyfunc-mode)
      (when global-semantic-stickyfunc-mode (global-semantic-stickyfunc-mode -1))))

  (defun my--buffer-to-string (buffer)
    "Return buffer contents as a string."
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (buffer-substring-no-properties (point-min) (point-max)))))

  (defun my--load-emacs-process-files-list (directory files-list)
    "Recursively process files-list. See (my--load-emacs-projects) for details."
    (when files-list
      (let ((my--loading-file (concat directory "/" (car files-list))))
        (when (file-regular-p my--loading-file)
          ;; (message "evaluating elisp code from file: %s" my--loading-file)
          (with-demoted-errors "%s" (load-file my--loading-file))))
      (my--load-emacs-process-files-list directory (cdr files-list))))

  (defun my--load-emacs-projects (directory)
    "Read and evaluate as elisp all files from a given directory."
    (with-demoted-errors "%s"
      (unless (file-directory-p directory)
        (my--error "Can't load emacs projects: \"%s\" - no such directory" directory))
      (my--load-emacs-process-files-list directory (directory-files directory))))

  (defun my--get-compilation-buffer ()
    "Return compilation buffer."
    (frame-parameter (selected-frame) 'my--compilation-buffer))

  (defun my--set-compilation-buffer (buffer)
    "Set compilation buffer to BUFFER."
    (set-frame-parameter (selected-frame) 'my--compilation-buffer buffer))

  (defun my--get-project-name ()
    "Return currently selected project name."
    (frame-parameter (selected-frame) 'my--project-name))

  (defun my--set-project-name (name)
    "Set currently selected project name to NAME."
    (message "Set my project to: \"%s\"" name)
    (set-frame-parameter (selected-frame) 'my--project-name name))

  (defun my--get-project-definition ()
    "Return currently selected project definition."
    (assoc (my--get-project-name) my--projects-alist))

  (defun my--get-project-file ()
    "Return a file where currently selected project is defined."
    (car (cdr (my--get-project-definition))))

  (defun my--get-project-shell-commands-alist ()
    "Return shell commands alist for currently selected project."
    (car (cdr (cdr (my--get-project-definition)))))

  (defun my--get-shell-command-for-project ()
    "Return currently selected shell command (for current project)."
    (frame-parameter (selected-frame) 'my--shell-command))

  (defun my--set-shell-command-for-project (cmd)
    "Set currently selected shell command (for current project) to CMD."
    (message "Set my shell command to: \"%s\"" cmd)
    (set-frame-parameter (selected-frame) 'my--shell-command cmd))

  (defun my--get-elisp-for-shell-command (cmd)
    "Returns elisp expression for setting current shell command to CMD."
    (concat "(my--set-shell-command-for-project "
            (let ((print-escape-newlines t)) (prin1-to-string cmd))
            ")\n"))

  (defun my-configure-build-run ()
    "Configure and/or build and/or run function/file/project."
    (interactive)
    (if (derived-mode-p 'emacs-lisp-mode)
        (my--test-elisp)
      (my--execute-shell-command)))

  (defun my--get-shell-command ()
    "Return shell command (either selected for current project or by file type)."
    (if (my--get-shell-command-for-project)
        (my--get-shell-command-for-project)
      (my--get-shell-command-by-file-type)))

  (defun my-print-shell-command ()
    "Print shell command to be executed."
    (interactive)
    (message "Shell command: %s" (my--get-shell-command)))

  (defun my--open-compilation-window ()
    "Open compilation window."
    (split-window-below-and-focus)
    (switch-to-buffer (my--get-compilation-buffer)))

  (defun my--execute-shell-command ()
    "Execute shell command."
    (my-save-all-buffers)
    ;; (spacemacs/close-compilation-window) ;; this closes compilation windows in all frames.
    (my-close-temporary-windows)
    (if (my--get-compilation-buffer)
        (let* ((shell-command (my--get-shell-command)))
          (save-selected-window
            (my--open-compilation-window)
            (compile shell-command)))
      (compile (my--get-shell-command))
      (with-current-buffer "*compilation*" (rename-uniquely))
      (my--set-compilation-buffer compilation-last-buffer)))

  (defun my-restore-temp-window ()
    "Restore previously closed temporary window."
    (interactive)
    (set-window-configuration (frame-parameter (selected-frame) 'my--window-configuration)))

  (defun my--get-shell-command-by-file-type ()
    "Return a shell command to be executed for a given file type."
    (let* ((file-path (buffer-file-name (current-buffer)))
           (file-name (file-name-nondirectory file-path))
           (file-extension (file-name-extension file-name))
           (compiled-file (concat "/tmp/" file-name ".out"))
           (compile-ending (concat file-path " -o " compiled-file " && " compiled-file))
           (script (concat "chmod +x " file-path " && " file-path)))
      (cond
       ((equal file-extension "c") (concat "gcc -g3 -Wall -Wextra " compile-ending))
       ((member file-extension '("cc" "cp" "cxx" "cpp" "CPP" "c++" "C")) (concat "g++ -g3 -Wall -Wextra -std=c++11 " compile-ending))
       ((equal file-extension "rs") (concat "rustc " compile-ending))
       ((member file-extension '("sh" "bash" "py" "pl" "lua")) script)
       ((or (equal file-extension "mk") (equal file-name "Makefile")) (concat "make -f " file-path))
       (t "echo \"shell command for this file type is not defined\" && false"))))

  (defun my--insert-prj-shell-commands (shell-commands)
    "Insert project shell commands in buffer."
    (when shell-commands
      (let* ((first-cmd (car shell-commands))
             (cmd-name (car first-cmd))
             (cmd-def (cdr first-cmd)))
        (insert (concat "# " cmd-name ":\n"))
        (insert (my--get-elisp-for-shell-command cmd-def))
        ;; (insert "\n")
        (my--insert-prj-shell-commands (cdr shell-commands)))))

  (defun my-edit-shell-command ()
    "Edit shell-command to be executed by (my-configure-build-run)."
    (interactive)
    (let ((shell-cmd-by-file-type (my--get-shell-command-by-file-type)))
      (switch-to-buffer "*scratch*")
      (erase-buffer)
      (if (my--get-shell-command-for-project)
          (progn
            (insert (my--get-elisp-for-shell-command (my--get-shell-command-for-project)))
            (insert "\n")
            (my--insert-prj-shell-commands (my--get-project-shell-commands-alist))
            (insert "\n")))
      (insert (my--get-elisp-for-shell-command shell-cmd-by-file-type)))
    (evil-goto-first-line)
    (evil-find-char 1 ?\")
    (forward-char))

  (defvar my--projects-alist nil "My alist of emacs projects.")

  (defun my-register-project (name cmds)
    "Register new emacs project."
    (let* ((prj-def (assoc name my--projects-alist))
           (file (if (buffer-file-name) (buffer-file-name) my--loading-file)))
      (when prj-def (setq my--projects-alist (remove prj-def my--projects-alist)))
      ;; TODO check project definition.
      (add-to-list 'my--projects-alist `(,name . (,file ,cmds)) t)))

  (defun my-select-project ()
    "Select project using ivy."
    (interactive)
    (ivy-read "Select project: " my--projects-alist :action (lambda (x) (my--set-project-name (car x)))))

  (defun my-select-shell-command ()
    "Select shell command within current project."
    (interactive)
    (unless (my--get-project-name) (my--error "you should select project first"))
    (ivy-read "Select shell command: " (my--get-project-shell-commands-alist)
              :action (lambda (x) (my--set-shell-command-for-project (cdr x)))))

  (defun my-edit-project-definition ()
    "Edit currently selected project definition."
    (interactive)
    (let* ((file (my--get-project-file)))
      (if file
          (find-file file)
        (my--error "you should select project first"))))

  (defun my--test-elisp ()
    "Evaluate current elisp function and call (my-elisp-testcase)."
    (save-excursion
      (re-search-backward "(defun ")
      (evil-jump-item)
      (lisp-state-eval-sexp-end-of-line))
    (save-excursion (my-elisp-testcase)))

  (defun my--set-tags (name)
    "Add tags table. This function calls (visit-tags-table).
Variable tags-table-list contains list of currently active tag tables.
See also variable tags-file-name."
    (let* ((saved-large-file-warning-threshold large-file-warning-threshold)
           (saved-dotspacemacs-large-file-size dotspacemacs-large-file-size)
           (selected-cscope-file (concat my--tags-dir "/" name ".cscope.out"))
           (selected-tags-file (concat my--tags-dir "/" name ".TAGS")))
      (setq cscope-database-file selected-cscope-file)
      (tags-reset-tags-tables)
      (setq large-file-warning-threshold nil)
      (setq dotspacemacs-large-file-size 1024) ;; in megabytes
      (visit-tags-table selected-tags-file)
      (tags-completion-table)
      (setq dotspacemacs-large-file-size saved-dotspacemacs-large-file-size)
      ;; (add-to-list 'spacemacs-large-file-modes-list 'fundamental-mode) ;; tags-table-mode
      (setq large-file-warning-threshold saved-large-file-warning-threshold)
      (message "Set tags: \"%s\"" name)))

  (defun my-select-tags ()
    "Select tags to be used."
    (interactive)
    (let* ((files (directory-files (concat my--emacs-projects-dir "/tags") nil "^.+\.TAGS$"))
           (tags (mapcar (lambda (file) (string-remove-suffix ".TAGS" file)) files)))
      (ivy-read "Select tags: " tags :action 'my--set-tags)))

  ;; In compilation buffer jump to proper line (buffer local variable): (goto-char compilation-next-error)
  ;; Get current line (into myLine variable):
  ;; (setq myLine
  ;;       (buffer-substring-no-properties
  ;;        (line-beginning-position)
  ;;        (line-end-position)
  ;;        ))

  (defun my-this-error()
    "Visit this (current) error in source code."
    (interactive)
    (unless (my--get-compilation-buffer) (my--error "Can't get compilation buffer"))
    (if (with-current-buffer (my--get-compilation-buffer) compilation-current-error)
        (evil-goto-error nil)
      (my-first-error)))

  (defun my-first-error()
    "Visit this (current) error in source code."
    (interactive)
    (first-error nil))

  (defun my-copy-location-to-clipboard ()
    "Copy \"/path/to/file:line\" to clipboard."
    (interactive)
    (let ((location (concat (buffer-file-name) ":" (number-to-string (line-number-at-pos)))))
      (my--copy-to-clipboard location)
      (message (concat "copied: " location))))

  (defun my-goto-last-line ()
    "Goto last line."
    (interactive)
    (goto-char (point-max))
    (when (looking-at-p "^$")
      (previous-line))
    (beginning-of-line))

  (defun my--find-exclude-dirs (dirs-list)
    "Return command line options for \"find\" command to exclude
specified in dirs-list directories from search."
    (let (options)
      (dolist (dir dirs-list options)
        (setq options (concat options " -type d -path $(realpath " dir ") -prune -o ")))
      options))

  (defun my--find-add-prefix (prefix list)
    "Adds prefix to each list element. Returns a string which contains
concatenated (modified) elements separated by ' '."
    (let (ret)
      (dolist (element list ret)
        (setq ret (concat ret " $(realpath " prefix element ") ")))
      ret))

  (defun my--delete-first-word-if-present (str)
    "If string contains space, delete from string first word including the space.
Otherwise return unmodified string."
    (let* ((index (string-match " " str)))
      (if index (substring str (+ index 1))
        str)))

  (defun my--delete-first-symbol-pointer-or-reference-if-present (str)
    "If string starts with symbol '*' or '&', delete first symbol from string.
Otherwise return unmodified string."
    (let* ((index-reference (string-match "&" str))
           (index-pointer (string-match "*" str)))
      (if (or (and index-reference (= index-reference 0)) (and index-pointer (= index-pointer 0)))
          (substring str 1) str)))

  (defun my--delete-brace-and-after-if-present (str)
    "If string contains symbol '(', delete last part of string including '('.
Otherwise return unmodified string."
    (let* ((index (string-match "(" str)))
      (if index (substring str 0 index) str)))

  (defun my--process-rtags-symbol (str)
    (if str (my--delete-brace-and-after-if-present
             (my--delete-first-symbol-pointer-or-reference-if-present
              (my--delete-first-word-if-present str)))
      ""))

  (defun my--get-symbol-under-cursor ()
    "Get symbol under cursor"
    (my--process-rtags-symbol (rtags-current-symbol-name)))

  (defun my-find-references-cscope ()
    "Select tags before running (cscope-find-this-symbol) if necessary."
    (interactive)
    (when (not tags-file-name) (my-select-tags))
    (cscope-find-this-symbol (find-tag--default))
    )

  (defun my-find-tag ()
    "My version of (find-tag) which uses (ivy-read) instead of standard (completing-read) and thus supports (ivy-resume)."
    (interactive)
    (when (not tags-file-name) (my-select-tags))
    (let* ((completion-ignore-case (if (memq tags-case-fold-search '(t nil)) tags-case-fold-search case-fold-search))
           (default-value (my--get-symbol-under-cursor)))
      ;; :initial-input default-value
      (ivy-read "Select tag: " (tags-lazy-completion-table) :preselect default-value :action 'find-tag)))

  (defun my--choose-directory ()
    "Choose directory interactively (use vifm). Returns choosed directory."
    (shell-command-to-string (concat my--os-settings "/other_files/choose_directory.sh")))

  (defun my-search-in-directory-recursive ()
    "Use ag to search in interactively choosen directory (ivy interface)."
    (interactive)
    ;; (spacemacs/counsel-search '("ag") t (my--choose-directory)) ;; this fails if we can't find a symbol under cursor
    (spacemacs/counsel-search '("ag") (not (not (find-tag--default))) (my--choose-directory)))

  (defun my-search-in-directory-ag ()
    "Use ag to search in interactively choosen directory (ag.el interface)."
    (interactive)
    ;; TODO wgrep:
    ;; use --vimgrep but delete column
    ;; header format must be like in (spacemacs/counsel-search), number of candidates is obligatory
    ;; delete ag statistics in the end.
    ;; put buffer in proper folder (relative file path must be correct)
    ;; (grep-mode)
    ;; (ivy-wgrep-change-to-wgrep-mode) ;; (require 'wgrep) (wgrep-change-to-wgrep-mode)
    ;; edit ...
    ;; (wgrep-finish-edit)
    ;; (my-save-all-buffers)
    (let ((dir (my--choose-directory)))
      (ag-regexp (read-from-minibuffer "Search string: " (find-tag--default)) dir)))

  (defun my-open-new-man-page ()
    "Open man page in new frame."
    (interactive)
    (if (string= major-mode "Man-mode")
        (call-interactively 'man)
      (let* ((saved-notify-method Man-notify-method))
        (setq Man-notify-method 'newframe)
        (call-interactively 'man)
        (setq Man-notify-method saved-notify-method))))

  (defun my-next-buffer ()
    "In selected window switch to next buffer.
Add Man mode support to (next-buffer)."
    (interactive)
    (if (string= major-mode "Man-mode")
        (let* ((next-buffers-before (window-next-buffers))
               (next-buffers-after (cdr next-buffers-before))
               (cur-window (get-buffer-window)))
          (if next-buffers-before
              (progn
                (switch-to-buffer (car (car next-buffers-before)))
                (set-window-next-buffers cur-window next-buffers-after))
            (my--error "there were no next buffers found.")))
      (next-buffer)))

  (defun my-previous-buffer ()
    "In selected window switch to previous buffer.
Add Man mode support to (previous-buffer)."
    (interactive)
    (if (string= major-mode "Man-mode")
        (let* ((prev-buffers-before (window-prev-buffers))
               (next-buffers-before (window-next-buffers))
               (unused (switch-to-buffer (car (car prev-buffers-before))))
               (next-buffers-after (cons (car (window-prev-buffers)) next-buffers-before))
               (prev-buffers-after (cdr prev-buffers-before))
               (cur-window (get-buffer-window)))
          (set-window-next-buffers cur-window next-buffers-after)
          (set-window-prev-buffers cur-window prev-buffers-after))
      (previous-buffer)))

  (defun my-goto-elisp-definition ()
    "In help buffer (function/variable description) go to definition."
    (interactive)
    ;; Move cursor to a file where function/variable is defined:
    (evil-goto-first-line)
    (beginning-of-line)
    (re-search-forward "[’'].")
    (backward-char)
    (backward-char)
    (backward-char)
    ;; Simulate <enter> keypress:
    (setq unread-command-events (listify-key-sequence (kbd "RET")))
    )

  (defun my-insert-snippet ()
    "Insert snippet."
    (interactive)
    (yas-insert-snippet)
    (evil-insert-state)
    )

  (defun my-kill-whole-line ()
    "Like (kill-whole-line), but doesn't fail if there no newline at the end of line."
    (interactive)
    (if (= (point) (point-max))
        (if (not (string= "1" (format-mode-line "%l")))
            (previous-line)))
    (if (= (point) (point-max))
        (if (/= (point) (point-min)) (backward-delete-char 1))
      (kill-whole-line))
    )

  (defun my-make-sure-have-newline-at-end-of-file ()
    "Insert newline at end of file (if absent)."
    (interactive)
    (save-excursion
      (goto-char (point-max))
      (if (/= 0 (buffer-size))
          (let* ((orig (thing-at-point 'line t))
                 (new (if (string-match-p "\n" orig) orig (concat orig "\n"))))
            (my-kill-whole-line)
            (insert new)
            ))))

  (defun my-update-include-guards ()
    "Add/update include guards."
    (interactive)
    (save-excursion
      (my-make-sure-have-newline-at-end-of-file)
      ;; Delete old include guards (if present):
      (if (/= 0 (buffer-size))
          (let* ((first-line (progn (goto-char (point-min)) (thing-at-point 'line t)))
                 (second-line (progn (goto-char (point-min)) (next-line) (thing-at-point 'line t)))
                 (first-line-matches (string-match-p "^#ifndef[ ]\+[^ ]" first-line))
                 (second-line-matches (string-match-p "^#define[ ]\+[^ ]" second-line))
                 (include-guard-present (and first-line-matches second-line-matches))
                 )
            (if include-guard-present (progn
                                        (goto-char (point-min))
                                        (my-kill-whole-line) ;; delete first line ("#ifndef ")
                                        (my-kill-whole-line) ;; delete second line ("#define ")
                                        (while (string= "\n" (thing-at-point 'line t))
                                          (my-kill-whole-line) ;; delete empty lines at the begin of buffer
                                          )
                                        (goto-char (point-max))
                                        (while (string= "\n" (thing-at-point 'line t))
                                          (my-kill-whole-line) ;; delete empty lines at the end of buffer
                                          )
                                        (if (string-match-p "^#endif" (thing-at-point 'line t))
                                            (progn (my-kill-whole-line) ;; delete last line ("#endif")
                                                   (while (string= "\n" (thing-at-point 'line t))
                                                     (my-kill-whole-line) ;; delete empty lines at the end of buffer
                                                     )
                                                   )
                                          )
                                        ))
            )
        )
      ;; Insert new include guards:
      (let* ((define (concat (upcase (replace-regexp-in-string "\\." "_" (file-name-nondirectory (buffer-file-name)))) "")) ;; "_DEFINED"))
             (ifndef (concat "#ifndef " define)))
        (goto-char (point-min))
        (insert (concat ifndef "\n#define " define "\n\n"))
        (goto-char (point-max))
        (insert (concat "\n#endif // " ifndef "\n"))
        )
      )
    )

  (defun my--string-to-expression (string)
    "Convert string to elisp expression that can be evaluated"
    (car (read-from-string string)))

  (defun my-evaluate-last-expression-from-history ()
    "Evaluate last expression from history using (eval-expression)."
    (interactive)
    ;; alt + shift + ';' (semicolon)
    ;; (eval-expression (rtags-current-symbol)))
    (let* ((last-expression (car read-expression-history)))
      (if last-expression
          (progn
            ;; (message (concat "Evaluating: |" last-expression "|"))
            (eval-expression (my--string-to-expression last-expression))
            )
        (my--error "Nothing to evaluate: history is empty."))))

  (defun my--choose-from-menu (menu-title menu-items)
    "Choose from a list of choices from a popup menu."
    (let ((item)
          (item-list))
      (while menu-items
        (setq item (car menu-items))
        (if (consp item)
            (setq item-list (cons (cons (car item) (cdr item) ) item-list))
          (setq item-list (cons (cons item item) item-list)))
        (setq menu-items (cdr menu-items))
        )
      (x-popup-menu t (list menu-title (cons menu-title (nreverse item-list))))))

  (defun my-rtags-find-symbol (arg)
    "Find symbol (case insensitively). With universal argument (SPC u): case sensitively."
    (interactive "P")
    (unless arg (setq rtags-symbolnames-case-insensitive t))
    (rtags-find-symbol)
    (unless arg (setq rtags-symbolnames-case-insensitive nil))
    )

  (defun my-rtags-find-symbol-at-point ()
    "Fall back to (rtags-find-symbol) in case of failure."
    (interactive)
    (when (not (rtags-find-symbol-at-point))
      ;; (rtags-find-symbol) ;; need to press return immediately here, so see below:
      (execute-kbd-macro (vconcat [?\M-x] (string-to-vector "rtags-find-symbol") [return return]))
      ))

  (defadvice rtags-current-symbol-name (after my--rtags-current-symbol-name activate)
    "Delete class from string (if present)."
    (setq ad-return-value (if ad-return-value (my--process-rtags-symbol ad-return-value) nil)))

  (defun my-elisp-testcase ()
    "Call function to be tested (execute a testcase)."
    ;; (call-interactively 'other-frame)
    ;; (message (my--delete-class-from-string "class NameSpace::ClName"))
    ;; (call-interactively 'other-frame)
    )

  (defun my-elisp-testcase-undo ()
    "Undo effect, caused by prefious execution of (my-elisp-testcase)."
    (interactive)
    (call-interactively 'other-frame)
    (undo-tree-undo)
    (call-interactively 'other-frame)
    )

  (setq mouse-wheel-scroll-amount '(3 ((shift) . 9) ((control))))
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  ;; (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

  (setq compilation-error-regexp-alist '(bash gcc-include gnu))
  (setq compilation-skip-threshold 2) ;; iterate only through errors (skip warnings).
  ;; (setq compilation-skip-threshold 1) ;; iterate through errors end warnings.
  ;; (setq compilation-skip-threshold 0) ;; iterate through everything (including notes).
  (setq compilation-auto-jump-to-first-error t) ;; automatically jump to first compilation error
  ;; Treat column numbers as character positions instead of screen columns in compilation errors.
  ;; Note: this adds error navigation bug: (next-error) and (prev-error) point to one line above actual error.
  ;; (setq compilation-error-screen-columns nil)
  (setq compilation-scroll-output 'first-error)

  (add-to-list 'auto-mode-alist '("\\vifmrc\\'" . vimrc-mode))
  (kill-buffer "*spacemacs*") ;; less blinking on screen
  (setq-default evil-symbol-word-search t) ;; * searches for a symbol (not a word)
  (add-hook 'buffer-list-update-hook 'my--disable-semantic-stickyfunc-mode)
  (setq dotspacemacs-distinguish-gui-tab t) ;; fix <C-i> (evil-jump-forward) in normal mode.
  ;; Highlight eLisp expression (inside braces)
  (setq show-paren-style 'expression)
  (show-paren-mode)

  ;; magit:
  ;; SPC g b (spacemacs/git-blame-micro-state).
  ;; SPC g s (magit-status).
  ;; In status buffer:
  ;; "g r" - (magit-refresh).
  ;; "SPC g m" or "?" or "h" - show help, submit changes (magit-dispatch-popup).
  ;; "q" - quit help (magit-mode-bury-buffer).
  ;; "TAB" - expand / collapse (magit-section-toggle). See also "C-<TAB>", "S-<TAB>", "M-<TAB>".
  ;; "4" - expand to diff level, "2" - expand to file names level.
  ;; "C-j" (magit-section-forward).
  ;; "C-k" (magit-section-backward).
  ;; "]" or "g j" (magit-section-forward-sibling).
  ;; "[" or "g k" (magit-section-backward-sibling).
  ;; "^" (magit-section-up).
  ;; "s" - stage, "u" - unstage. "S" - stage all. "U" - unstage all.
  ;; "C-SPC" - mark for staging (set-mark-command).
  ;; After putting mark move somewhere and press "s" to stage only part of the hunk.
  ;; "! g", "! k" - run external git gui frontends (git-gui, gitk).
  ;; Customize settings:
  ;; 1) Change desired settings (type "-" and then your setting switch).
  ;; 2) Write changes in .spacemacs: "C-x C-s".
  ;; Add "--color" to "git log":
  (custom-set-variables '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))))

  (with-eval-after-load 'magit
    (define-key git-rebase-mode-map (kbd "C-k") 'git-rebase-move-line-up)
    (define-key git-rebase-mode-map (kbd "C-j") 'git-rebase-move-line-down)
    (define-key git-rebase-mode-map (kbd "M-k") nil)
    (define-key git-rebase-mode-map (kbd "M-j") nil)
    (define-key git-rebase-mode-map (kbd "gg") 'evil-goto-first-line)
    (define-key magit-hunk-section-map (kbd "C-SPC") 'set-mark-command)
    )

  ;; Snippets: (yas-new-snippet) (yas-reload-all)

  ;; Diff buffer with file on disk: (ediff-current-file). My ediff keybindings:
  (add-hook 'ediff-display-help-hook '(lambda ()
                                        ;; The following required only for spacemacs-base distribution:
                                        ;; (setq ediff-help-message (replace-regexp-in-string "j -jump to diff" "d -first diff  " ediff-help-message))
                                        ;; (setq ediff-help-message (replace-regexp-in-string "p,DEL" "    k" ediff-help-message))
                                        ;; (setq ediff-help-message (replace-regexp-in-string "n,SPC" "    j" ediff-help-message))
                                        (setq ediff-help-message (replace-regexp-in-string "ignore case        |" "ignore case        | ox -go to (open) buf X" ediff-help-message))
                                        ))
  (add-hook 'ediff-keymap-setup-hook (lambda ()
                                       ;; The following required only for spacemacs-base distribution:
                                       ;; (define-key ediff-mode-map "d" '(lambda () (interactive) (ediff-jump-to-difference 1))) ;; original: j
                                       ;; (define-key ediff-mode-map "j" 'ediff-next-difference)                                  ;; original: n,SPC
                                       ;; (define-key ediff-mode-map "k" 'ediff-previous-difference)                              ;; original: p,DEL
                                       ;; (define-key ediff-mode-map " " spacemacs-default-map) ;; Use SPC as leader key instead of (ediff-next-difference).
                                       (define-key ediff-mode-map "oa" '(lambda () (interactive) (select-window ediff-window-A)))
                                       (define-key ediff-mode-map "ob" '(lambda () (interactive) (select-window ediff-window-B)))
                                       (define-key ediff-mode-map "oc" '(lambda () (interactive) (select-window ediff-window-C)))
                                       ))
  ;; Refine diff to character-level (default is to word-level):
  (setq-default ediff-forward-word-function 'forward-char)

  ;; Fix refined ediff highlighting:
  (custom-set-faces
   '(ediff-fine-diff-A ((t (:background "black"))))
   '(ediff-fine-diff-B ((t (:background "black"))))
   '(ediff-fine-diff-C ((t (:background "black"))))
   )

  ;; Fix "describe function" and "describe variable" highlighting:
  (custom-set-faces
   ;; '(ivy-current-match ((t :background "#ff0000" :foreground "white" :weight bold)))
   '(ivy-highlight-face ((t :weight bold)))
   )

  (setq use-file-dialog nil) ;; disable gtk file diailog
  (require 'ag)

  ;; elisp debugging:
  ;; (debug-on-entry 'read-file-name)
  ;; (cancel-debug-on-entry 'read-file-name)
  ;; Get backtrace when I press C-g: M-x (toggle-debug-on-quit). Now do whatever and press C-g.
  ;; See debugger hotkeys: SPC dm. d - step into. c - step out.

  ;; Do not ask for confirmation when visiting symbolic links, which point to git-controlled files
  (setq vc-follow-symlinks nil)

  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))) ;; search results for "word1 word2" are equal to "word2 word1"
  (setq ivy-initial-inputs-alist nil) ;; do not automatically prepend "^" in ivy input
  ;; Disable all automatically loading variables:
  (setq enable-dir-local-variables nil)
  (setq enable-local-variables nil)
  (setq local-enable-local-variables nil)
  (setq enable-local-eval nil)

  (setq sh-here-document-word " EOF") ;; this adds space before EOF in "cat << EOF".
  ;; To see which rules are applied for indentation: C-c C-o.
  ;; To switch style: C-c . (c-set-style). Variable: c-style-alist
  ;; To reindent: SPC j = (spacemacs/indent-region-or-buffer)
  (c-add-style "my_style" '("stroustrup"
                            (c-basic-offset . 4)
                            (c-offsets-alist
                             (inline-open . 0)
                             )
                            ))
  (setq c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "my_style")))
  (my-set-tab-width 4)
  ;; permanently, force TAB to insert just one TAB:
  (global-set-key (kbd "TAB") 'self-insert-command);
  (define-key evil-insert-state-map (kbd "<tab>") (kbd "C-q <tab>"))
  ;; use backspace to delete tab in c-mode:
  (setq c-backspace-function 'backward-delete-char)
  ;; use tabs instead of spaces:
  ;; (setq-default indent-tabs-mode t)
  ;; (setq indent-tabs-mode t)
  ;; (add-hook 'python-mode-hook (lambda () (setq indent-tabs-mode t)))

  (defun my--c-initialization ()

    ;; Setup cscope:
    (require 'xcscope)
    (cscope-setup)
    (setq cscope-option-do-not-update-database t)
    (define-key cscope-list-entry-keymap [mouse-1] 'cscope-select-entry-other-window)
    (define-key cscope-list-entry-keymap [mouse-3] 'my-close-temporary-windows)
    ;; [S-mouse-3] (cscope-run-last-search-noprompt)

    ;; Setup rtags:
    (require 'rtags)
    (evil-define-key 'motion rtags-mode-map (kbd "RET") 'rtags-select-other-window)
    (define-key rtags-mode-map [mouse-3] 'my-close-temporary-windows)
    ;; (setq rtags-display-result-backend 'ivy)

    ;; Setup right-click menu:
    (defun my-c-mode-right-popup (event)
      "Show a popup menu of commands. See also `choose-from-menu'."
      (interactive "e")
      (mouse-set-point event)
      (eval-expression
       (car
        (read-from-string
         (my--choose-from-menu
          "Commands"
          (list
           ;; (cons "Goto definition using rtags" "(call-interactively 'my-rtags-find-symbol-at-point)")
           (cons "Goto definition using rtags" "(my-rtags-find-symbol-at-point)")
           (cons "Find references using rtags" "(rtags-find-all-references-at-point)")
           (cons "Goto definition using ctags/cscope" "(my-find-tag)")
           (cons "Find references using cscope" "(my-find-references-cscope)")
           ))))))
    ;; (global-set-key [mouse-3] 'my-c-mode-right-popup)
    (define-key cscope-minor-mode-keymap [mouse-3] 'my-c-mode-right-popup)
    )

  (add-hook 'c-initialization-hook 'my--c-initialization)

  (spacemacs/set-leader-keys "or>" 'my-rtags-find-symbol)
  (spacemacs/set-leader-keys "or." 'my-rtags-find-symbol-at-point)
  (spacemacs/set-leader-keys "or<" 'rtags-find-references)
  (spacemacs/set-leader-keys "or," 'rtags-find-all-references-at-point)

  ;; TODO rtags:
  ;; (rtags-current-symbol)
  ;; (car rtags-symbol-history)

  ;; C/C++ autocompletion (cpp_hotkeys):
  ;; C-n - next
  ;; C-p - previous
  ;; C-f - select candidate
  ;; C-g - cancel completion
  ;; Alt+tab - trigger autocompletion manually
  (with-eval-after-load 'company (spacemacs//company-active-navigation nil))
  (define-key evil-insert-state-map (kbd "C-j") 'completion-at-point)
  ;; See also: (company-filter-candidates) (company-search-candidates)
  ;; (company-complete-selection) (company-complete-number)

  (define-key evil-insert-state-map (kbd "C-;") 'my-switch-keyboard-layout)
  (define-key evil-visual-state-map (kbd "C-;") 'my-switch-keyboard-layout)
  (define-key evil-normal-state-map (kbd "C-;") 'my-switch-keyboard-layout)
  (global-set-key [?\C-\ ] 'evil-normal-state) ;; Use ctrl-space as ESC
  ;; (define-key evil-insert-state-map (kbd "C-l") 'evil-normal-state)
  ;; (define-key evil-visual-state-map (kbd "C-l") 'evil-normal-state)

  (define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; quit on ESC
  (define-key evil-visual-state-map "i" 'my-execute-macro)
  (define-key evil-normal-state-map (kbd "C-d") 'my-close-window-or-frame)
  (define-key evil-normal-state-map (kbd "g C-]") 'my-find-tag)
  (evil-define-key 'motion help-mode-map (kbd "C-<return>") 'my-goto-elisp-definition)
  (evil-define-key 'motion help-mode-map (kbd "C-d") 'my-close-window-or-frame)
  (evil-define-key 'motion spacemacs-buffer-mode-map (kbd "C-d") 'my-close-window-or-frame)
  (evil-define-key 'motion Man-mode-map (kbd "C-d") 'my-close-window-or-frame)
  (evil-define-key 'motion Man-mode-map (kbd "q") 'my-close-window-or-frame)
  (evil-define-key 'motion Man-mode-map (kbd "C-o") 'my-previous-buffer)
  (evil-define-key 'motion Man-mode-map (kbd "RET") 'man-follow)
  (evil-define-key 'motion Man-mode-map (kbd "C-s") 'Man-goto-section)
  (evil-define-key 'motion Man-mode-map (kbd "C-n") 'Man-next-section)
  (evil-define-key 'motion Man-mode-map (kbd "C-p") 'Man-previous-section)
  ;; u - reformat current manpage (Man-update-manpage).
  (setq Man-notify-method 'pushy)

  (require 'man)
  (defun Man-notify-when-ready (man-buffer)
    "Notify the user when MAN-BUFFER is ready.
See the variable `Man-notify-method' for the different notification behaviors."
    (let ((saved-frame (with-current-buffer man-buffer
                         Man-original-frame)))
      (pcase Man-notify-method
        (`newframe
         ;; Since we run asynchronously, perhaps while Emacs is waiting
         ;; for input, we must not leave a different buffer current.  We
         ;; can't rely on the editor command loop to reselect the
         ;; selected window's buffer.
         (save-excursion
           (let ((frame (make-frame Man-frame-parameters)))
             (set-window-buffer (frame-selected-window frame) man-buffer)
             ;; (set-window-dedicated-p (frame-selected-window frame) t) ;; my change: do not set window dedicated
             (or (display-multi-frame-p frame)
                 (select-frame frame)))))
        (`pushy
         (switch-to-buffer man-buffer))
        (`bully
         (and (frame-live-p saved-frame)
              (select-frame saved-frame))
         (pop-to-buffer man-buffer)
         (delete-other-windows))
        (`aggressive
         (and (frame-live-p saved-frame)
              (select-frame saved-frame))
         (pop-to-buffer man-buffer))
        (`friendly
         (and (frame-live-p saved-frame)
              (select-frame saved-frame))
         (display-buffer man-buffer 'not-this-window))
        (`polite
         (beep)
         (message "Manual buffer %s is ready" (buffer-name man-buffer)))
        (`quiet
         (message "Manual buffer %s is ready" (buffer-name man-buffer)))
        (_ ;; meek
         (message ""))
        )))

  (evil-define-key 'motion compilation-mode-map "h" 'evil-backward-char)
  (evil-define-key 'visual compilation-mode-map "h" 'evil-backward-char)
  (evil-define-key 'motion compilation-mode-map "0" 'evil-digit-argument-or-evil-beginning-of-line)
  (evil-define-key 'visual compilation-mode-map "0" 'evil-digit-argument-or-evil-beginning-of-line)
  (evil-define-key 'motion ag-mode-map "n" 'evil-search-next)
  (evil-define-key 'visual ag-mode-map "n" 'evil-search-next)
  (evil-define-key 'motion ag-mode-map (kbd "C-n") 'compilation-next-error)
  (evil-define-key 'motion ag-mode-map (kbd "C-p") 'compilation-previous-error)
  (define-key compilation-mode-map (kbd "C-n") 'compilation-next-error)
  (define-key compilation-mode-map (kbd "C-p") 'compilation-previous-error)

  (define-key evil-normal-state-map "G" 'my-goto-last-line)

  (add-hook 'compilation-mode-hook '(lambda () (local-set-key "\C-d" 'my-close-window-or-frame)))
  (spacemacs/set-leader-keys "qm" 'my-open-new-man-page)
  (spacemacs/set-leader-keys "ds" 'bookmark-set)
  (spacemacs/set-leader-keys "dd" 'bookmark-delete)
  (spacemacs/set-leader-keys "dq" 'my-close-temporary-windows)
  (spacemacs/set-leader-keys "do" 'my-restore-temp-window)
  (spacemacs/set-leader-keys "dm" 'my-modify-lines)
  (spacemacs/set-leader-keys "di" 'my-update-include-guards)
  (spacemacs/set-leader-keys "is" 'my-insert-snippet)
  (spacemacs/set-leader-keys "wa" 'my-save-all-buffers)
  (spacemacs/set-leader-keys "ot" 'my-select-tags)
  ;; (spacemacs/set-leader-keys "qr" 'tags-reset-tags-tables)
  (spacemacs/set-leader-keys "oe" 'my-clear-current-line)
  (spacemacs/set-leader-keys "of" 'my-configure-build-run)
  (spacemacs/set-leader-keys "op" 'my-print-shell-command)
  (spacemacs/set-leader-keys "os" 'my-select-shell-command)
  (spacemacs/set-leader-keys "oc" 'my-select-project)
  (spacemacs/set-leader-keys "od" 'my-edit-project-definition)
  (spacemacs/set-leader-keys "cc" 'my-edit-shell-command)
  (spacemacs/set-leader-keys "cl" 'my-copy-location-to-clipboard)

  ;; search hotkeys:
  (spacemacs/set-leader-keys "sm" 'my-search-in-directory-recursive) ;; use ag, ivy interface
  (spacemacs/set-leader-keys "sa" 'my-search-in-directory-ag) ;; use ag.el
  (spacemacs/set-leader-keys "st" 'my-find-tag) ;; g C-] (ctags)
  ;; (spacemacs/set-leader-keys "sd" 'cscope-find-global-definition) ;; C-c s g, C-c s d find symbol's definition.
  (spacemacs/set-leader-keys "su" 'my-find-references-cscope) ;; (cscope-find-this-symbol) find all references (+definition) of symbol.

  (spacemacs/set-leader-keys "mel" 'lisp-state-eval-sexp-end-of-line) ;; evaluate lisp expression at the end of the current line
  (spacemacs/set-leader-keys "mef" 'eval-defun) ;; evaluate lisp function (the function our cursor is in)
  (spacemacs/set-leader-keys "mee" 'eval-last-sexp) ;; evaluate lisp expression at cursor
  (spacemacs/set-leader-keys "meh" 'my-evaluate-last-expression-from-history) ;; evaluate last expression from history
  (spacemacs/set-leader-keys "meu" 'my-elisp-testcase-undo)
  (spacemacs/set-leader-keys "et" 'my-this-error)
  (spacemacs/set-leader-keys "ef" 'my-first-error)
  (spacemacs/set-leader-keys "ws" 'my-split-and-open-buffer-below)
  (spacemacs/set-leader-keys "bn" 'my-next-buffer)
  (spacemacs/set-leader-keys "bp" 'my-previous-buffer)

  (global-set-key (kbd "C-=") 'my-text-scale-increase)
  (global-set-key (kbd "C--") 'my-text-scale-decrease)
  (global-set-key (kbd "<C-mouse-4>") 'my-text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'my-text-scale-decrease)
  (global-set-key (kbd "M-=") 'my-text-scale-default)

  ;; Leader hotkeys:
  ;; ? search for a hotkey (counsel-descbinds)
  ;; jj - jump to the currently visible char (evil-avy-goto-char)
  ;; sc - clear highlight (spacemacs/evil-search-clear-highlight)
  ;; sF - search symbol under cursor (ag)
  ;; zx - change font (spacemacs/scale-font-transient-state/body)
  ;; xu - convert selected region to lower case
  ;; xU - convert selected region to upper case
  ;; u SPC en - go to first error (next-error 1 t)
  ;; nf - narrow the buffer to the current function (narrow-to-defun)
  ;; np - narrow the buffer to the visible page (narrow-to-page)
  ;; nr - narrow the buffer to the selected text (narrow-to-region)
  ;; nw - widen, i.e show the whole buffer again (widen)
  ;; qq - kill emacs (spacemacs/prompt-kill-emacs)
  ;; qz - kill frame (spacemacs/frame-killer)
  ;; fb - jump to bookmark (bookmark-jump)
  ;; ff - open file (counsel-find-file)
  ;; fed - edit .spacemacs file (spacemacs/find-dotfile)
  ;; fs - save current file (save-buffer)
  ;; [ - (help-go-back)
  ;; ] - (help-go-forward)
  ;; C-h i - emacs help/manuals (info).
  ;; hdk - help key binding (describe-key)
  ;; hdf - help function (counsel-describe-function)
  ;; hdv - help variable (counsel-describe-variable)
  ;; hdm - describe mode (spacemacs/describe-mode)
  ;; tn - toggle line numbers (spacemacs/toggle-line-numbers)
  ;; bR - revert buffer (like :q!, but without exit) (spacemacs/safe-revert-buffer)
  ;; bd - kill this buffer (spacemacs/kill-this-buffer)
  ;; C/C++ (cpp_hotkeys):
  ;; mr - refactor at point (semantic)
  ;; mga - switch source <--> header (cpp <--> hpp), (c <--> h) (projectile-find-other-file)
  ;; mgA - switch source <--> header (open in other window)
  ;; mgg - jump to definition
  ;; mgG - jump to definition (open in other window)
  ;; M-? - find references to symbol under cursor (xref-find-references) (like full text search implemented in elisp)
  ;; (whitespace-mode) display spaces, tabs and newlines.
  ;; Convert spaces to tabs and backwards: (tabify) (untabify).
  ;; (toggle-truncate-lines) - set wrap!
  ;; For available packages see (list-packages) or (package-list-packages).

  ;; minibuffer hotkeys. Functions: (my-search-in-directory-ag).
  (define-key minibuffer-local-map (kbd "C-0") 'evil-delete-whole-line)
  (define-key minibuffer-local-map (kbd "M-i") 'move-beginning-of-line)
  (define-key minibuffer-local-map (kbd "M-a") 'move-end-of-line)

  ;; ivy hotkeys:
  (define-key ivy-minibuffer-map [mouse-3] 'ivy-done)
  (define-key ivy-minibuffer-map [mouse-4] 'ivy-previous-line)
  (define-key ivy-minibuffer-map [mouse-5] 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-h") 'ivy-backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)     ;; C-n
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line) ;; C-p
  (define-key ivy-minibuffer-map (kbd "C-0") 'ivy-kill-line)     ;; C-k
  (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-done)          ;; RET
  (define-key ivy-minibuffer-map (kbd "M-i") 'move-beginning-of-line)
  (define-key ivy-minibuffer-map (kbd "M-a") 'move-end-of-line)
  ;; stop completion and put the current matches into a new buffer: "C-c C-o" (ivy-occur)
  ;; insert from clipboard: "C-y"
  ;; cancel search: "C-g" (keyboard-escape-quit)
  ;; jump to one of the current ivy candidates <c-'> (ivy-avy)
  ;; freeze candidates list and search again among them <s-SPC> (ivy-restrict-to-matches)
  ;; resume (repeat) last (previous) completion session: "SPC r l" (ivy-resume)

  ;; cscope: go to insert mode, use "n" (next), "p" (previous).

  ;; ag (spacemacs/counsel-search) "SPC s m":
  ;; Stop completion and put the current matches into a new buffer: "C-c C-e" (spacemacs//counsel-edit).
  ;; Now we can edit this buffer:
  ;; - apply all changes to corresponding files: ", c" (wgrep-finish-edit)
  ;; - abort (kill, cancel) all changes: ", k" (wgrep-abort-changes)

  ;; ag (ag.el) "SPC s a":
  ;; custom command line: SPC u SPC s a
  ;; next search result: SPC e n, C-n (compilation-next-error)
  ;; previous search result: SPC e p, C-p (compilation-previous-error)
  ;; visit current search result: RET (compile-goto-error)
  ;; automatically visit error under cursor: C-c C-f (next-error-follow-minor-mode)

  ;; vim navigation: f<symbol> - jump to symbol. t<symbol> - jump before symbol.
  ;; [dc]io - delete/change inner object (symbol). [dc]ao - outer object (including space).

  ;; TODO research: iedit mode.
  ;; TODO add evil-exchange.
  ;; TODO spacemacs documentation 10.
  ;; TODO do nothing on C-j in tooltip.

  ;; See also: universal ctags, cxxtags, ebrowse, mural
  ;; complete-tag find-tag-regexp find-tag-other-window find-tag-other-frame
  ;; tags-search list-tags tags-apropos tag-query-replace

  ;; cscope-output-buffer-name cscope-command-map cscope-keymap-prefix

  ;; semantic-ia-fast-jump semantic-complete-jump (works only in current file?)
  ;; semantic-symref semantic-symref-symbol semantic-symref-tool
  ;; ebrowse - class hierarchy

  ;; Search in string:
  ;; (let ((case-fold-search nil)) ;; case-sensitive
  ;;   (string-match "regex" "haystack qwasdqw"))

  ;; To see current encoding: (describe-variable 'buffer-file-coding-system)

  ;; How to create own color theme based on existed one:
  ;; M-x customize-create-theme
  ;; do not include basic face customization
  ;; press "visit theme" (answer "yes" to all questions)
  ;; change "show paren match face" (use "gray 8")

  (setq my--os-settings "~/os_settings")
  (setq magit-repository-directories `(,my--os-settings))
  (setq my--emacs-projects-dir "/media/files/workspace/dotrc_s/emacs_projects")
  (setq my--tags-dir (concat my--emacs-projects-dir "/tags"))
  (my--load-emacs-projects my--emacs-projects-dir)
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
