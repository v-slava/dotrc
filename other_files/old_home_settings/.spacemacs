  (defun my-frame-config ()
	"Frame configuration function (should be called for each new frame)."
	;; (my-zoom-value 5)
	(spacemacs/cycle-spacemacs-theme) ;; needed to fix highlighting of searched text
	;; (without this call it gets yellow - #ffff00). Correct color is #333c45.
	;; The problem is only in server mode. TODO find out what exactly is wrong.
	)

  (defun my-after-make-frame-hook (frame)
	"A hook to be added to after-make-frame-functions. It calls (my-frame-config)."
	(select-frame frame)
	(my-frame-config))

  (defun my-empty-function ()
	"Do nothing."
	)

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

  (defun my-copy-to-clipboard (data)
	"Copy data to clipboard. Based on function xclip-set-selection from xclip-1.3.el."
	(let* ((process-connection-type nil) (proc (cond ((getenv "DISPLAY")
	  (start-file-process-shell-command "my_clipboard" nil "clipboard.sh")))))
	  (when proc
		(process-send-string proc data)
		(process-send-eof proc))
	  data))

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

  (defun my-get-build-error-index-string ()
	"Return a string for modeline, which contains my build error index."
	(format "Error index: %d" my_build_error_index))
  (defun my-increment-build-error-index ()
	"Increment build error index."
	(setq my_build_error_index (1+ my_build_error_index))
	)
  (defun my-decrement-build-error-index ()
	"Decrement build error index."
	(setq my_build_error_index (1- my_build_error_index))
	)

  (defun my-build-run ()
	"Build and run project or single file."
	(interactive)
	(setq my_build_error_index -1)
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
	(comint-send-string "*shell*" (concat cmd "\n")))

  (defun my-filter-compilation-results ()
	"Filter compilation results"
	;; (with-current-buffer "*compilation*"
	;;   (delete-matching-lines "Compilation started")
	;;   (delete-matching-lines "6:2")
	;;   )
	)

  (defun my-goto-error (func-error func-build-error-index)
	"Visit either next or previous error in source code."
	(with-current-buffer "*compilation*"
	  (switch-to-buffer-other-window "*compilation*")
	  (condition-case nil (progn
							(funcall func-error)
							(helm-highlight-current-line)
							(funcall my-build-error-index)
							)
		(error (evil-goto-error nil)))))

  (defun my-next-error ()
	"Visit next error in source code."
	(interactive)
	(my-goto-error 'spacemacs/next-error 'my-increment-build-error-index))

  (defun my-previous-error ()
	"Visit previous error in source code."
	(interactive)
	(my-goto-error 'spacemacs/previous-error 'my-decrement-build-error-index))

  (defun my-current-error()
	"Visit current error in source code."
	(interactive)
	(my-goto-error '(evil-goto-error nil) 'my-empty-function))

  (defun my-display-compilation-results ()
	"Display compilation results in new window."
	(my-filter-compilation-results)
	(setq my_build_error_index 0) ;; first error will be '1' however (see (my-next-error) below)
	(my-next-error))

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
  (add-hook 'after-make-frame-functions 'my-after-make-frame-hook)

  ;; (setq show-trailing-whitespace nil) ;; do not highlight whitespace at end of lines
  (evil-visual-mark-mode)
  (with-eval-after-load 'linum (linum-relative-toggle)) ;; make linums relative by default
  (setq compilation-read-command nil) ;; do not prompt for compile command
  (add-to-list 'compilation-finish-functions 'my-compilation-finish)
  (add-to-list 'display-buffer-alist '("\\*shell\\*" display-buffer-pop-up-frame)) ;; always open shell in new frame
  (add-to-list 'display-buffer-alist '("\\*compilation\\*" display-buffer-no-window)) ;; do not display compilation buffer by default
  (setq compilation-error-screen-columns nil) ;; treat column numbers as character positions instead of screen columns in compilation errors
  ;; Modify modeline (display build error index):
  (setq my_build_error_index -1)
  (spacemacs|define-mode-line-segment my-mode-line-segment-build-error-index (my-get-build-error-index-string) :when (/= -1 my_build_error_index))
  (add-to-list 'spacemacs-mode-line-left 'my-mode-line-segment-build-error-index)
  ;; Avoid matching "from file:line:column:" as a warning.
  ;; For details see http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
  (require 'compile)
  (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  ;; add new settings here

  ;; Put this in .dir-locals.el (also file .projectile may be required):
  ;; ((nil . ((eval . (progn
  ;;     (setq my_run_cmd "/tmp/vim_ide_dir/main.out")
  ;;     (setq my_build_cmd "gcc main.c -Wall -Wextra -o /tmp/vim_ide_dir/main.out")
  ;;     (require 'projectile)
  ;; 	   (puthash (projectile-project-root)
  ;; 	       my_build_cmd
  ;; 	       projectile-compilation-cmd-map)
  ;; 				   )))))

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
  (evil-leader/set-key "dem" 'my-eval-mark-end-of-line)
  (evil-leader/set-key "ob" 'my-build-run)
  ;; (evil-leader/set-key "or" 'my-rebuild-run)
  ;; (evil-leader/set-key "oc" 'my-configure)
  ;; (evil-leader/set-key "od" 'my-distclean)
  ;; (evil-leader/set-key "os" 'my-select-build-configuration)

  ;; TODO main:
  ;; (info) (info "elisp")
  ;; make .projectile unnecessary if .dir-locals.el exists
  ;; warnings filter (compilation-filter)
  ;; compile on build server
  ;; multiple configurations (use configuration names, helm to select by name).
  ;; Save current configuration to .dir-locals.el (so that it will be selected on new build/run)
  ;; configure
  ;; rebuild
  ;; load dir-locals.el without prompt http://emacs.stackexchange.com/questions/14753/white-list-of-dir-locals-el
  ;; grep through the project
  ;; (indent-region) visually selected
  ;; rtags
  ;; ctags / cscope
  ;; remap hotkeys for other modes (list-packages, compilation, help, info, ...).
  ;; diff
  ;; hotkey to wrap selected region in braces / quotes / ...
  ;; display "error number 1 of 34" in modeline
  ;; vim configs
  ;; save project session (including frames positions, marks - see (helm-filtered-bookmarks)) on exit (see session management)
  ;; evaluate emacs expression from clipboard
  ;; insert function from help to minibuffer
  ;; , in normal mode - reverse ; (next character)
  ;; auto-fill-mode - move cursor to next line
  ;; flyspell-mode - typo correction
  ;; do not run built program next time if previous run hasn't been finished

  ;; always open help in current frame (not in another one where it was opened earlier) and do not close help in another frames:
  ;; (add-to-list 'same-window-buffer-names "*Help*")
  ;; (add-to-list 'same-window-regexps ".*elp.*")
  ;; special-display-buffer-names

  ;; C-c C-l - toggle electric indentation for C/C++
  ;; M-x whitespace-mode  (variable: whitespace-stile)
  ;; M-x toggle-truncate-lines - set wrap!
  ;; M-x tabify / untabify
  ;; S-M-: evaluate expression

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
