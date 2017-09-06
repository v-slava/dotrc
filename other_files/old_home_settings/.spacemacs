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

  (defun my-system (cmd)
	"Execute shell command. Exit on failure."
	(with-temp-buffer
	  (when (/= 0 (call-process-shell-command cmd nil (current-buffer) nil))
		(error (buffer-string)))))

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
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; (my-frame-config)
  (add-hook 'after-make-frame-functions 'my-after-make-frame-hook)

  ;; (setq show-trailing-whitespace nil) ;; do not highlight whitespace at end of lines
  (evil-visual-mark-mode)
  (add-to-list 'compilation-finish-functions 'my-compilation-finish)
  (add-to-list 'display-buffer-alist '("\\*shell\\*" display-buffer-pop-up-frame)) ;; always open shell in new frame
  (add-to-list 'display-buffer-alist '("\\*compilation\\*" display-buffer-no-window)) ;; do not display compilation buffer by default
  ;; Modify modeline (display build error index):
  (setq my_build_error_index -1)
  (spacemacs|define-mode-line-segment my-mode-line-segment-build-error-index (my-get-build-error-index-string) :when (/= -1 my_build_error_index))
  (add-to-list 'spacemacs-mode-line-left 'my-mode-line-segment-build-error-index)
  ;; Avoid matching "from file:line:column:" as a warning.
  ;; For details see http://stackoverflow.com/questions/15489319/how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
  (require 'compile)
  (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  ;; add new settings here

  ;; TODO main:
  ;; (info) (info "elisp")
  ;; warnings filter (compilation-filter)
  ;; compile on build server
  ;; rtags
  ;; hotkey to wrap selected region in braces / quotes / ...
  ;; display "error number 1 of 34" in modeline
  ;; save project session (including frames positions, marks - see (helm-filtered-bookmarks)) on exit (see session management)
  ;; evaluate emacs expression from clipboard
  ;; insert function from help to minibuffer
  ;; auto-fill-mode - move cursor to next line
  ;; flyspell-mode - typo correction

  ;; always open help in current frame (not in another one where it was opened earlier) and do not close help in another frames:
  ;; (add-to-list 'same-window-buffer-names "*Help*")
  ;; (add-to-list 'same-window-regexps ".*elp.*")
  ;; special-display-buffer-names

  ;; C-c C-l - toggle electric indentation for C/C++

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
