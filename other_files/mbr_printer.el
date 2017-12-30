;; To evaluate whole buffer use "SPC m e b". See also variable:
;; my--projects-alist

(let* (
       (prj-name "mbr_printer")
       (executable-name prj-name)
       (prj-dir (concat "/media/files/workspace/" prj-name))
       (compiled-file (concat prj-dir "/" executable-name))
       (cd (concat "cd " prj-dir " && "))
       (make (concat cd "make -j12 "))
       (gdb-shell-script (lambda (cmd) (concat "/tmp/gdb_" prj-name "_" cmd ".sh")))
       (gdb-commands (lambda (cmd) (concat "/tmp/gdb_" prj-name "_" cmd ".gdb")))
       )
  (add-to-list 'magit-repository-directories `(,prj-dir . 0))
  (my-register-project (make-my--project-definition :name prj-name :commands (make-my--all-frame-commands
:build `(
  ("build" . ,make)
  ("run" . ,(concat make " && echo \"Program's output:\" && " compiled-file))
  ("clean" . ,(concat make "clean"))
  ("index" . ,(concat cd "index_src.sh " executable-name))
  ;; ("arm build" . "echo TODO && false")
  ;; ("pc clean" . ,(concat "rm -rf " pc-out))
)
:interactive `(
               ("run_interactively" . ,(concat "x-terminal-emulator -e ~/os_settings/other_files/vifm_run_command.sh --pause " compiled-file " &"))
               )
:debug `(
         ("default" . ,(let* ((name "default")
                              (debug-shell-script (funcall gdb-shell-script name))
                              (debug-commands-file (funcall gdb-commands name))
                              )
                         (make-my--frame-command--debug
                          :format-str (format
"#!/bin/bash

if [ \"$1\" = \"emacs\" ]; then
    GDB_ARGS=\"--i=mi \"
else
    GDB_NATIVE_CMDS=\"
layout src
\"
fi

cat << EOF > \"%s\"
file \"%s\"
b main
run
%%s
c
del
$GDB_NATIVE_CMDS
EOF
gdb $GDB_ARGS -x \"%s\"" debug-commands-file compiled-file debug-commands-file)
                          :shell-script debug-shell-script :debug-cmd (concat debug-shell-script " emacs")
)))
         )
))))
