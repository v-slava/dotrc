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
  (my-register-project prj-name
                       `(("build" . ,make)
                         ("run" . ,(concat make " && echo \"Program's output:\" && " compiled-file))
                         ("clean" . ,(concat cd "rm -f " executable-name))
                         ("index" . ,(concat "cd " prj-dir " && index_src.sh " executable-name))
                         ;; ("arm build" . "echo TODO && false")
                         ;; ("pc clean" . ,(concat "rm -rf " pc-out))
                         )
                       `(
                         ("default" . ,(let* ((name "default")
                                              (debug-shell-script (funcall gdb-shell-script name))
                                              (debug-commands-file (funcall gdb-commands name))
                                              ) (concat
"cat << EOF1 > " debug-shell-script "
#!/bin/bash

if [ \"\\$1\" = \"emacs\" ]; then
    ARGS=\"--i=mi \"
else
    NATIVE=\"
layout src
\"
fi

cat << EOF2 > " debug-commands-file "
file \"" compiled-file "\"
b main
run
del 1
\\$NATIVE
EOF2
gdb \\$ARGS -x " debug-commands-file "
EOF1
chmod +x " debug-shell-script
my-emacs-text-separator
debug-shell-script " emacs")))
                         )
                         ))
