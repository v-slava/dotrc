;; To evaluate whole buffer use "SPC m e b". See also variable:
;; my--projects-alist

(let* (
       (prj-name "mbr_printer")
       (executable-name prj-name)
       (prj-dir (concat "/media/files/workspace/" prj-name))
       (cd (concat "cd " prj-dir " && "))
       (make (concat cd "make -j12 "))
       )
  (add-to-list 'magit-repository-directories `(,prj-dir . 0))
  (my-register-project prj-name
                       `(("build" . ,make)
                         ("run" . ,(concat make " && echo \"Program's output:\" && ./" prj-name))
                         ("clean" . ,(concat cd "rm -f " executable-name))
                         ("index" . ,(concat "cd " prj-dir " && index_src.sh " prj-name))
                         ;; ("arm build" . "echo TODO && false")
                         ;; ("pc clean" . ,(concat "rm -rf " pc-out))
                         )))
