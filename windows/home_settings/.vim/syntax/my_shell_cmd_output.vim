if exists("b:current_syntax")
  finish
endif

syn match command '^Command: '
hi def link command Constant

syn match command_output ' Command output:$'
hi def link command_output StorageClass

syn match command_succeeded '^Command succeeded (exit code = 0)$'
" hi def link command_succeeded Preproc
hi def link command_succeeded MySuccessMsg

syn match command_failed '^Command failed (exit code = [0-9]*)$'
" hi def link command_failed Statement
" hi def link command_failed ErrorMsg
hi def link command_failed MyErrorMsg

syn match compile_error 'error:'
hi def link compile_error Error

syn match compile_warning 'warning:'
hi def link compile_warning Number

syn match compile_note 'note:'
hi def link compile_note Comment

let b:current_syntax = "my_shell_cmd_output"
