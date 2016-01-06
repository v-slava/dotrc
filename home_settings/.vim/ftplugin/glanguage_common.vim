function! Translate()
	" copy line to clipboard
	execute 'normal! 0y$'
	" translate line with goldendict
	call system('goldendict "$(clipboard.sh -o)" &')
endfunction

" Insert word from clipboard:
imap <buffer> <Leader>fp <Esc>PA<CR>
" Insert <shortinfo></shortinfo> and switch language
imap <buffer> <Leader>fi <shortinfo></shortinfo><Esc><C-k>F<i
" Throw away (undo) current word
imap <buffer> <Leader>fu <Esc>ddk<C-k>S
" Fix previous word/translation:
imap <buffer> <Leader>fj <Esc><C-k>ddk

