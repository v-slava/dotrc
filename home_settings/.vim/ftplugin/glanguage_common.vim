function! Translate()
	" copy line to clipboard
	execute 'normal! 0y$'
	" translate line with goldendict
	call system('goldendict "$(clipboard.sh -o)" &')
endfunction

" Insert word from clipboard:
imap <Leader>fp <Esc>PA<CR>
" Insert <shortinfo></shortinfo> and switch language
imap <Leader>fi <shortinfo></shortinfo><Esc><C-k>F<i
" Throw away (undo) current word
imap <Leader>fu <Esc>ddk<C-k>S
" Fix previous word/translation:
imap <Leader>fj <Esc><C-k>ddk

