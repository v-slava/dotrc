function! Translate()
	" copy line to clipboard
	execute 'normal! 0y$'
	" translate line with goldendict
	call system('goldendict "$(clipboard.sh -o)" &')
endfunction

" Insert word from clipboard:
imap <C-p> <Esc>PA<CR>
" Insert <shortinfo></shortinfo> and switch language
imap <C-i> <shortinfo></shortinfo><Esc><C-k>F<i
" Throw away (undo) current word
imap <C-u> <Esc>ddk<C-k>S
" Fix previous word/translation:
imap <C-j> <Esc><C-k>ddk

