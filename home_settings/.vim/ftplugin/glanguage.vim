function! Translate()
	" copy line to clipboard
	execute 'normal! 0y$'
	" translate line with goldendict
	call system('goldendict "$(clipboard.sh -o)" &')
endfunction

function! Translate_and_say()
	if &iminsert == 0 " If current keyboard layout is not russian
		call Translate()
		" say line (if LINE.mp3 is available)
		call system('~/os_settings/other_files/say_word.sh "$(clipboard.sh -o)" &')
	endif " else do nothing
endfunction

function! Translate_only()
	if &iminsert == 0 " If current keyboard layout is not russian
		call Translate()
	endif
endfunction

" Insert word from clipboard:
imap <buffer> <C-p> <Esc>PA<CR>
" Insert <shortinfo></shortinfo> and switch language
imap <buffer> <C-i> <shortinfo></shortinfo><Esc><C-k>F<i
" Throw away (undo) current word
imap <buffer> <C-u> <Esc>ddk<C-k>S
" Fix previous word/translation:
imap <buffer> <C-j> <Esc><C-k>ddk

imap <buffer> <CR> <Esc> k :call Translate_and_say()<CR><C-k>o
" imap <buffer> <CR> <Esc> k :call Translate_only()<CR><C-k>o

