source ~/.vim/ftplugin/glanguage_common.vim

function! Translate_and_say_english()
	if &iminsert == 0 " If current keyboard layout is not russian
		call Translate()
		" say line (if LINE.mp3 is available)
		call system('~/os_settings/other_files/say_word.sh "$(clipboard.sh -o)" &')
	endif " else do nothing
endfunction

imap <CR> <Esc> k ;call Translate_and_say_english()<CR><C-k>o

