source ~/.vim/ftplugin/glanguage_common.vim

function! Translate_german()
	if &iminsert == 0 " If current keyboard layout is not russian
		call Translate()
	endif
endfunction

imap <buffer> <CR> <Esc> k :call Translate_german()<CR><C-k>o
call German_mapping_toggle()

