" Toggle language: <C-K>
"
" To reformat text to fit max 80 columns: select text, and type 'gq'.
" In normal mode: 'gq' + motion
"
" <C-W>=           - make split windows equal (diff)
" ]c               - advance to the next block with differences
" [c               - reverse search for the previous block with differences
" do (diff obtain) - bring changes from the other file to the current file
" dp (diff put)    - send changes from the current file to the other file
" zo               - unfold/unhide text
" zc               - refold/rehide text
" zr               - unfold both files completely
"
" <C-E> - line up
" <C-Y> - line down
"
" <C-L> - scroll right (my binding)
" <C-H> - scroll left (my binding)
"
" <F12> - copy full source location into clipboard
" <F11> - copy stripped source location into clipboard
"
" in visual mode '=' - fix identation
" Tcomment block comment:
" gb in visual mode (my)
" gcb<motion> in normal mode
"
" \mt : Toggles ShowMarks on and off.
"
" ctags:
" :help tag
" :[count]ta[g][!] {ident}  == <C-]>	jump to the definition of {ident}
" :[count]ta[g][!]		       	Jump to [count] newer entry in tag stack (default 1).
" :[count]po[p][!]	    == <C-T>	Jump to [count] older entry in tag stack (default 1).
" :ts[elect][!] [ident]     == g]	list the tags that match [indent]
" :sts[elect][!] [ident]    == <C-W>g]	Like :ts, but splits the window for the selected tag.
" :tags		Show the contents of the tag stack.  The active entry is marked with a '>'.
"
" cscope:
" Query types:
"   's'   symbol: find all references to the token under cursor
"   'g'   global: find global definition(s) of the token under cursor
"   'c'   calls:  find all calls to the function name under cursor
"   't'   text:   find all instances of the text under cursor
"   'e'   egrep:  egrep search for the word under cursor
"   'f'   file:   open the filename under cursor
"   'i'   includes: find files that include the filename under cursor
"   'd'   called: find functions that function under cursor calls
" :cs  find {querytype} {name}  ==  <C-\>{querytype}
" :scs find {querytype} {name}  ==  <C-space>{querytype} - split window horizontally
" :vert scs find {querytype} {name}  ==  <C-space-space>{querytype} - split window vertically
"
" Make windows equal (vsplit): <C-w>= (standard) or '=' (my)
"
" reload file: :edit
"
" Add (framework/standard C/C++ library) tags: <F8> (see ~/.vim/tags)
" Index source files and update cscope connection: <F9>. Command usage: :SrcIndexOn PRJ_ROOT_PATH
"
" view vim filetypes:
" ls /usr/share/vim/vim74/ftplugin/
" ls /usr/share/vim/vim74/syntax/
"
" set lines=25
" set columns=83

syntax on
colorscheme molokai

" Fix colorscheme:
hi Search ctermfg=0 ctermbg=12
hi DiffAdd ctermbg=234
hi DiffDelete ctermbg=16
hi StatusLine ctermfg=232 ctermbg=46
hi StatusLineNC ctermfg=232 ctermbg=252

" Highlight spaces and tabs in the end of the line as errors:
match Error /\s\+$/

" highlight current line:
set cursorline

" highlight column (right after last that can be used):
set colorcolumn=81
hi ColorColumn ctermbg=234

set hlsearch
set ignorecase
set incsearch
set mouse=a

set autoindent
" set cindent " C-style indents (after '{' and so on)
set number " display line numbers
" set nowrap " do not wrap long lines
set linebreak

" View invisible characters for makefiles:
" autocmd FileType make set list
nmap <F2> :set list!<CR>

" Use F3 to set/unset search highlighting:
nmap <F3> :set hlsearch!<CR>

" Scroll horizontally:
nmap <C-l> zl
nmap <C-h> zh

" Reload vimrc:
nmap <C-m> :source $MYVIMRC<CR>

" Use <ctrl>+D to exit from vim:
nmap <C-d> :q<CR>

" Use <Tab> to change focus to another window (split, vsplit):
nmap <Tab> <C-W>W
" Use '=' to make windows size equal:
nmap = <C-W>=

" Use :Wq to save file as root (you can change % to another file name):
" cmap Wq w !sudo tee >/dev/null %

" Apply macro to selected lines:
vmap 2 :normal @

" Do not capture stderr while using :read
" set shellredir=>%s 2>&1 " - default value
set shellredir=>%s

" Reformat C/C++ source code:
nmap <C-u> :%d<CR>:r !uncrustify -f %<CR>:1,1d<CR>

" Using vifm in vim as file selector:
nmap <C-s> :VsplitVifm<CR>
" :EditVifm :SplitVifm :DiffVifm :TabVifm

" Map clipboard to unnamedplus register '+':
set clipboard=unnamedplus
" Preserve copied text in clipboard on exit:
autocmd VimLeave * call system("clipboard.sh", getreg('+'))

" Set correct filetypes:
" autocmd BufRead,BufNewFile
autocmd BufEnter vifmrc setlocal filetype=vifm
autocmd BufEnter *vifm/colors/* setlocal filetype=vifm
autocmd BufEnter *.i setlocal filetype=c
autocmd BufEnter *.ii setlocal filetype=cpp
autocmd BufEnter *.gdb setlocal filetype=gdb " my filetype extension
autocmd BufEnter *.cmm setlocal filetype=jtag_script " my filetype
autocmd BufEnter menurc setlocal filetype=claws_mail_menurc " my filetype
autocmd BufEnter *_en.gl setlocal filetype=glanguage_en " my filetype
autocmd BufEnter *_de.gl setlocal filetype=glanguage_de " my filetype
autocmd BufEnter * if &filetype == "" | setlocal filetype=unknown | endif

" Set correct syntax:
autocmd FileType asm setlocal syntax=armasm

" Set tab width:
autocmd FileType c,cpp,sh,expect,cmake,vim,python setlocal tabstop=4
autocmd FileType c,cpp,sh,expect,cmake,vim,python setlocal shiftwidth=4

" Auto insert <EOL> and move last word to next line if it reaches 81 column
autocmd FileType c,cpp setlocal textwidth=80

" glanguage mappings:
" Use <C-i> to insert <shortinfo></shortinfo> and switch language
autocmd FileType glanguage_en,glanguage_de imap <C-i> <shortinfo></shortinfo><Esc><C-k>F<i
" Use <C-u> to throw away (undo) current word
autocmd FileType glanguage_en,glanguage_de imap <C-u> <Esc>ddk<C-k>S| call Update_status_line('', 'normal')
function! Translate()
	" copy line to clipboard
	execute 'normal! 0y$'
	" translate line with goldendict
	call system('goldendict "$(clipboard.sh -o)" &')
endfunction
function! Translate_and_say_english()
	if &iminsert == 0 " If current keyboard layout is not russian
		call Translate()
		" say line (if LINE.mp3 is available)
		call system('~/os_settings/other_files/say_word.sh "$(clipboard.sh -o)" &')
	endif " else do nothing
endfunction
function! Translate_german()
	if &iminsert == 0 " If current keyboard layout is not russian
		call Translate()
	endif
endfunction
autocmd FileType glanguage_en imap <CR> <Esc> k :call Translate_and_say_english()<CR><C-k>o
autocmd FileType glanguage_de imap <CR> <Esc> k :call Translate_german()<CR><C-k>o

" Russian keyboard layout:
set keymap=russian-jcukenwin
set iminsert=0 " default english in insert mode
set imsearch=0 " default english while searching

function! Update_status_line(message, status)
	if &iminsert == 1
		let l:lang='RU'
	else
		if g:german == 1
			let l:lang='DE'
		else
			let l:lang='EN'
		endif
	endif
	let &statusline=l:lang . '   file: %f   ' . a:message
	if a:status == 'error'
		hi StatusLine ctermbg=160
	endif
	if a:status == 'normal'
		hi StatusLine ctermbg=46
	endif
	set laststatus=2
	redrawstatus!
endfunction

function! Swap_keyboard_layout()
	if &iminsert == 1 " If current layout is russian
		" then switch to english
		set iminsert=0
		set imsearch=0
	else " if current layout is english
		" then switch to russian
		set iminsert=1
		set imsearch=1
	endif
	call Update_status_line('', 'normal')
endfunction
nmap <C-k> :call Swap_keyboard_layout()<CR>
imap <C-k> <Esc>:call Swap_keyboard_layout()<CR>gi

" German-only letters mapping:
function! German_mapping_toggle()
	if &iminsert == 0 " If current layout is english
		if g:german == 1 " If german mapping is enabled
			iunmap oe
			iunmap OE
			iunmap ae
			iunmap AE
			iunmap ue
			iunmap UE
			iunmap ss
			iunmap SS
			let g:german = 0
		else " if german mapping is disabled
			imap oe ö
			imap OE Ö
			imap ae ä
			imap AE Ä
			imap ue ü
			imap UE Ü
			imap ss ß
			imap SS ẞ
			let g:german = 1
		endif " if german mapping is enabled
	endif " if current layout is english
	call Update_status_line('', 'normal')
endfunction
let g:german = 0 " disable german mappings by default
autocmd FileType glanguage_de call German_mapping_toggle()
nmap <F7> :call German_mapping_toggle()<CR>
imap <F7> <Esc>:call German_mapping_toggle()<CR>gi

" Move current tab left and right:
nnoremap <silent> <S-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <S-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Open tags file in $(dirname EDITED_FILE), if not present then in $(dirname EDITED_FILE)/.. and until /
set tags=./tags;,tags;

" Add (framework/standard C/C++ library) tags:
nmap <F8> :set tags+=~/.vim/tags/std.ctags<CR> :cs add ~/.vim/tags/std.cscope<CR>

" Index source files and update cscope connection. Command usage: :SrcIndexOn PRJ_ROOT_PATH
nmap <F9> :SrcIndexOn 

let g:build_cmd = system('~/os_settings/other_files/get_default_build_cmd.sh ' . expand('%:t'))
let g:run_cmd =   system('~/os_settings/other_files/get_default_run_cmd.sh ' . expand('%:t'))
let g:config_cmd =  'echo "no config_cmd defined" && false'
let g:rebuild_cmd = 'echo "no rebuild_cmd defined" && false'
let g:error_index = -1
let g:warnings = 'w' " change to 'nw' if you want to suppress warnings
let g:filter = 'nf' " change to shell script name if you want to filter issues
function! Build_and_run(build_cmd, run_cmd, warnings, filter)
	call Update_status_line('Build started...', 'normal')
	let g:OUT_DIR = '/tmp/vim_ide_dir'
	let l:full_build_cmd = "echo '" . a:build_cmd . "' | bash 2>&1 | ~/os_settings/other_files/log_errors.sh " . a:warnings . ' ' . a:filter . ' ; echo -n ${PIPESTATUS[1]}'
	let l:build_exit_code = system(l:full_build_cmd)
	let l:src_loc_file = g:OUT_DIR . '/source_locations'
	let l:issues_found = filereadable(l:src_loc_file)
	if l:build_exit_code == 0 && !l:issues_found " if build succeeded
		let l:run_log = g:OUT_DIR . '/run_log'
		call system('echo -e "Output:\n" > ' . l:run_log)
		call Update_status_line('Running...', 'normal')
		call system("echo '" . a:run_cmd . "' | bash 1>>" . l:run_log . ' 2>&1')
		let l:run_exit_code = v:shell_error
		call system('echo -en "\nExit code: ' . l:run_exit_code . '" >> ' . l:run_log)
		execute 'botright pedit ' . l:run_log
		call Update_status_line('Running is done. Exit code: ' . l:run_exit_code, 'normal')
	else " build failed => open error log for first error and goto source location
		if l:issues_found
			let g:source_locations = readfile(l:src_loc_file)
			let g:error_index = 0
			call Show_error(g:error_index)
		else
			execute 'botright pedit ' . g:OUT_DIR . '/full_file'
			call Update_status_line('Build failed. Exit code: ' . l:build_exit_code, 'normal')
		endif
	endif
endfunction
" build begin (with warinings):
nmap \bb :w<CR>:call Build_and_run(g:build_cmd, g:run_cmd, g:warnings, g:filter)<CR>
" rebuild begin (with warinings):
nmap \br :w<CR>:call Build_and_run(g:rebuild_cmd, g:run_cmd, g:warnings, g:filter)<CR>

function! Configure(config_cmd)
	call Update_status_line('Config started...', 'normal')
	let l:config_log = '/tmp/vim_config_log'
	let l:full_config_cmd = "echo '" . a:config_cmd . "' | bash 1>" . l:config_log . ' 2>&1'
	call system(l:full_config_cmd)
	let l:config_exit_code = v:shell_error
	call system('echo -en "\nExit code: ' . l:config_exit_code . '" >> ' . l:config_log)
	execute 'botright pedit ' . l:config_log
	call Update_status_line('Config is done. Exit code: ' . l:config_exit_code, 'normal')
endfunction
nmap \bc :w<CR>:call Configure(g:config_cmd)<CR>

function! Show_error( error_index )
	let l:current_source_location = get(g:source_locations, a:error_index)
	execute ':edit ' . l:current_source_location
	execute 'botright pedit ' . g:OUT_DIR . '/message_error_' . a:error_index
	call Update_status_line('error_index = ' . a:error_index, 'normal')
endfunction

function! Show_next_error()
	if g:error_index == -1 " if there was no build
		call Update_status_line('Error: unable to show next build error', 'error')
	else
		let l:last_error_index = len(g:source_locations) - 1
		if g:error_index == l:last_error_index
			call Update_status_line('Error: unable to show next build error', 'error')
		else
			let g:error_index = g:error_index + 1
			call Show_error(g:error_index)
		endif
	endif
endfunction
" build next error:
nmap \bn :w<CR>:call Show_next_error()<CR>

function! Show_prev_error()
	if g:error_index <= 0 " if there is no previous error
		call Update_status_line('Error: unable to show previous build error', 'error')
	else
		let g:error_index = g:error_index - 1
		call Show_error(g:error_index)
	endif
endfunction
" build previous error:
nmap \bp :w<CR>:call Show_prev_error()<CR>

function! Copy_location(in_file, strip_part)
	let l:line_number = line('.')
	let l:full_location = a:in_file . ':' . l:line_number
	let l:strip_width = strlen(a:strip_part)
	if strip_width
		let @+ = strpart(l:full_location, l:strip_width)
	else
		let @+ = l:full_location
	endif
endfunction
nmap <F12> :call Copy_location( expand('%:p'), '' )<CR>
nmap <F11> :call Copy_location( expand('%:p'), '/home/volkov/workspace/project_root_dir/' )<CR>


" Update cscope connection while opening new file:
autocmd FileType c,cpp,asm call Cscope_connect()

" Initialize pathogen plugin (update runtimepath variable):
execute pathogen#infect()

" tagfinder plugin:
runtime bundle/tagfinder/plugin/tagfinder.vim
" DefineTagFinder USER_COMMAND type1[,type2]...
DefineTagFinder Tclass c
DefineTagFinder Tdefine d
" Values inside an enumeration:
DefineTagFinder Tenumerator e
DefineTagFinder Tfunction f
" Enumeration names:
DefineTagFinder Tenumeration g
" Class, struct and union members:
DefineTagFinder Tmember m
DefineTagFinder Tnamespace n
DefineTagFinder Tprototype p
DefineTagFinder Tstruct s
DefineTagFinder Ttypedef t
DefineTagFinder Tunion u
DefineTagFinder Tvariable v
"DefineTagFinder Tx x		" external and forward variable declarations

" Marks:
let g:showmarks_enable=0
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
hi ShowMarksHLl   cterm=bold ctermfg=black ctermbg=white
hi ShowMarksHLu   cterm=bold ctermfg=black ctermbg=white
hi ShowMarksHLm   cterm=bold ctermfg=black ctermbg=white

" Tcomment:
vmap gb :TCommentBlock<CR>
call tcomment#DefineType('unknown', '# %s')
call tcomment#DefineType('make', '# %s')
call tcomment#DefineType('gdb', '# %s')
call tcomment#DefineType('kconfig', '# %s')
call tcomment#DefineType('sudoers', '# %s')
call tcomment#DefineType('inittab', '# %s')
call tcomment#DefineType('mplayerconf', '# %s')
call tcomment#DefineType('aptconf', '// %s')
call tcomment#DefineType('jtag_script', '; %s')
call tcomment#DefineType('claws_mail_menurc', '; %s')
call tcomment#DefineType('fusesmbconf', '; %s')
call tcomment#DefineType('asm', '/* %s */')
call tcomment#DefineType('texinfo', '@c %s')
call tcomment#DefineType('xdefaults', '! %s')
call tcomment#DefineType('vifm', '" %s')

" localvimrc:
let g:localvimrc_count = 1 " on the way from root, the last 1 file is sourced
" accept .lvimrc files in /home/volkov/* and /media/files/*:
let g:localvimrc_whitelist='^\(/home/volkov/*\|/media/files/*\)'
" ignore .lvimrc files on mounted filesystems:
let g:localvimrc_blacklist='^/media/*'
let g:localvimrc_ask = 1 " ask before loading a vimrc file (0 = don't ask)
let g:localvimrc_sandbox = 0 " don't use sandbox (1 = use)

" Code to convert spaces to \n and backwards:
function! Split_lines() range
	let l:first_line = a:firstline
	let l:last_line = a:lastline
	let l:lines_array = getline(l:first_line, l:last_line)
	let l:orig_text = join(l:lines_array, '\n')
	" Delete original text into register z:
	execute l:first_line ',' last_line 'delete z'
	let l:words_array = split(l:orig_text)
	let l:failed = append(l:first_line - 1, l:words_array)
endfunction
command! -range -nargs=* Slines <line1>,<line2> call Split_lines()
function! Merge_lines() range
	let l:first_line = a:firstline
	let l:last_line = a:lastline
	let l:lines_array = getline(l:first_line, l:last_line)
	let l:orig_text = join(l:lines_array, ' ')
	" Delete original text into register z:
	execute l:first_line ',' l:last_line 'delete z'
	let l:failed = append(l:first_line - 1, l:orig_text)
endfunction
command! -range -nargs=* Mlines <line1>,<line2> call Merge_lines()

" doxygen: function begin:
nmap \fb i/**<Esc>o * @fn 
" doxygen: function end:
nmap \fe :call End_function()<CR>
function! End_function()
	let l:fn_line_number = line('.')
	let l:fn_line = getline('.')

	let l:prototype = strpart(l:fn_line, 6) " extra space in the front present
	let l:brace_index = stridx(l:prototype, '(') " starts from 0
	let l:prototype_before_brace = strpart(l:prototype, 0, l:brace_index)
	let l:prototype_after_brace = substitute(strpart(l:prototype, l:brace_index + 1), ')', ', ', "")

	" Insert function prototype:
	call append(l:fn_line_number, '}')
	call append(l:fn_line_number, '{')
	call append(l:fn_line_number, strpart(l:prototype, 1))
	call append(l:fn_line_number, ' */')

	" Insert @return if needed:
	let l:void_index = stridx(l:prototype_before_brace, ' void ')
	if l:void_index == -1
		call append(l:fn_line_number, ' * @return ')
	endif

	" Insert @param:
	let l:param_words = split(l:prototype_after_brace, ' \zs')
	let l:param_names = []
	for word in l:param_words
		let l:word_len = strlen(l:word)
		let l:symbol = strpart(l:word, l:word_len - 2, 1)
		if l:symbol == ','
			let l:param_name = strpart(l:word, 0, l:word_len - 2)
			call add(l:param_names, l:param_name)
		endif
	endfor
	call reverse(l:param_names)
	for param in l:param_names
		call append(l:fn_line_number, ' * @param ' . l:param . ' ')
	endfor

	" Insert @brief:
	call append(l:fn_line_number, ' * @brief ')
	" Set cursor to proper position and go to insert mode:
	execute 'normal! j$'
	startinsert!
endfunction

" function! MyCIndent() range
" 	let l:first_line = a:firstline
" 	let l:last_line = a:lastline
" 	let l:lines_array = getline(l:first_line, l:last_line)
" 	silent execute l:first_line ',' l:last_line 'w! /tmp/asd'
" 	silent execute '!cat /tmp/asd | ./c_indent.bin > /tmp/qwe'
" 	" Delete original text into register z:
" 	silent execute l:first_line ',' l:last_line 'delete z'
" 	silent execute 'normal k'
" 	silent execute 'r /tmp/qwe'
" 	execute 'redraw!'
" endfunction
" command! -range -nargs=* MyIndent <line1>,<line2> call MyCIndent()

" Macros:
" You can use <C-o>q to finish recording while in insert mode.
" <C-o> in insert mode allows you to execute one command in
" normal mode, and then returns to insert mode (see :help i^O).

" Insert <shortinfo> tags (for glanguage):
" let @s = 'A<shortinfo></shortinfo>F<i'

