" Rtags:
" Run rdm, select project by running "rc -J <project dir>".
" Project dir should contain file "compile_commands.json".
" +------------+------------------+--------------------------------------------+
" | Mapping    | rc flag          | Description                                |
" +------------+------------------+--------------------------------------------+
" | <Leader>ri | -U               | Symbol info                                |
" | <Leader>rj | -f               | Follow location (jump)                     |
" | <Leader>rJ | -f --declarations-only | Follow location only declarations    |
" | <Leader>rh | -f               | Follow location (open in horizontal split) |
" | <Leader>rv | -f               | Follow location (open in vertical split)   |
" | <Leader>rt | -f               | Follow location open in a new tab          |
" | <Leader>rp | -U --symbol-info-include-parents      |        Jump to parent |
" | <Leader>rf | -e -r            | Find references                            |
" | <Leader>rn | -I -ae -R        | Find references by name                    |
" | <Leader>rN | -ae -R           | Find references by name (case sensitive)   |
" | <Leader>rs | -a -I -F         | Find symbols by name                       |
" | <Leader>rS | -a -F            | Find symbols by name (case sensitive)      |
" | <Leader>rx | -V               | Reindex current file                       |
" | <Leader>rl | -w               | List all available projects                |
" | <Leader>rr | -e -r --rename   | Rename symbol under cursor                 |
" | <Leader>r= | -k -r            | Find virtuals                              |
" | <Leader>re | -E               | preprocess and reformat                    |
" | <Leader>rE | -E               | preprocess                                 |
" +------------+------------------+--------------------------------------------+
"
" " View man page in vim:
" :Man 2 read
"
" Resize vim window:
" :[vertical] resize {+|-}<NUMBER><CR>
"
" List all sourced script names:
" :scriptnames
"
" Generate helptags:
" cd ~/.vim/bundle/SOME_PLUGIN
" vim -c "helptags doc | q"
"
" :help string-functions
"
" Open quickfix window:
" :copen
"
" Open location list window:
" :lopen
"
" To reindent json: 1) Make a selection. 2) :!python -m json.tool
"
" To repeat last colon command (in normal mode): @:, @@, @@, @@, ...
" Toggle language: <C-K>
"
" To reformat text to fit max 80 columns: select text, and type 'gq'.
" In normal mode: 'gq' + motion
"
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
" zt - make cursor position first line on a screen.
" zb - make cursor position last line on a screen.
" zz - make cursor position central line on a screen.
"
" H - move cursor to the first line on a screen (high).
" L - move cursor to the last line on a screen (low).
" M - move cursor to the middle line on a screen (middle).
"
" in visual mode '=' - fix identation
"
" <Leader>mt : Toggles ShowMarks on and off.
"
" reload file: :edit
"
" view vim filetypes:
" ls /usr/share/vim/vim74/ftplugin/
" ls /usr/share/vim/vim74/syntax/
"
" set lines=25
" set columns=83
" set langmenu=ru_RU.UTF-8

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
autocmd WinEnter * match Error /\s\+$/

" Set maximum number of tab pages to be opened by the "-p" command line argument:
set tabpagemax=100

" Display symbol '»' as a first symbol for tab:
set listchars=tab:»\ 
set list

" Disable some unused standard plugins:
let g:loaded_getscriptPlugin = 1
let g:loaded_gzip = 1
let g:loaded_2html_plugin = 1
let g:loaded_netrwPlugin = 1
let g:loaded_rrhelper = 1
let g:loaded_tarPlugin = 1
let g:loaded_zipPlugin = 1
let g:loaded_vimballPlugin = 1

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
" set smartindent
set number " display line numbers
set relativenumber
" set nowrap " do not wrap long lines
set linebreak

" Fix backspace befavior:
set backspace=2

" Use spelling:
" See spell files in /usr/share/vim/vim74/spell.
" http://ftp.vim.org/pub/vim/runtime/spell/{ru|en|de}.utf-8.spl
" set spell spelllang=en,de,ru,en_us
nmap <F7> :setlocal spell spelllang=
nmap <F6> :setlocal nospell<CR>

" Use fd instead of <esc> to exit from insert mode:
inoremap fd <esc>
" imap <esc> <nop>

" let mapleader = "\\"
let mapleader = "\<Space>"

" Disable ex mode:
map Q <nop>
" Disable commands history mode:
map q: <nop>

" Use <C-j> as <Backspace> in insert mode:
" inoremap <C-j> <Backspace>
" imap <Backspace> <nop>

" Swap ':' and ';' in normal mode:
" nnoremap ; :
" nnoremap : ;

" Swap ',' and '<' in normal mode:
" nnoremap , <
" nnoremap < ,

" Clear last used search pattern (highlighting, search clear, no highlight search):
nmap <Leader>sc :let @/ = ""<CR>

" Search for text in clipboard:
nmap <Leader>s/ :let @/ = @+<CR>

" Set/unset search highlighting:
nmap <F3> :set hlsearch!<CR>

" Scroll horizontally:
nmap <C-l> zl
nmap <C-h> zh

" Exit from vim:
nmap <C-d> :q<CR>
nmap <Leader>qm :q<CR>

" View next/previous buffer:
nmap <Leader>vn :bnext<CR>
nmap <Leader>vp :bprevious<CR>

" Split vim window:
nmap <Leader>ws :split<CR>
nmap <Leader>wv :vsplit<CR>

" Switch focus to vim window <Leader>w{h|l|j|k}:
nmap <Leader>w <C-w>
" <Leader>w=           - make split windows equal (diff)
" Focus previous window:
nmap <Leader>wp <C-w><C-p>

" Clear current line:
nmap <Leader>oe $d0x

" Use Y to copy until end of line:
nmap Y y$

" Diff this (view only this diff panel, hide another one):
nmap <Leader>dt :resize +1000<CR>:vertical resize +1000<CR>
" To restore diff use "<Leader>w="

" Refresh diff screen (recalculate diff):
nmap <Leader>du :diffupdate<CR>

let g:EasyMotion_do_mapping = 0 " Disable default mappings
" Jump to character within current line:
" nmap <Leader>l <Plug>(easymotion-sl)
" vmap <Leader>l <Plug>(easymotion-sl)
" Jump to character within screen:
nmap <Leader><Leader> <Plug>(easymotion-s)
vmap <Leader><Leader> <Plug>(easymotion-s)

" Apply macro to selected lines:
vmap 2 :normal @

" Do not capture stderr while using :read
" set shellredir=>%s 2>&1 " - default value
set shellredir=>%s

" Reformat C/C++ source code:
" nmap <C-u> :%d<CR>:r !uncrustify -f %<CR>:1,1d<CR>
nmap <C-u> :%d<CR>:r !astyle.sh %<CR>

" Use vifm in vim as file selector:
nmap <C-s> :VsplitVifm<CR>
" :EditVifm :SplitVifm :DiffVifm :TabVifm

" Map clipboard to unnamedplus register '+':
set clipboard=unnamedplus
" Preserve copied text in clipboard on exit:
autocmd VimLeave * call system("clipboard.sh", getreg('+'))

filetype plugin on
" Set correct filetypes:
" autocmd BufRead,BufNewFile
autocmd BufEnter vifmrc setlocal filetype=vifm
autocmd BufEnter *vifm/colors/* setlocal filetype=vifm
autocmd BufEnter *.i setlocal filetype=c
autocmd BufEnter *.ii setlocal filetype=cpp
autocmd BufEnter .spacemacs setlocal filetype=lisp
autocmd BufEnter *.gdb setlocal filetype=gdb " my filetype extension
autocmd BufEnter *.cmm setlocal filetype=jtag_script " my filetype
autocmd BufEnter menurc setlocal filetype=claws_mail_menurc " my filetype
" autocmd BufEnter *.gl setlocal filetype=glanguage " my filetype
autocmd BufEnter * if &filetype == "" | setlocal filetype=unknown | endif

" Set correct syntax:
autocmd FileType asm setlocal syntax=armasm

" Tab settings:
autocmd FileType rust,c,cpp,sh,expect,cmake,vim,python,perl setlocal tabstop=4 | setlocal noexpandtab | setlocal shiftwidth=0
autocmd BufNewFile,BufRead,BufEnter */dotrc/* setlocal noexpandtab

" Auto insert <EOL> and move last word to next line if it reaches 81 column
autocmd FileType c,cpp setlocal textwidth=80 | setlocal formatoptions+=t
" autocmd FileType c,cpp setlocal cindent | setlocal noautoindent

function! IsLQList()
	exec 'redir @x | silent ls | redir END'
	if match(@x,'%a-  "\[Location List\]"') >= 0
		return 1
	elseif match(@x,'%a-  "\[Quickfix List\]"') >= 0
		return 0
	else
		return -1
	endif
endfunction

function! OpenLocation()
	let l:lqList = IsLQList()
	if l:lqList == -1
		exec 'echoerr "Neither Location nor Quickfix List focused."'
		return
	endif
	if l:lqList == 1
		execute "ll " . line('.')
	else
		execute "cc " . line('.')
	endif
	normal zz
endfunction
" Open location in QuickFix window:
autocmd FileType qf nmap <buffer> o :call OpenLocation()<CR>

" Russian keyboard layout:
set keymap=russian-jcukenwin
set iminsert=0 " default english in insert mode
set imsearch=0 " default english while searching
function! Update_status_line(message, status)
	if &iminsert == 1
		let l:lang='RU'
	else
		let l:lang='EN'
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

" Move current tab left and right (move tab):
nmap <silent> <S-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nmap <silent> <S-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Open tags file in $(dirname EDITED_FILE), if not present then in $(dirname EDITED_FILE)/.. and until /
set tags=./tags;,tags;

" Add (framework/standard C/C++ library) tags:
nmap <F8> :set tags+=~/.vim/tags/std.ctags<CR>:cs add ~/.vim/tags/std.cscope<CR>

" Update cscope connection while opening new file:
autocmd FileType c,cpp,asm call Cscope_connect()

" Index source files and update cscope connection. Command usage: :SrcIndexOn PRJ_ROOT_PATH
nmap <F9> :SrcIndexOn 

" ctags:
" :help tag
" :[count]ta[g][!] {ident}  == <C-]> jump to the definition of {ident}
" :[count]ta[g][!]         Jump to [count] newer entry in tag stack (default 1).
" :[count]po[p][!]          == <C-T> Jump to [count] older entry in tag stack (default 1).
" :ts[elect][!] [ident]     == g]    list the tags that match [indent]
" :sts[elect][!] [ident]    == <C-W>g] Like :ts, but splits the window for the selected tag.
" :tags  Show the contents of the tag stack.  The active entry is marked with a '>'.

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

function Window_is_temporary()
	let l:dir_name = expand('%:p:h')
	let l:file_name = expand('%:t')
	if l:dir_name == '/usr/share/vim/vim74/doc' || l:dir_name == g:ide_dir || &l:buftype == 'help' || &l:buftype == 'quickfix' || l:file_name == 'search-results.agsv'
		return 1
	endif
	let l:last_part_expected = '.fugitiveblame'
	let l:len_expected = strlen(l:last_part_expected)
	let l:len_actual = strlen(l:file_name)
	if l:len_actual > l:len_expected
		let l:last_part_actual = strpart(l:file_name, l:len_actual - l:len_expected)
		if l:last_part_actual == l:last_part_expected
			return 1
		endif
	endif
	if &pvw == 1 " If it is a preview window
		return 1
	endif
	return 0
endfunction
function Close_window_if_temporary()
	if Window_is_temporary() != 0
		execute ':q'
	endif
endfunction
command! CloseWindowIfTemporary call Close_window_if_temporary()
nmap <silent> <Leader>dq :windo CloseWindowIfTemporary<CR>

let g:Modify_line___text_to_prepend = '        '
let g:Modify_line___start_column = 78
let g:Modify_line___text_to_append = ' \'
function Modify_line(text_to_prepend, start_column, text_to_append)
	let l:cur_line = getline('.')
	" Prepend text:
	let l:cur_line = substitute(l:cur_line, "^[ \t]*", a:text_to_prepend, "")
	" Delete text ending if necessary:
	let l:cur_line_len = strlen(l:cur_line)
	let l:append_text_len = strlen(a:text_to_append)
	let l:cur_line_ending = strpart(l:cur_line, l:cur_line_len - l:append_text_len)
	if l:cur_line_ending == a:text_to_append
		" Need to delete text ending
		let l:cur_line = substitute(l:cur_line, " *" . a:text_to_append, "", "")
		let l:cur_line_len = strlen(l:cur_line)
	endif
	let l:line_ending = a:text_to_append
	let l:i = l:cur_line_len
	while l:i < a:start_column
		let l:line_ending = ' ' . l:line_ending
		let l:i = l:i + 1
	endwhile
	" Update line:
	call setline('.', l:cur_line . l:line_ending)
endfunction
nmap <silent> <Leader>dm :call Modify_line(g:Modify_line___text_to_prepend, g:Modify_line___start_column, g:Modify_line___text_to_append)<CR>0
command! -range ModifyLine <line1>,<line2> call Modify_line(g:Modify_line___text_to_prepend, g:Modify_line___start_column, g:Modify_line___text_to_append)

function! ViewInNewBuffer(cmd)
	redir @z
	execute 'silent ' . a:cmd
	redir END
	tabnew
	put z
endfunction
command! -nargs=1 ViewInNewBuffer call ViewInNewBuffer(<f-args>)
" Usage example: :ViewInNewBuffer :map<CR> (show mappings in buffer).

let g:ide_dir = '/tmp/ide_dir' . expand('%:p')
let g:build_cmd = system('~/os_settings/other_files/get_default_build_cmd.sh "' . g:ide_dir . '" "' . expand('%:t') . '"')
let g:build_cmd_all = g:build_cmd
let g:run_cmd =   system('~/os_settings/other_files/get_default_run_cmd.sh "' . g:ide_dir . '" "' . expand('%:t') . '"')
let g:config_cmd =  'echo "no config_cmd defined" && false'
let g:rebuild_cmd = 'echo "no rebuild_cmd defined" && false'
let g:src_loc_relative_prefix = ''
let g:src_loc_absolute_prefix = ''
let g:error_index = -1
let g:warnings = 'w' " change to 'nw' if you want to suppress warnings
let g:filter = 'nf' " change to shell script name if you want to filter issues
let g:run_interactive = 'false' " change to 'true' to spawn terminal
let g:build_log_file = g:ide_dir . '/build_log'
function Log_cmd(cmd, logFile)
	let l:file = a:logFile . '.sh'
	call writefile(['#!/bin/bash', '', 'set -e', 'cd "' . expand('%:p:h') . '"', '', a:cmd], l:file)
	call system("chmod +x '" . l:file . "'")
endfunction
function! Build_and_run(build_cmd, run_cmd, warnings, filter, run_interactive)
	call Update_status_line('Build started...', 'normal')
	let l:full_build_cmd = "echo '" . a:build_cmd . "' | bash 2>&1 | ~/os_settings/other_files/log_errors.sh " . g:ide_dir . ' ' . a:warnings . ' ' . a:filter . ' ; echo -n ${PIPESTATUS[1]}'
	let l:build_exit_code = system(l:full_build_cmd)
	call Log_cmd(a:build_cmd, g:ide_dir . '/build_cmd')
	call writefile([l:build_exit_code], g:ide_dir . '/build_exit_code')
	let l:log_line_numbers_file = g:ide_dir . '/build_log_line_numbers'
	let l:issues_found = getfsize(l:log_line_numbers_file) != 0
	if l:build_exit_code == 0 && !l:issues_found " if build succeeded
		if a:run_interactive == 'false'
			let l:run_log_file = g:ide_dir . '/run_log'
			call Update_status_line('Running...', 'normal')
			call Log_cmd(a:run_cmd, g:ide_dir . '/run_cmd')
			call system("echo '" . a:run_cmd . "' | bash 1>" . l:run_log_file . ' 2>&1')
			let l:run_exit_code = v:shell_error
			call writefile([l:run_exit_code], g:ide_dir . '/run_exit_code')
			execute 'botright pedit ' . l:run_log_file
			call Update_status_line('Running completed. Exit code: ' . l:run_exit_code, 'normal')
		else " a:run_interactive == 'true'
			call Update_status_line('Running started (may be ended now)...', 'normal')
			call system("x-terminal-emulator -e $HOME/os_settings/other_files/vifm_run_command.sh --pause " . a:run_cmd)
			" if !has('nvim')
			" 	call Update_status_line('Interactive mode is only available in neovim', 'error')
			" else
			" 	call Update_status_line('Running...', 'normal')
			" 	execute 'botright split term://' . a:run_cmd
			" 	startinsert
			" 	" run_interactive exit code:
			" 	" NVIM_LISTEN_ADDRESS
			" 	" call Update_status_line('Running is done. Exit code: ' . l:run_exit_code, 'normal')
			" endif
		endif
	else " build failed => open error log for first error and goto source location
		if l:issues_found
			let g:build_log = readfile(g:build_log_file)
			let g:build_log_line_numbers = readfile(l:log_line_numbers_file)
			let g:error_index = 0
			call Show_error(g:error_index)
		else
			execute 'botright pedit ' . g:ide_dir . '/build_log'
			call Update_status_line('Build failed. Exit code: ' . l:build_exit_code, 'normal')
		endif
	endif
endfunction
nmap <silent> <Leader>ob :wa<CR>:call Build_and_run(g:build_cmd, g:run_cmd, g:warnings, g:filter, g:run_interactive)<CR>
nmap <silent> <Leader>of :wa<CR>:call Build_and_run(g:build_cmd_fast, g:run_cmd, g:warnings, g:filter, g:run_interactive)<CR>
nmap <silent> <Leader>oa :wa<CR>:call Build_and_run(g:build_cmd_all, g:run_cmd, g:warnings, g:filter, g:run_interactive)<CR>
nmap <silent> <Leader>or :wa<CR>:call Build_and_run(g:rebuild_cmd, g:run_cmd, g:warnings, g:filter, g:run_interactive)<CR>

function! Configure(config_cmd)
	call Update_status_line('Config started...', 'normal')
	let l:config_log_file = g:ide_dir . '/config_log'
	call system('rm -rf ' . g:ide_dir . ' && mkdir -p ' . g:ide_dir)
	call Log_cmd(a:config_cmd, g:ide_dir . '/config_cmd')
	let l:full_config_cmd = "echo '" . a:config_cmd . "' | bash 1>" . l:config_log_file . ' 2>&1'
	call system(l:full_config_cmd)
	let l:exit_code = v:shell_error
	call writefile([l:exit_code], g:ide_dir . '/config_exit_code')
	execute 'botright pedit ' . l:config_log_file
	call Update_status_line('Configure is done. Exit code: ' . l:exit_code, 'normal')
endfunction
nmap <silent> <Leader>oc :wa<CR>:call Configure(g:config_cmd)<CR>

function! Show_error(error_index)
	let l:log_line_number = g:build_log_line_numbers[a:error_index]
	let l:build_log_line = g:build_log[l:log_line_number - 1]
	let l:line = l:build_log_line
	let l:slash_index = match(l:line, "/")
	if l:slash_index == 0 " absolute path
		let l:line = g:src_loc_absolute_prefix . l:line
	else
		let l:line = g:src_loc_relative_prefix . l:line
	endif
	let l:current_source_location = GetFileNameUnderCursor(l:line)
	let l:in_errors_window = Window_is_temporary()
	if l:in_errors_window != 0
		" switch to source code window
		execute ':wincmd k'
	endif
	execute ':edit ' . l:current_source_location
	execute 'botright pedit ' . g:build_log_file . ':' . l:log_line_number
	if l:in_errors_window != 0
		" switch back to errors window
		execute ':wincmd j'
	endif
	call Update_status_line('error_index = ' . a:error_index, 'normal')
endfunction

" Open error messages window again:
nmap <Leader>ec :wa<CR>:call Show_error(g:error_index)<CR>

function! Show_next_error()
	if g:error_index == -1 " if there was no build
		call Update_status_line('Error: unable to show next build error', 'error')
	else
		let l:last_error_index = len(g:build_log_line_numbers) - 1
		if g:error_index == l:last_error_index
			let l:was_last_error = 1
		else
			let l:was_last_error = 0
			let g:error_index = g:error_index + 1
		endif
		call Show_error(g:error_index)
		if l:was_last_error == 1
			call Update_status_line('Error: unable to show next build error', 'error')
		endif
	endif
endfunction
nmap <Leader>en :wa<CR>:call Show_next_error()<CR>

function! Show_prev_error()
	if g:error_index == -1 " if there was no build
		call Update_status_line('Error: unable to show previous build error', 'error')
	else
		if g:error_index == 0 " if there is no previous error
			let l:was_first_error = 1
		else
			let l:was_first_error = 0
			let g:error_index = g:error_index - 1
		endif
		call Show_error(g:error_index)
		if l:was_first_error == 1
			call Update_status_line('Error: unable to show previous build error', 'error')
		endif
	endif
endfunction
nmap <Leader>ep :wa<CR>:call Show_prev_error()<CR>

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
" Copy full source location into clipboard:
nmap <F12> :call Copy_location( expand('%:p'), '' )<CR>
" nmap <F11> :call Copy_location( expand('%:p'), '/home/volkov/workspace/project_root_dir/' )<CR>

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
"DefineTagFinder Tx x  " external and forward variable declarations

" Marks:
let g:showmarks_enable=0
let g:showmarks_include="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
hi ShowMarksHLl   cterm=bold ctermfg=black ctermbg=white
hi ShowMarksHLu   cterm=bold ctermfg=black ctermbg=white
hi ShowMarksHLm   cterm=bold ctermfg=black ctermbg=white

" Tcomment:
call tcomment#DefineType('unknown', '# %s')
call tcomment#DefineType('make', '# %s')
call tcomment#DefineType('gdb', '# %s')
call tcomment#DefineType('kconfig', '# %s')
call tcomment#DefineType('sudoers', '# %s')
call tcomment#DefineType('inittab', '# %s')
call tcomment#DefineType('mplayerconf', '# %s')
call tcomment#DefineType('text', '# %s')
call tcomment#DefineType('toml', '# %s')
call tcomment#DefineType('aptconf', '// %s')
call tcomment#DefineType('jtag_script', '; %s')
call tcomment#DefineType('claws_mail_menurc', '; %s')
call tcomment#DefineType('fusesmbconf', '; %s')
call tcomment#DefineType('asm', '/* %s */')
call tcomment#DefineType('texinfo', '@c %s')
call tcomment#DefineType('xdefaults', '! %s')
call tcomment#DefineType('xmodmap', '! %s')
call tcomment#DefineType('vifm', '" %s')
let g:tcommentMapLeader1 = ''
let g:tcommentMapLeader2 = ''
" Block comment (use gcb<motion> in normal mode):
vmap gb :TCommentBlock<CR>
call tcomment#DefineType('c_block', g:tcommentBlockC2)
call tcomment#DefineType('cpp_block', g:tcommentBlockC2)

" localvimrc:
let g:localvimrc_count = 1 " on the way from root, the last 1 file is sourced
" accept .lvimrc files in /home/volkov/* and /media/files/*:
let g:localvimrc_whitelist='^\(/home/volkov/*\|/media/files/*\)'
" ignore .lvimrc files on mounted filesystems:
let g:localvimrc_blacklist='^/media/*'
let g:localvimrc_ask = 1 " ask before loading a vimrc file (0 = don't ask)
let g:localvimrc_sandbox = 0 " don't use sandbox (1 = use)

" Code to convert spaces to \n and backwards (mlines, merge lines, slines, split lines):
function! SplitLines()
	let l:current_line_number = line('.')
	let l:text_below = getline(l:current_line_number + 1)
	let l:orig_text = getline('.')
	" Delete original text into register z:
	execute 'normal 0"zD'
	" Get list of words:
	let l:words_array = split(l:orig_text)
	if l:text_below != ''
		call add(l:words_array, '')
	endif
	" Append list of words to current buffer:
	let l:failed = append(l:current_line_number, l:words_array)
	let l:text_above = getline(line('.') - 1)
	echom l:text_above
	if l:text_above == '' " We've inserted unnecessary empty line. Need to delete it.
		execute 'normal "zdd'
	else " Need to move cursor to first word.
		execute 'normal j'
	endif
endfunction
command! SplitLines call SplitLines()
nmap <Leader>s0 :SplitLines<CR>

function! MergeLines(...) range
	execute a:firstline
	let l:last_line_in_buffer = line('$')
	let l:separator = (a:0 >= 1) ? a:1 : ' '
	let l:lines_array = getline(a:firstline, a:lastline)
	let l:orig_text = join(l:lines_array, l:separator)
	" Delete original text into register z:
	execute a:firstline ',' a:lastline 'delete z'
	let l:failed = append(a:firstline - 1, l:orig_text)
	if a:lastline == l:last_line_in_buffer
		execute 'normal j'
	else
		execute 'normal k'
	endif
endfunction
command! -nargs=? -range MergeLines <line1>,<line2> call MergeLines(<f-args>)
function! MergeBlock()
	execute 'normal {'
	if getline('.') == ''
		execute 'normal j'
	endif
	let l:first_line = line('.')
	execute 'normal }'
	if getline('.') == ''
		execute 'normal k'
	endif
	let l:last_line = line('.')
	execute l:first_line . ',' . l:last_line . 'call MergeLines()'
endfunction
nmap <Leader>m1 :call MergeBlock()<CR>

" function! Backspace_handler()
" 	let l:cursor = getpos('.')
" 	let l:column = l:cursor[2]
" 	if l:column < 4 " if string is not long enough
" 		" then execute simple backspace
" 		call feedkeys("", "insert")
" 		return
" 	endif
" 	let l:line = getline('.')
" 	let l:line_before_cursor = strpart(l:line, 0, l:column)
" 	" Check if we have non-empty symbols in string before cursor:
" 	let l:i = 0
" 	while l:i < l:column
" 		let l:symbol = strpart(l:line_before_cursor, l:i, 1)
" 		if l:symbol != "	" && l:symbol != " "
" 			" We have non-empty symbols: execute simple backspace.
" 			call feedkeys("", "insert")
" 			return
" 		endif
" 		let l:i += 1
" 	endwhile
" 	" Check if we have at least four contigious spaces in front of cursor:
" 	let l:four_symbols = strpart(l:line_before_cursor, l:column - 4)
" 	if l:four_symbols != "    "
" 		call feedkeys("", "insert")
" 		return
" 	endif
" 	" Delete 4 spaces at once:
" 	call feedkeys("", "insert")
" 	call feedkeys("", "insert")
" 	call feedkeys("", "insert")
" 	call feedkeys("", "insert")
" endfunction
" The above function fails to work with autoindent mode and 'o', 'O' normal
" mode hotkeys:
" First we use <Esc> to go to normal mode. In this case autoindent deletes
" indentation (the problem). Solution: need to disable standard
" autoindentation, and implement it by myself.
" autocmd FileType rust,c,cpp,sh,expect,cmake,vim,python,perl imap <Backspace> <Esc>:call Backspace_handler()<CR>gi

" " doxygen: function begin:
" nmap <Leader>fb i/**<Esc>o * @fn 
" " doxygen: function end:
" nmap <Leader>fe :call End_function()<CR>
" function! End_function()
" 	let l:fn_line_number = line('.')
" 	let l:fn_line = getline('.')
"
" 	let l:prototype = strpart(l:fn_line, 6) " extra space in the front present
" 	let l:brace_index = stridx(l:prototype, '(') " starts from 0
" 	let l:prototype_before_brace = strpart(l:prototype, 0, l:brace_index)
" 	let l:prototype_after_brace = substitute(strpart(l:prototype, l:brace_index + 1), ')', ', ', "")
"
" 	" Insert function prototype:
" 	call append(l:fn_line_number, '}')
" 	call append(l:fn_line_number, '{')
" 	call append(l:fn_line_number, strpart(l:prototype, 1))
" 	call append(l:fn_line_number, ' */')
"
" 	" Insert @return if needed:
" 	let l:void_index = stridx(l:prototype_before_brace, ' void ')
" 	if l:void_index == -1
" 		call append(l:fn_line_number, ' * @return ')
" 	endif
"
" 	" Insert @param:
" 	let l:param_words = split(l:prototype_after_brace, ' \zs')
" 	let l:param_names = []
" 	for word in l:param_words
" 		let l:word_len = strlen(l:word)
" 		let l:symbol = strpart(l:word, l:word_len - 2, 1)
" 		if l:symbol == ','
" 			let l:param_name = strpart(l:word, 0, l:word_len - 2)
" 			call add(l:param_names, l:param_name)
" 		endif
" 	endfor
" 	call reverse(l:param_names)
" 	for param in l:param_names
" 		call append(l:fn_line_number, ' * @param ' . l:param . ' ')
" 	endfor
"
" 	" Insert @brief:
" 	call append(l:fn_line_number, ' * @brief ')
" 	" Set cursor to proper position and go to insert mode:
" 	execute 'normal! j$'
" 	startinsert!
" endfunction

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
" command! -range MyIndent <line1>,<line2> call MyCIndent()

" command! -nargs=1 CmdName call Func_name(<f-args>)
" command! -nargs=0 CmdName call Func_name('arg1')

" if has('nvim')
" 	" First invoke terminal:
" 	nmap <Leader>tt :silent w<CR>:e term://bash<CR>:startinsert<CR>
" 	" Use <C-Space> in terminal to switch to normal mode:
" 	tnoremap <C-@> <C-\><C-n>:set relativenumber<CR>
" 	" In terminal switch back to insert mode:
" 	nmap <C-@> :set norelativenumber<CR>:startinsert<CR>
" 	" Copy modified file name from git status:
" 	function! Git_copy_modified_file_name(file_number)
" 		let l:line_1 = search('        modified:   ', 'b')
" 		if l:line_1 == 0 " not found
" 			echo 'Error: modified by git files not found'
" 			return
" 		endif
" 		let @+ = strpart(getline(l:line_1 - a:file_number + 1), 20)
" 		set norelativenumber
" 		startinsert
" 	endfunction
" 	command! -nargs=1 GitCopy call Git_copy_modified_file_name(<f-args>)
" 	" nmap <Leader>gg :call Git_copy_modified_file_name(1)<CR>
" 	" nmap <Leader>ga :GitCopy 
" endif

" function! My_insert_numbers(first_number)
" 	let l:first_num = str2nr(a:first_number)
" 	let l:num_digits = strlen(a:first_number)
" 	let l:format = printf("%%0%dd_%%s", l:num_digits)
" 	let l:num_lines = line('$')
" 	let l:cur_line_num = 1
" 	while l:cur_line_num <= l:num_lines
" 		let l:cur_line_contents = getline(l:cur_line_num)
" 		let l:cur_num = l:first_num + l:cur_line_num - 1
" 		let l:modified_line_contents = printf(l:format, l:cur_num, l:cur_line_contents)
" 		call setline(l:cur_line_num, l:modified_line_contents)
" 		let l:cur_line_num = l:cur_line_num + 1
" 	endwhile
" endfunction
" command! -nargs=1 InsertNumbers call My_insert_numbers(<f-args>)
" " Prepend numbers like "1_, 2_, ..." to all lines in file:
" nmap <Leader>dn :InsertNumbers 

function! GetFileNameUnderCursor(line)
	let l:last_file_char = stridx(a:line, " ", 1)
	if l:last_file_char == -1
		let l:last_file_char = stridx(a:line, "\t", 1)
		if l:last_file_char == -1
			let l:last_file_char = stridx(a:line, ",", 1)
		endif
	endif
	if l:last_file_char != -1
		return strpart(a:line, 0, l:last_file_char)
	endif
	return a:line
endfunction

function! OpenFile(win, cmd) " at least one colon expected
	let l:line = strpart(getline('.'), col(".") - 1)
	let l:file = GetFileNameUnderCursor(l:line)
	if a:win == 'prev_win'
		execute "normal \<c-w>\<c-p>"
	endif
	execute a:cmd . ' ' . l:file
endfunction
nmap <Leader>ft :call OpenFile('cur_win', 'tabedit')<CR>
nmap <Leader>fj :call OpenFile('cur_win', 'e')<CR>
nmap <Leader>fv :call OpenFile('cur_win', 'vsp')<CR>
nmap <Leader>fh :call OpenFile('cur_win', 'sp')<CR>
nmap <Leader>fpj :call OpenFile('prev_win', 'e')<CR>
nmap <Leader>fpv :call OpenFile('prev_win', 'vsp')<CR>
nmap <Leader>fph :call OpenFile('prev_win', 'sp')<CR>

" Fugitive (git):
" :copen - open quickfix window
nmap <Leader>gs :Gstatus<CR>
" In status window use:
" g?    show help
" "-" to "git add" or "git reset" file (depending on where your cursor is).
" <c-n> to go to next file, <c-p> to go to previous file.
" <Enter> to open current file in window below.
" ca    :Gcommit --amend
nmap <Leader>gb :Gblame<CR>
" Diff against index:
nmap <Leader>gdi :Gdiff<CR>
" Diff against HEAD:
nmap <Leader>gdh :Gdiff HEAD<CR>
" Diff against HEAD~1:
nmap <Leader>gd1 :Gdiff ~1<CR>
" Diff next file:
nmap <Leader>gdn :q<CR>:q<CR>:exec ':Gdiff ' . g:Gdiff_arg<CR>
" Diff against previous commit:
nmap <Leader>gdp :Gdiff ~1<CR>
nmap <Leader>gw :Gwrite<CR><C-w>k
nmap <Leader>gr :Gread<CR>:w<CR><C-w>k<C-n>
nmap <Leader>gco :Gcommit<CR>
nmap <Leader>gca :Gcommit --amend<CR>
nmap <Leader>ga <C-w>k-
nmap <Leader>ge :!git show --name-only --pretty="" HEAD<CR>
" Need for "fugitive-:Gdiff":
set diffopt+=vertical
" Search for next unstaged file (TODO):
" nmap <C-j>/Changes not staged for commit:<CR>

" Search using ag (silverlight searcher):
nmap <Leader>ags :Ags 
" Kill running ag instance:
nmap <Leader>agk :!~/os_settings/other_files/kill_process_by_unique_name.sh ag<CR><CR>
" let g:ags_agmaxcount = 2000
" In search results window press "u" for usage (mappings) information.
let g:ags_agargs = {
                \ '--break'             : [ '', '' ],
                \ '--color'             : [ '', '' ],
                \ '--color-line-number' : [ '"1;30"', '' ],
                \ '--color-match'       : [ '"32;40"', '' ],
                \ '--color-path'        : [ '"1;31"', '' ],
                \ '--column'            : [ '', '' ],
                \ '--context'           : [ 'g:ags_agcontext', '-C' ],
                \ '--group'             : [ '', '' ],
                \ '--heading'           : [ '', '-H' ],
                \ '--max-count'         : [ 'g:ags_agmaxcount', '-m' ],
                \ }
" Deleted unsupported options:
                " \ '--filename'          : [ '', '' ],
                " \ '--numbers'           : [ '', '' ]

function! My_eval_replace()
	let l:cursor = getpos('.')
	let l:column = l:cursor[2]
	let l:line_before = getline('.')
	let l:evaluated = eval(strpart(l:line_before, 0, l:column))
	let l:result = l:evaluated . strpart(l:line_before, l:column)
	call append(line('.'), l:result)
	normal dd
	call setpos('.', [l:cursor[0], l:cursor[1], strlen(evaluated), l:cursor[3]])
endfunction
nmap <Leader>e :call My_eval_replace()<CR>

" To insert echo (for Makefile) use the following macro:
let @e = 'i	@echo "|$()|"hhi'

" Macros:
" You can use <C-o>q to finish recording while in insert mode.
" <C-o> in insert mode allows you to execute one command in
" normal mode, and then returns to insert mode (see :help i^O).

