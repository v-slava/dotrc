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
" <C-B> - page up
" <C-F> - page down
" <C-U> - half-page up
" <C-D> - half-page down
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

set nocompatible
set path+=**
set wildmenu

" Fix colorscheme:
hi Search ctermfg=0 ctermbg=12
hi DiffAdd ctermbg=234
hi DiffDelete ctermbg=16
hi StatusLine ctermfg=232 ctermbg=46
hi StatusLineNC ctermfg=232 ctermbg=252

" Highlight spaces and tabs in the end of the line as errors (trailing whitespaces):
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
" inoremap fd <esc>
" Map <C-Space> to <esc>:
inoremap <nul> <esc>
vnoremap <nul> <esc>
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
" Set/unset search highlighting:
" nmap <F3> :set hlsearch!<CR>

" Search for text in clipboard:
nmap <Leader>s/ :let @/ = @+<CR>

" Scroll horizontally:
nmap <C-l> zl
nmap <C-h> zh

" Exit from vim:
nmap <C-d> :q<CR>
nmap <Leader>qm :q<CR>

" View next/previous buffer:
nmap <Leader>bn :bnext<CR>
nmap <Leader>bp :bprevious<CR>

" Split vim window:
nmap <Leader>ws :split<CR>
nmap <Leader>wv :vsplit<CR>

" Switch focus to vim window <Leader>w{h|l|j|k}:
nmap <Leader>w <C-w>
" <Leader>w=           - make split windows equal (diff)
" Focus previous window:
nmap <Leader>wp <C-w><C-p>

nmap <Leader>ss /

" Clear current line:
nmap <Leader>oc $d0x

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
nmap <Leader>jj <Plug>(easymotion-s)
vmap <Leader>jj <Plug>(easymotion-s)

nmap <Leader>jw <Plug>(easymotion-bd-W)
vmap <Leader>jw <Plug>(easymotion-bd-W)

" Apply macro to selected lines:
vmap i :normal @

" Do not capture stderr while using :read
" set shellredir=>%s 2>&1 " - default value
set shellredir=>%s

" Reformat C/C++ source code:
" nmap <C-u> :%d<CR>:r !uncrustify -f %<CR>:1,1d<CR>
" nmap <C-u> :%d<CR>:r !astyle.sh %<CR>

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
autocmd FileType rust,c,cpp,sh,expect,cmake,vim,python,perl,lua setlocal tabstop=4 | setlocal expandtab | setlocal shiftwidth=0
" autocmd BufNewFile,BufRead,BufEnter */dotrc/* setlocal expandtab

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
nmap <Leader>ln :lne<CR>
nmap <Leader>lp :lp<CR>

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
nmap <C-;> :call Swap_keyboard_layout()<CR>
vmap <C-;> <Esc>:call Swap_keyboard_layout()<CR>gv
imap <C-;> <Esc>:call Swap_keyboard_layout()<CR>gi

function! Add_include_guards(file_name)
	let l:guard_name = tr(toupper(a:file_name), '.', '_')
	call append(line('0'), ['#ifndef ' . l:guard_name, '#define ' . l:guard_name])
	call append(line('$'), ['', '#endif // #ifndef ' . l:guard_name])
endfunction
nmap <Leader>di :call Add_include_guards(expand('%:t'))<CR>

" Move current tab left and right (move tab):
nmap <silent> <S-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nmap <silent> <S-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

function Window_is_temporary()
	let l:dir_name = expand('%:p:h')
	let l:file_name = expand('%:t')
	if l:dir_name == '/usr/share/vim/vim74/doc' || &l:buftype == 'help' || &l:buftype == 'quickfix' || l:file_name == 'search-results.agsv'
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

function! Copy_location(in_file, strip_part)
	let l:line_number = line('.')
	let l:full_location = a:in_file . ':' . l:line_number
	let l:strip_width = strlen(a:strip_part)
	if strip_width
		let @+ = strpart(l:full_location, l:strip_width)
	else
		let @+ = l:full_location
	endif
	echo 'copied: ' . @+
endfunction
" Copy full source location into clipboard:
nmap <Leader>cl :call Copy_location( expand('%:p'), '' )<CR>
" nmap <F11> :call Copy_location( expand('%:p'), '/home/slava/workspace/project_root_dir/' )<CR>

" Initialize pathogen plugin (update runtimepath variable):
execute pathogen#infect()
colorscheme molokai

" Tcomment:
call tcomment#type#Define('unknown', '# %s')
call tcomment#type#Define('lisp', ';; %s')
call tcomment#type#Define('make', '# %s')
call tcomment#type#Define('gdb', '# %s')
call tcomment#type#Define('kconfig', '# %s')
call tcomment#type#Define('sudoers', '# %s')
call tcomment#type#Define('inittab', '# %s')
call tcomment#type#Define('mplayerconf', '# %s')
call tcomment#type#Define('text', '# %s')
call tcomment#type#Define('toml', '# %s')
call tcomment#type#Define('ninja', '# %s')
call tcomment#type#Define('aptconf', '// %s')
call tcomment#type#Define('jtag_script', '; %s')
call tcomment#type#Define('claws_mail_menurc', '; %s')
call tcomment#type#Define('fusesmbconf', '; %s')
call tcomment#type#Define('asm', '/* %s */')
call tcomment#type#Define('texinfo', '@c %s')
call tcomment#type#Define('xdefaults', '! %s')
call tcomment#type#Define('xmodmap', '! %s')
call tcomment#type#Define('vifm', '" %s')
let g:tcomment_mapleader1 = ''
let g:tcomment_mapleader2 = ''
" Block comment (use gcb<motion> in normal mode):
vmap gb :TCommentBlock<CR>
" call tcomment#type#Define('c_block', g:tcommentBlockC2)
" call tcomment#type#Define('cpp_block', g:tcommentBlockC2)

" To split lines: :%s/ /<ctrl-v><ENTER>/g"

" if has('nvim')
" 	" First invoke terminal:
" 	nmap <Leader>tt :silent w<CR>:e term://bash<CR>:startinsert<CR>
" 	" Use <C-Space> in terminal to switch to normal mode:
" 	tnoremap <C-@> <C-\><C-n>:set relativenumber<CR>
" 	" In terminal switch back to insert mode:
" 	nmap <C-@> :set norelativenumber<CR>:startinsert<CR>
" 	" Window management with terminal:
" 	tnoremap <C-w><C-w> <C-\><C-n><C-w><C-w>
" 	autocmd TermOpen * startinsert
" 	autocmd WinEnter term://* startinsert
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

" autocmd TermClose * bd! " do not show 'Process exited ' message
" sp | wincmd j | e term://f s | set norelativenumber | set nonumber | startinsert

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
" nmap <Leader>gw :Gwrite<CR><C-w>k
" nmap <Leader>gr :Gread<CR>:w<CR><C-w>k<C-n>
nmap <Leader>gco :Gcommit<CR>
nmap <Leader>gca :Gcommit --amend<CR>
" nmap <Leader>ga <C-w>k-
nmap <Leader>ge :!git show --name-only --pretty="" HEAD<CR>
" Need for "fugitive-:Gdiff":
set diffopt+=vertical

function! My_get_eval_first_last_lines()
    let l:cursor = getpos('.')
    try
        normal ?EVAL REGION BEGINS HERE:j
        let l:first_line = getpos('.')[1]
    catch
        call setpos('.', l:cursor)
        throw "can't find 'EVAL REGION BEGINS HERE:'"
    endtry

    try
        normal /EVAL REGION ENDS HERE.k
        let l:last_line = getpos('.')[1]
    catch
        call setpos('.', l:cursor)
        throw "can't find 'EVAL REGION ENDS HERE.'"
    endtry

    call setpos('.', l:cursor)

    let l:cur_line = l:cursor[1]
    if l:cur_line < l:first_line - 1 || l:cur_line > l:last_line + 1
        throw "cursor is not inside EVAL REGION"
    endif

    echo
    return [l:first_line, l:last_line]
endfunction

function! My_get_text(first_line, last_line)
    let l:lines_list = getbufline('%', a:first_line, a:last_line)
    return join(l:lines_list, "\n")
endfunction

function! My_eval_vim()
    try
        let l:lines_range = My_get_eval_first_last_lines()
        let l:text = My_get_text(l:lines_range[0], l:lines_range[1])
        " echo l:text
    catch
        echo v:exception
        return
    endtry
    execute l:text
endfunction
nmap <Leader>mel :call My_eval_vim()<CR>

" To insert echo (for Makefile) use the following macro:
let @E = 'i	@echo "|$()|"hhi'
let @e = 'oEVAL REGION BEGINS HERE:EVAL REGION ENDS HERE.gcko'

" Macros:
" You can use <C-o>q to finish recording while in insert mode.
" <C-o> in insert mode allows you to execute one command in
" normal mode, and then returns to insert mode (see :help i^O).

