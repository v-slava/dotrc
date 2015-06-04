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
" Make windows equal (vsplit): <C-w>=
"
" reload file: :edit
"
" Add (framework/standard C/C++ library) tags: <F8> (see ~/.vim/tags)
" Index source files and update cscope connection: <F9>. Command usage: :SrcIndexOn PRJ_ROOT_PATH
"
" view vim filetypes:
" ls /usr/share/vim/vim74/ftplugin/
" ls /usr/share/vim/vim74/syntax/

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

" Auto insert <EOL> and move last word to next line if it reaches 81 column
" set wrapmargin=80

set hlsearch
set ignorecase
set incsearch

set autoindent
" set cindent " C-style indents (after '{' and so on)
set number " display line numbers
" set nowrap " do not wrap long lines

" View invisible characters for makefiles:
" autocmd FileType make set list
nmap <F2> :set list!<CR>

" Use F3 to set/unset search highlighting:
nmap <F3> :set hlsearch!<CR>

imap оо <shortinfo></shortinfo><Esc>F<i

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

" Map clipboard to unnamedplus register '+':
set clipboard=unnamedplus
" Preserve copied text in clipboard on exit:
autocmd VimLeave * call system("clipboard.sh", getreg('+'))

" Set correct filetypes:
autocmd BufEnter vifmrc set filetype=vim
autocmd BufEnter *.i set filetype=c
autocmd BufEnter *.ii set filetype=cpp
autocmd BufEnter *.gdb set filetype=gdb " my filetype extension
autocmd BufEnter *.cmm set filetype=jtag_script " my filetype
autocmd BufEnter menurc set filetype=claws_mail_menurc " my filetype
autocmd BufEnter * if &filetype == "" | setlocal filetype=unknown | endif

" Set correct syntax:
autocmd FileType asm set syntax=armasm

" Set tab width:
autocmd FileType c,cpp,sh,expect set tabstop=4
autocmd FileType c,cpp,sh,expect set shiftwidth=4

" Move current tab left and right:
nnoremap <silent> <S-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <S-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Open tags file in $(dirname EDITED_FILE), if not present then in $(dirname EDITED_FILE)/.. and until /
set tags=./tags;,tags;

" Add (framework/standard C/C++ library) tags:
nmap <F8> :set tags+=~/.vim/tags/std.ctags<CR> :cs add ~/.vim/tags/std.cscope<CR>

" Index source files and update cscope connection. Command usage: :SrcIndexOn PRJ_ROOT_PATH
nmap <F9> :SrcIndexOn 

function! MyQuickRun(in_file)
	let cmd = "~/os_settings/other_files/quick_run.sh " . a:in_file
 	let log = '/tmp/vim_ide_' . a:in_file . '_log'
	let ignored = system(cmd)
	execute "botright pedit " . log
endfunction
nmap <F5> :w<CR>:call MyQuickRun( @% )<CR>

function! CopyLocation(in_file, strip_part)
	let line_number = line('.')
	let full_location = a:in_file . ':' . line_number
	let strip_width = strlen(a:strip_part)
	if strip_width
		let @+ = strpart(full_location, strip_width)
	else
		let @+ = full_location
	endif
endfunction
nmap <F12> :call CopyLocation( expand('%:p'), "" )<CR>
nmap <F11> :call CopyLocation( expand('%:p'), "/home/volkov/workspace/uhd_image/p4_workspace/" )<CR>
nmap <F10> :call CopyLocation( expand('%:p'), "/home/volkov/workspace/15_UHD_BD/modified_files/" )<CR>


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
call tcomment#DefineType('jtag_script', '; %s')
call tcomment#DefineType('claws_mail_menurc', '; %s')
call tcomment#DefineType('inittab', '# %s')
call tcomment#DefineType('mplayerconf', '# %s')
call tcomment#DefineType('asm', '/* %s */')
call tcomment#DefineType('texinfo', '@c %s')
call tcomment#DefineType('xdefaults', '! %s')
call tcomment#DefineType('fusesmbconf', '; %s')

" Code to convert spaces to \n and backwards:
function! MySplitLines() range
	let first_line = a:firstline
	let last_line = a:lastline
	let lines_array = getline(first_line, last_line)
	let orig_text = join(lines_array, "\n")
	" Delete original text into register z:
	execute first_line "," last_line 'delete z'
	let words_array = split(orig_text)
	let failed = append(first_line - 1, words_array)
endfunction
command! -range -nargs=* Slines <line1>,<line2> call MySplitLines()
function! MyMergeLines() range
	let first_line = a:firstline
	let last_line = a:lastline
	let lines_array = getline(first_line, last_line)
	let orig_text = join(lines_array, " ")
	" Delete original text into register z:
	execute first_line "," last_line 'delete z'
	let failed = append(first_line - 1, orig_text)
endfunction
command! -range -nargs=* Mlines <line1>,<line2> call MyMergeLines()

" Macros:
" You can use <C-o>q to finish recording while in insert mode.
" <C-o> in insert mode allows you to execute one command in
" normal mode, and then returns to insert mode (see :help i^O).

" Insert <shortinfo> tags (for glanguage):
" let @s = 'A<shortinfo></shortinfo>F<i'

