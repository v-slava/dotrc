" Writing python plugins:
" https://neovim.io/doc/user/remote_plugin.html
" https://pynvim.readthedocs.io/en/latest/usage/remote-plugins.html
" https://github.com/neovim/pynvim/blob/master/docs/usage/python-plugin-api.rst
"
" " View man page in vim:
" :Man 2 read

" Macros:
" You can use <C-o>q to finish recording while in insert mode.
" <C-o> in insert mode allows you to execute one command in
" normal mode, and then returns to insert mode (see :help i^O).
" Insert (save, store, append) vimscript macro "a":
" call append('.', @a)
" To insert echo (for Makefile) use the following macro:
" let @E = 'i	@echo "|$()|"hhi'

" Resize vim window:
" :[vertical] resize {+|-}<NUMBER><CR>
"
" List all sourced script names:
" :scriptnames
"
" :help string-functions
"
" To split lines: :%s/ /<ctrl-v><ENTER>/g"

" Open quickfix window:
" :copen
"
" Open location list window:
" :lopen
"
" To reindent json: 1) Make a selection. 2) :!python -m json.tool
"
" To repeat last colon command (in normal mode): @:, @@, @@, @@, ...
" Toggle language: <C-k>
"
" To reformat text to fit max 80 columns: select text, and type 'gq'.
" In normal mode: 'gq' + motion
"
" vimdiff mode hotkeys:
" ]c               - advance to the next block with differences
" [c               - reverse search for the previous block with differences
" do (diff obtain) - bring changes from the other file to the current file
" dp (diff put)    - send changes from the current file to the other file
" zo               - unfold/unhide text
" zc               - refold/rehide text
" zr               - unfold both files completely
"
" <C-G> - print current cursor position (percentage), etc...
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
" Use spelling:
" See spell files in /usr/share/vim/vim81/spell.
" http://ftp.vim.org/pub/vim/runtime/spell/{ru|en|de}.utf-8.spl
" set spell spelllang=en,de,ru,en_us
"
" view vim filetypes:
" ls /usr/share/vim/vim81/ftplugin/
" ls /usr/share/vim/vim81/syntax/

" Will be written on windows by apply_homme_settings.bat:
" let g:My_win_dotrc = 'D:\root_folder\dotrc'

" Disable some unused standard plugins:
let g:loaded_getscriptPlugin = 1
let g:loaded_gzip = 1
let g:loaded_2html_plugin = 1
let g:loaded_netrwPlugin = 1
let g:loaded_rrhelper = 1
let g:loaded_tarPlugin = 1
let g:loaded_zipPlugin = 1
let g:loaded_vimballPlugin = 1

 " disable default mappings:
let g:EasyMotion_do_mapping = 0
let g:rtagsUseDefaultMappings = 0
" let g:swoopUseDefaultKeyMap = 0 " we use denite instead

let g:rtagsUseLocationList = 0

let g:My_win_cmd_exe = 0
let g:My_tmp_dir = "/tmp"
if $WINDIR != ""
    let g:My_is_windows = 1
    let g:My_user_id = 0
    if &shell == 'C:\windows\system32\cmd.exe'
        let g:My_win_cmd_exe = 1
        let g:My_tmp_dir = system("echo %TEMP%")[:-3]
    endif
else
    let g:My_is_windows = 0
    let g:My_user_id = system('id -u')
endif

if !g:My_is_windows
    " Initialize pathogen plugin (update runtimepath variable):
    execute pathogen#infect()
endif

if globpath(&runtimepath, "autoload/denite.vim") != ""
    " Use ripgrep:
    call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob',
                \ '!.git'])
    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep',
                \ '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
    function! My_source_vim_script(context)
        let l:file = a:context['targets'][0].action__path
        " echo 'to be sourced: |' . l:file . '|'
        execute 'source ' . l:file
    endfunction
    call denite#custom#action('file', 'source_vim_script',
                \ function('My_source_vim_script'))
endif

autocmd FileType denite call My_denite_settings()
function! My_denite_settings() abort
    nnoremap <silent><buffer><expr> o denite#do_map('do_action', 'open')
    nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
    nnoremap <silent><buffer><expr> q denite#do_map('quit')
    nnoremap <silent><buffer><expr> s denite#do_map('do_action',
                \ 'source_vim_script')
    nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
    " nnoremap <silent><buffer><expr> t denite#do_map('toggle_select')
    " nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
    " this is default action ('open' in most cases, see :help denite-kinds):
    nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
    " only for "buffer" kind:
    nnoremap <silent><buffer><expr> d denite#do_map('do_action', 'delete')
endfunction

function! My_select_project()
    let l:prj_dir = '$DOTRC_S/other_files/vim_projects'
    let l:prj_files_list = split(globpath(l:prj_dir, '*'), '\n')
    let l:num_files = len(l:prj_files_list)
    if l:num_files == 0
        echo 'No projects found in ' . l:prj_dir
        return
    elseif l:num_files == 1
        let l:prj_file_name = fnamemodify(l:prj_files_list[0], ":t")
        execute 'source ' . l:prj_files_list[0]
        echo 'Selected project: ' . l:prj_file_name
    else
        execute 'Denite -default-action=source_vim_script -start-filter -path='
                    \ . l:prj_dir . ' file/rec'
    endif
endfunction
" default action for "output" kind is to copy selected item to clipboard:
" :Denite output:!echo:-e:'hello\\nWorld'
" :Denite output:echo:\'hello:world\'
" :Denite output:echo:\\"hello\\nworld\\"
" Change default action:
" call denite#custom#kind('file', 'default_action', 'source_vim_script')

" let g:menus = {}
" let g:menus.zsh = {
"     \ 'description': 'Edit your import zsh configuration'
"     \ }
" let g:menus.zsh.file_candidates = [
"     \ ['zshrc', '~/.config/zsh/.zshrc'],
"     \ ['zshenv', '~/.zshenv'],
"     \ ]
" let g:menus.my_commands = {
"     \ 'description': 'Example commands'
"     \ }
" let g:menus.my_commands.command_candidates = [
"     \ ['Split the window', 'vnew'],
"     \ ['Open zsh menu', 'Denite menu:zsh'],
"     \ ['Format code', 'FormatCode', 'go,python'],
"     \ ]
" call denite#custom#var('menu', 'menus', g:menus)
" Denite menu

if g:My_is_windows
    set noswapfile
    set expandtab
    set tabstop=4
    set shiftwidth=4
    " set shell=\"C:/Program\ Files/Git/bin/bash.exe\"
    " set shell=\"C:/windows/system32/cmd.exe\"
    " set shell="C:\\windows\\system32\\cmd.exe"
    if has("gui_running")
        set clipboard=unnamed " map clipboard to unnamed register '*'
        set encoding=utf-8
        set guifont=Consolas:h14
        " colorscheme evening
        " colorscheme desert
        colorscheme slate
        " Fix colorscheme:
        function! My_fix_colorscheme()
            " gui=bold
            hi CursorLine guibg=grey10
            hi MySuccessMsg guifg=seagreen
            hi MyErrorMsg guifg=#ff6060
        endfunction
        autocmd BufEnter * call My_fix_colorscheme()

        set lines=35
        set columns=90
        " start in fullscreen mode:
        " autocmd GUIEnter * simalt ~x
    endif
    if g:My_win_cmd_exe
        " let &runtimepath.=','.system("echo %homedrive%%homepath%\\.vim")[:-3]
        let &runtimepath.=',~\.vim'
    endif
else
    " let g:molokai_original = 1
    let g:rehash256 = 1
    colorscheme molokai
    " Fix colorscheme:
    function! My_fix_colorscheme()
        hi Search ctermfg=0 ctermbg=12
        hi IncSearch ctermfg=0 ctermbg=12
        hi DiffAdd ctermbg=234
        hi DiffDelete ctermbg=16
        " hi StatusLine ctermfg=232 ctermbg=46
        " hi StatusLineNC ctermfg=232 ctermbg=252
        " commented-out because it highlights spaces in
        " "which-key" (leader) window:
        " autocmd WinEnter * match Error /\s\+$/
        hi MySuccessMsg ctermfg=118
        hi MyErrorMsg ctermfg=161 cterm=bold
        if g:rehash256
            hi Error ctermfg=219 ctermbg=124
        endif
        hi MatchParen ctermfg=none ctermbg=59 " ctermbg=20
    endfunction
    autocmd BufEnter * call My_fix_colorscheme()
    set clipboard=unnamedplus " map clipboard to unnamedplus register '+'
    " Preserve copied text in clipboard on exit:
    autocmd VimLeave * call system("clipboard.sh", getreg('+'))
endif

set colorcolumn=81
" Highlight spaces and tabs in the end of the line (trailing whitespaces):
match Error /\s\+$/

syntax on
set nocompatible " do not try to be Vi-compatible
" set path+=** " gf - edit the file whose name is under or after the cursor.
set wildmenu " enhanced command line completion mode
set cursorline " highlight current line
set hlsearch
set ignorecase
set smartcase
set incsearch
set mouse=a
set autoindent
" set cindent " C-style indents (after '{' and so on)
" set smartindent
set number " display line numbers
set relativenumber
" set nowrap " do not wrap long lines
set linebreak
set backspace=2 " fix backspace befavior
map Q <nop> " disable ex mode
map q: <nop> " disable commands history mode
" let mapleader = "\\"
let mapleader = "\<Space>"
set diffopt+=vertical " need for "fugitive-:Gdiff"
" set lines=25
" set columns=83
" set langmenu=ru_RU.UTF-8

" Set maximum number of tab pages to be opened with "-p" command line argument:
set tabpagemax=100

" Display symbol '»' as a first symbol for tab:
set listchars=tab:»\ 
set list

" Use fd instead of <esc> to exit from insert mode:
" inoremap fd <esc>
" Map <C-Space> to <esc>:
inoremap <nul> <esc>
vnoremap <nul> <esc>
" imap <esc> <nop>

" Use <C-j> as <Backspace> in insert mode:
" inoremap <C-j> <Backspace>
" imap <Backspace> <nop>

" Swap ':' and ';' in normal mode:
" nnoremap ; :
" nnoremap : ;

" Swap ',' and '<' in normal mode:
" nnoremap , <
" nnoremap < ,

" Do not capture stderr while using :read
" set shellredir=>%s 2>&1 " - default value
set shellredir=>%s

filetype plugin on
" Set correct filetypes:
" autocmd BufRead,BufNewFile
autocmd BufEnter $DOTRC_S/other_files/vim_projects/* setlocal filetype=vim
autocmd BufEnter vifmrc setlocal filetype=vifm
autocmd BufEnter *vifm/colors/* setlocal filetype=vifm
autocmd BufEnter *.i setlocal filetype=c
autocmd BufEnter *.ii setlocal filetype=cpp
autocmd BufEnter *.lds,*.lds.S,*.lds.h setlocal filetype=ld
autocmd BufEnter .spacemacs setlocal filetype=lisp
autocmd BufEnter *.gdb setlocal filetype=gdb " my filetype extension
autocmd BufEnter *.cmm setlocal filetype=jtag_script " my filetype
autocmd BufEnter *.gv setlocal filetype=dot
autocmd BufEnter *.ds setlocal filetype=arm_ds_5_script " my filetype
" note: originally for *.ds filetype was "datascript".
autocmd BufEnter menurc setlocal filetype=claws_mail_menurc " my filetype
" autocmd BufEnter *.gl setlocal filetype=glanguage " my filetype
autocmd BufEnter *.mbsyncrc,*.msmtprc setlocal filetype=conf
autocmd BufEnter *kitty/*.conf setlocal filetype=conf
autocmd BufEnter *.muttrc setlocal filetype=muttrc
autocmd BufEnter foot.ini setlocal filetype=foot_ini | setlocal syntax=dosini

autocmd BufEnter * if &filetype == "" | setlocal filetype=unknown | endif
            \ | call My_configure_tcomment()

" Set correct syntax:
autocmd FileType asm setlocal syntax=armasm
autocmd FileType arm_ds_5_script setlocal syntax=gdb

" Tab settings:
function! My_apply_tab_settings_s()
    " redefine this function in $DOTRC_S/home_settings/.vimrc
endfunction
let g:detectindent_preferred_expandtab = 1
let g:detectindent_preferred_indent = 4
let g:detectindent_preferred_when_mixed = 1
let g:detectindent_max_lines_to_analyse = 1024
autocmd FileType
\ c,cpp,rust,java,sh,expect,cmake,vim,python,perl,lua,php,json,dot,html,css,vb
\,kconfig
\ call My_apply_tab_settings()
function! My_apply_tab_settings()
    if g:My_user_id != 0 && !g:My_is_windows
        DetectIndent
    endif
    if expand("%:e") == "rs"
        " For some reason DetectIndent sets tabstop and shiftwidth to 1.
        setlocal tabstop=4
        setlocal shiftwidth=4
    endif
    if My_is_linux_kernel_source_file(expand("%:p"))
        if &expandtab == 0
            setlocal shiftwidth=8
        endif
    endif
    if &filetype == "vim"
        setlocal shiftwidth=4
    endif
    if &filetype == "text"
        setlocal epandtab
    endif
    call My_apply_tab_settings_s()
    execute 'setlocal tabstop=' . &shiftwidth
endfunction

" For some reason which-key doesn't work with perl files. Bug?
autocmd FileType perl call My_register_which_key(g:which_key_map, '')

" Auto insert <EOL> and move last word to next line if it reaches 81 column
autocmd FileType c,cpp,rust,java,sh,expect,cmake,vim,python,perl,lua,php
            \ setlocal formatoptions+=t
" setlocal cindent | setlocal noautoindent | setlocal expandtab
" setlocal textwidth=80

autocmd FileType unknown if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "echo 'using dummy g:My_eval_var value'" | endif

autocmd FileType vim if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "execute 'call ' . My_vimscript_function_eval() . '()'" | endif

if g:My_win_cmd_exe
    autocmd FileType dosbatch if ! exists('g:My_eval_var') |
        \ let g:My_eval_var = "silent MyRunShellCmd call "
        \ . expand("%:p") | endif

    autocmd FileType python if ! exists('g:My_eval_var')
        \ | let g:My_eval_var = "MyRunShellCmd python3 "
        \ . expand("%:p") | endif
else
    autocmd FileType sh,python,perl if ! exists('g:My_eval_var')
        \ | let g:My_eval_var = "MyRunShellCmd chmod +x ./"
        \ . expand("%:t") . " && ./" . expand("%:t") | endif
endif

autocmd FileType make if ! exists('g:My_eval_var')
    \ | let g:My_eval_var = "MyRunShellCmd make -f ./"
    \ . expand("%:t") | endif

autocmd FileType c if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "MyRunShellCmd clang -g3 -Weverything -pedantic "
    \ . expand("%:t") . " -o /tmp/" . expand("%:t") . ".out && /tmp/"
    \ . expand("%:t") . ".out" | endif

autocmd FileType cpp if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "MyRunShellCmd clang++ -g3 -Weverything -pedantic "
    \ . expand("%:t") . " -o /tmp/" . expand("%:t") . ".out && /tmp/"
    \ . expand("%:t") . ".out" | endif

autocmd FileType rust call My_rust_lang()
autocmd BufEnter Cargo.toml,Cargo.lock call My_rust_lang()

autocmd FileType java if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "MyRunShellCmd javac -d /tmp " . expand("%:t")
    \ . " && java -cp /tmp " . expand("%:t:r") | endif

autocmd FileType dot if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "silent wa | silent !if pgrep -x xdot > /dev/null ; then true ; "
    \ . "else xdot " . expand("%:p") . " & fi" | endif
    " "silent wa | silent !gcc -E -P -C -x c-header -o /tmp/" . expand("%:t")
    " "silent wa | silent !gpp.sh -o /tmp/" . expand("%:t")

autocmd FileType markdown if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "silent wa | silent !markdown " . expand("%:p") . " > /tmp/markdown.html"
    \ . " && if pgrep -x chrome > /dev/null ; then true ; "
    \ . "else google-chrome /tmp/markdown.html & fi &&"
    \ . "$DOTRC/other_files/update_page_in_web_browser.sh" | endif

autocmd FileType html if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "silent MyRunShellCmdNoOpen "
    \ . "$DOTRC/other_files/update_page_in_web_browser.sh"

autocmd FileType sql if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "silent MyRunShellCmd sqlite3 < " . expand("%:p") | endif

function! My_set_cargo_cmd(cmd)
    let g:My_eval_var = g:My_cargo_prefix . a:cmd
endfunction

function! My_rust_lang()
    set errorformat=\ \ -->\ %f:%l:%c
    " The following errorformat is stolen from:
    " https://github.com/rust-lang/rust.vim/blob/master/compiler/rustc.vim
    " For this case my error navigation is broken (if num_errors > 1):
    " set errorformat=
    "         \%-G,
    "         \%-Gerror:\ aborting\ %.%#,
    "         \%-Gerror:\ Could\ not\ compile\ %.%#,
    "         \%Eerror:\ %m,
    "         \%Eerror[E%n]:\ %m,
    "         \%Wwarning:\ %m,
    "         \%Inote:\ %m,
    "         \%C\ %#-->\ %f:%l:%c,
    "         \%E\ \ left:%m,%C\ right:%m\ %f:%l:%c,%Z
    if exists('g:My_eval_var')
        return
    endif
    let l:out = system("cargo locate-project")
    if stridx(l:out, 'error: could not find `Cargo.toml` in ') == 0
        let g:My_eval_var = "MyRunShellCmd rustc -g " . expand("%:t")
                    \ . " -o /tmp/" . expand("%:t") . ".out && /tmp/"
                    \ . expand("%:t") . ".out"
        return
    endif
    " l:out should look like: '{"root":"/path/to/Cargo.toml"}'
    let l:tmp = strpart(l:out, 9)
    let l:dir = "'" . strpart(l:tmp, 0, strlen(l:tmp) - 14) . "'"
    " let l:dir = "system('git rev-parse --show-toplevel')[:-2]"
    let g:My_cargo_prefix = "exe 'cd ' . " . l:dir . " | MyRunShellCmd cargo "
    let g:My_eval_var = g:My_cargo_prefix . 'run'
    let g:which_key_map.r.b = [':call My_set_cargo_cmd("build")', 'cargo build']
    let g:which_key_map.r.r = [':call My_set_cargo_cmd("run")', 'cargo run']
    let g:which_key_map.r.t = [':call My_set_cargo_cmd("test")', 'cargo test']
endfunction

function! My_is_linux_kernel_source_file(full_path)
    if g:My_is_windows
        return 0
    endif
    let l:full_path = system("realpath '" . a:full_path . "'")[:-2]
    let l:subdirs = ['arch', 'crypto', 'drivers', 'fs', 'init', 'ipc', 'kernel',
                \ 'mm', 'net', 'security', 'sound']
    let l:cur_dir = l:full_path
    while 1
        let l:idx = strridx(l:cur_dir, '/')
        if l:idx <= 0
            return 0
        endif
        let l:cur_dir = strpart(l:cur_dir, 0, l:idx)
        let l:subdir_not_found = 0
        for l:subdir in l:subdirs
            if ! isdirectory(l:cur_dir . '/' . l:subdir)
                let l:subdir_not_found = 1
                break
            endif
        endfor
        if l:subdir_not_found == 0
            return 1
        endif
    endwhile
endfunction

function! My_grep_in_dir(dir)
    " -default-action=my_split -post-action=suspend
    let g:My_use_denite_errors = 1
    execute 'Denite -start-filter -path=' . a:dir . ' grep:::!'
endfunction

function! My_vifm_choose(action)
    let l:chosen = tempname()
    let l:callback = { 'chosen' : l:chosen, 'action' : a:action }
    function! callback.on_exit(id, exit_code, event)
        silent! bdelete! #
        if a:exit_code != 0
            call nvim_err_writeln('Got non-zero code from vifm: ' . a:exit_code)
            return
        endif
        if self.action == 'ripgrep'
            let g:My_grep_dir = join(readfile(self.chosen))
            call My_grep_in_dir(g:My_grep_dir)
        " elseif self.action == 'something'
        else
            call nvim_err_writeln('Got unsupported action: ' . self.action)
        endif
    endfunction
    enew
    call termopen('vifm --choose-dir ' . l:chosen, l:callback)
    " let oldbuf = bufname('%')
    " execute 'keepalt file' escape('vifm: '.'edit', ' |')
    " execute bufnr(oldbuf).'bwipeout'
    setlocal norelativenumber
    setlocal nonumber
    startinsert
endfunction

let g:My_grep_dir = ''
function! My_grep_recursive()
    if g:My_grep_dir == ''
        call My_vifm_choose("ripgrep")
    else
        call My_grep_in_dir(g:My_grep_dir)
    endif
endfunction

let g:My_vim_errors_file = g:My_tmp_dir . '/vim_errors__'
            \ . tr(expand('%:t'), '/', '_') . '.err'

function! My_run_shell_cmd(open_window_on_success, cmd)
    silent wa

    silent execute '!$DOTRC/other_files/nvim_execute_cmd.py --exclude '
                \ . v:servername . ' "call My_save_all_files()"'
    " Previous command generated 1 empty line in message area. Afterwards we
    " will additionally print command exit code (2nd line). In this case due to
    " more than 1 line of output, the user will be forced to type ENTER and
    " presented with the following message:
    " Press ENTER or type command to continue
    " To avoid 2nd line message we will redraw the screen:
    redraw

    silent! execute 'noautocmd silent botright pedit ' . g:My_vim_errors_file
    " jump to previous window:
    noautocmd wincmd P
    " set buftype=nofile
    setlocal filetype=my_shell_cmd_output
    set noreadonly
    normal ggdG
    " exe 'noautocmd r! $DOTRC/other_files/vifm_run_command.sh ' . a:cmd
    " \ . ' 2>&1'
    " AnsiEsc
    if g:My_win_cmd_exe
        let l:cmd = 'noautocmd silent r! ' . g:My_win_dotrc
            \ . '\windows\other_files\vim_cmd_exe_wrapper.bat ' . a:cmd
    else
        let l:cmd = 'noautocmd silent r!
\ echo "Command: ' . a:cmd . ' Command output:" &&
\ exec 2>&1 ;
\ ' . a:cmd . ' ;
\ EXIT_CODE=$? ;
\ if [ $EXIT_CODE -eq 0 ]; then
\     echo "Command succeeded (exit code = 0)" ;
\ else
\     echo "Command failed (exit code = $EXIT_CODE)" ;
\ fi ;
\ exit $EXIT_CODE
\ '
    endif
    execute l:cmd
    normal 0ggdd
    silent w
    set readonly

    " wincmd p
    " execute 'lgetfile ' . g:My_vim_errors_file
    " wincmd p
    execute 'cgetfile ' . g:My_vim_errors_file

    let g:My_use_denite_errors = 0
    wincmd p
    if v:shell_error == 0
        if ! a:open_window_on_success
            pclose
        endif
        echo "Command succeeded (exit code = 0)"
    else
        echo "Command failed (exit code = " . v:shell_error . ")"
    endif
endfunction
command! -nargs=1 MyRunShellCmd :call My_run_shell_cmd(1, '<args>')
command! -nargs=1 MyRunShellCmdNoOpen :call My_run_shell_cmd(0, '<args>')

function! My_run_shell_cmd_interactive(cmd)
    let l:dir = expand('%:p:h')
    execute '!x-terminal-emulator -e bash -c "cd ' . l:dir . ' && ' . a:cmd
                \ . ' ; echo \"Exit code: $?\" && vifm-pause"'
    redraw
endfunction
command! -nargs=1 MyRunShellCmdInteractive :call
            \ My_run_shell_cmd_interactive('<args>')

function! My_is_lq_list()
    exec 'redir @x | silent ls | redir END'
    if match(@x,'%a-  "\[Location List\]"') >= 0
        return 1
    elseif match(@x,'%a-  "\[Quickfix List\]"') >= 0
        return 0
    else
        return -1
    endif
endfunction

function! My_open_location()
    let l:lqList = My_is_lq_list()
    if l:lqList == -1
        exec 'echoerr "Neither Location nor Quickfix List focused."'
        return
    endif
    let g:My_use_denite_errors = 0
    if l:lqList == 1
        execute "ll " . line('.')
    else
        execute "cc " . line('.')
    endif
    normal zz
endfunction

" Russian keyboard layout:
set keymap=russian-jcukenwin
set iminsert=0 " default english in insert mode
set imsearch=0 " default english while searching
function! My_update_status_line(message, status)
    if &iminsert == 1
        if &keymap == 'russian-jcukenwin'
            let l:lang='RU'
        else
            let l:lang='UA'
        endif
    else
        let l:lang='EN'
    endif
    let &statusline=l:lang . '   file: %f %p%% %c   ' . a:message
    if a:status == 'error'
        hi StatusLine ctermbg=160
    endif
    if a:status == 'normal'
        hi StatusLine ctermbg=46
    endif
    set laststatus=2
    redrawstatus!
endfunction
call My_update_status_line('', 'normal')

function! My_swap_keyboard_layout()
    if &iminsert == 0 " if current layout is english
        " then switch to russian
        set iminsert=1
        set imsearch=1
        set keymap=russian-jcukenwin
    else
        if &keymap == 'russian-jcukenwin' " if current layout is russian
            " then switch to ukrainian
            set keymap=my_ukrainian-jcuken
        else " current layout is ukrainian
            " switch to english
            set iminsert=0
            set imsearch=0
        endif
    endif
    call My_update_status_line('', 'normal')
endfunction

function! My_insert_snippet()
    let l:file_name = expand('%:t')
    let l:file_path = expand('%:p')
    let l:ext = expand('%:e')
    if &filetype == "c" || &filetype == "cpp"
        if l:ext == 'h' || l:ext == 'hpp'
            let l:guard_name = tr(toupper(l:file_name), '.', '_')
            call append(0, ['#ifndef ' . l:guard_name, '#define '
                        \ . l:guard_name])
            call append(line('$'), ['', '#endif // #ifndef ' . l:guard_name])
        else
            call append(0, [
\ "#include <stdio.h>",
\ "#include <stdint.h>",
\ "#include <stddef.h>",
\ "",
\ "int main(int argc, char *argv[])",
\ "{",
\ "    // (void)argc; (void)argv;",
\ "    printf(\"argc = %d, argv = {|%s|\", argc, argv[0]);" .
\    " for (int i = 1; i < argc; ++i)" .
\        " printf(\", |%s|\", argv[i]);" .
\    " puts(\"}\");",
\ "    return 0;",
\ "}",
\ ])
        if getline('.') == ''
            normal "zdd2k
        else
            normal 3k
        endif
        endif
    elseif &filetype == "rust"
        call append(0, [
\ "use std::env;",
\ "",
\ "fn main()",
\ "{",
\ "    let args: Vec<String> = env::args().collect();",
\ "    println!(\"args: {:?}\", args);",
\ "}",
\ ])
        if getline('.') == ''
            normal "zdd2k
        else
            normal 3k
        endif
    elseif &filetype == "java"
        call append(0, [
\ "class " . expand("%:t:r") . " {",
\ "    public static void main(String args[]) {",
\ "        System.out.println(\"hello world!\");",
\ "    }",
\ "}",
\ ])
        if getline('.') == ''
            normal "zdd2k
        else
            normal 3k
        endif
    elseif &filetype == "sh"
        if stridx(l:file_path, 'other_files/settings_merge/') != -1
            if stridx(l:file_path, '/dotrc_s/') != -1
                let l:func = 'config_dotrc_s'
            else
                let l:func = 'config_dotrc'
            endif
            call append(0, [
\ l:func . "()",
\ "(",
\ "    set -e",
\ ")",
\ ])
            if getline('.') == ''
                normal "zdd
            endif
            normal gg2j
        else
            call append(0, [
\ "#!/bin/bash",
\ "",
\ "echo \"\\$# = |$#|\"; echo \"\\$0 = |$0|\"; echo \"\\$@ = |$@|\"",
\ ])
            if getline('.') == ''
                normal "zdd
            else
                normal k
            endif
        endif
    elseif &filetype == "perl"
        call append(0, [
\ "#!/usr/bin/perl",
\ "",
\ "use strict;",
\ "use warnings;",
\ "",
\ 'print "hello world!\n";',
\ 'use Data::Dumper qw(Dumper); print Dumper \@ARGV;',
\ ])
        if getline('.') == ''
            normal "zdd
        endif
    elseif &filetype == "python"
        call append(0, [
\ "#!/usr/bin/python3",
\ "",
\ "import sys; print(sys.argv)",
\ ])
        if getline('.') == ''
            normal "zdd
        else
            normal k
        endif
    elseif &filetype == "sql"
        call append(0, [
\ "create table " . expand("%:r") . "(text, priority INTEGER);",
\ "insert into " . expand("%:r") . " values('deliver project description', 10);",
\ "insert into " . expand("%:r") . " values('lunch with Christine', 100);",
\ "",
\ "select * from " . expand("%:r") . ";",
\ ])
        if getline('.') == ''
            normal "zdd
        else
            normal k
        endif
    endif
endfunction

function! My_window_is_temporary()
    let l:dir_name = expand('%:p:h')
    let l:file_name = expand('%:t')
    if l:dir_name == '/usr/share/vim/vim74/doc' || &l:buftype == 'help'
\ || &l:buftype == 'quickfix' || &l:buftype == 'nofile'
\ || &l:buftype == 'nowrite' || l:file_name == 'swoopBuf'
        " || l:file_name == 'search-results.agsv'
        return 1
    endif
    let l:last_part_expected = '.fugitiveblame'
    let l:len_expected = strlen(l:last_part_expected)
    let l:len_actual = strlen(l:file_name)
    if l:len_actual > l:len_expected
        let l:diff = l:len_actual - l:len_expected
        let l:last_part_actual = strpart(l:file_name, l:diff)
        if l:last_part_actual == l:last_part_expected
            return 1
        endif
    endif
    if &pvw == 1 " If it is a preview window
        return 1
    endif
    return 0
endfunction
function! My_close_window_if_temporary()
    if My_window_is_temporary() != 0
        execute ':q!'
    endif
endfunction
command! MyCloseWindowIfTemporary call My_close_window_if_temporary()

function! My_save_all_files()
    try
        wa
        " catch exception if there was no file name associated with the buffer:
    catch /^Vim\%((\a\+)\)\=:E141/ " no file name for this buffer
        echo v:exception
    endtry
endfunction

if ! exists('g:My_modify_line___text_to_prepend')
    let g:My_modify_line___text_to_prepend = '        '
endif
if ! exists('g:My_modify_line___start_column')
    let g:My_modify_line___start_column = 78
endif
if ! exists('g:My_modify_line___text_to_append')
    let g:My_modify_line___text_to_append = ' \'
endif
function! My_modify_line(text_to_prepend, start_column, text_to_append)
    let l:cur_line = getline('.')
    " Prepend text:
    let l:cur_line = substitute(l:cur_line, "^[ \t]*", a:text_to_prepend, "")
    " Delete text ending if necessary:
    let l:cur_line_len = strlen(l:cur_line)
    let l:append_text_len = strlen(a:text_to_append)
    let l:diff = l:cur_line_len - l:append_text_len
    let l:cur_line_ending = strpart(l:cur_line, l:diff)
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
command! -range MyModifyLine <line1>,<line2> call My_modify_line(
\ g:My_modify_line___text_to_prepend, g:My_modify_line___start_column,
\ g:My_modify_line___text_to_append)

function! My_view_in_new_buffer(cmd)
    redir @z
    execute 'silent ' . a:cmd
    redir END
    tabnew
    put z
endfunction
command! -nargs=1 MyViewInNewBuffer call My_view_in_new_buffer(<f-args>)
" Usage example: :MyViewInNewBuffer :map<CR> (show mappings in buffer).

function! My_copy_location()
    let @+ = expand('%:p') . ':' . line('.')
    echo 'copied: ' . @+
endfunction

" vim-commentary:
autocmd FileType unknown setlocal commentstring=#\ %s

let g:My_tcomment_configured = 0
function! My_configure_tcomment()
    " if exists('loaded_tcomment')
    if ! g:My_tcomment_configured && exists('g:tcomment_maps')
        call tcomment#type#Define('unknown', '# %s')
        call tcomment#type#Define('lisp', ';; %s')
        call tcomment#type#Define('make', '# %s')
        call tcomment#type#Define('gdb', '# %s')
        call tcomment#type#Define('arm_ds_5_script', '# %s')
        call tcomment#type#Define('kconfig', '# %s')
        call tcomment#type#Define('sudoers', '# %s')
        call tcomment#type#Define('inittab', '# %s')
        call tcomment#type#Define('mplayerconf', '# %s')
        call tcomment#type#Define('text', '# %s')
        call tcomment#type#Define('toml', '# %s')
        call tcomment#type#Define('ninja', '# %s')
        call tcomment#type#Define('wget', '# %s')
        call tcomment#type#Define('foot_ini', '# %s')
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
        let g:My_tcomment_configured = 1
        " Was not used:
        " call tcomment#type#Define('c_block', g:tcommentBlockC2)
        " call tcomment#type#Define('cpp_block', g:tcommentBlockC2)
    endif
endfunction

" if has('nvim')
"     " First invoke terminal:
"     nmap <Leader>tt :silent w<CR>:e term://bash<CR>:startinsert<CR>
"     " Use <C-Space> in terminal to switch to normal mode:
"     tnoremap <C-@> <C-\><C-n>:set relativenumber<CR>
"     " In terminal switch back to insert mode:
"     nmap <C-@> :set norelativenumber<CR>:startinsert<CR>
"     " Window management with terminal:
"     tnoremap <C-w><C-w> <C-\><C-n><C-w><C-w>
"     autocmd TermOpen * startinsert
"     autocmd WinEnter term://* startinsert
"     " Copy modified file name from git status:
"     function! Git_copy_modified_file_name(file_number)
"         let l:line_1 = search('        modified:   ', 'b')
"         if l:line_1 == 0 " not found
"             echo 'Error: modified by git files not found'
"             return
"         endif
"         let @+ = strpart(getline(l:line_1 - a:file_number + 1), 20)
"         set norelativenumber
"         startinsert
"     endfunction
"     command! -nargs=1 GitCopy call Git_copy_modified_file_name(<f-args>)
"     " nmap <Leader>gg :call Git_copy_modified_file_name(1)<CR>
"     " nmap <Leader>ga :GitCopy 
" endif

" autocmd TermClose * bd! " do not show 'Process exited ' message
" sp | wincmd j | e term://f s | set norelativenumber | set nonumber
" \ | startinsert

function! My_get_file_name_under_cursor(line)
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

function! My_open_file(win, cmd) " at least one colon expected
    " ('cur_win', 'e') "under cursor current window
    " ('prev_win', 'sp') " under cursor horizontal split
    " ('cur_win', 'vsp') " under cursor vertical split
    " ('cur_win', 'tabedit') " under cursor tabedit
    let l:line = strpart(getline('.'), col(".") - 1)
    let l:file = My_get_file_name_under_cursor(l:line)
    if a:win == 'prev_win'
        " execute "normal \<c-w>\<c-p>"
        execute 'wincmd p'
    endif
    execute a:cmd . ' ' . l:file
endfunction

function! My_fuzzy_open_file()
    let l:dir = expand('%:p:h') " current file's directory
    let l:cmd = 'git -C ' . l:dir . ' rev-parse --show-toplevel 2>/dev/null'
    let l:git_root = system(l:cmd)
    if l:git_root != ''
        let l:dir = l:git_root[:-2]
    endif
    let g:My_use_denite_errors = 1
    execute 'Denite -start-filter -path=' . l:dir . ' file/rec'
endfunction

function! My_get_region_lines(start, end)
    let l:cursor = getpos('.')
    try
        execute 'normal $?' . a:start . ''
        let l:first_line = getpos('.')[1]
    catch
        call setpos('.', l:cursor)
        throw "can't find: " . a:start
    endtry
    try
        execute 'normal 0/' . a:end . ''
        let l:last_line = getpos('.')[1]
    catch
        call setpos('.', l:cursor)
        throw "can't find: " . a:end
    endtry
    call setpos('.', l:cursor)
    let l:cur_line = l:cursor[1]
    if l:cur_line < l:first_line - 1 || l:cur_line > l:last_line + 1
        throw "cursor is not inside region specified"
    endif
    echo ''
    return [l:first_line, l:last_line]
endfunction

function! My_get_filetype_comments(first_line_num)
    let l:first_line = getline(a:first_line_num)
    let l:start = stridx(l:first_line, '|')
    return strpart(l:first_line, l:start + 1)[:-2]
endfunction

function! My_eval_vim_lines(lines_range, filetype_comments)
    let l:full_lines_list = getbufline('%', a:lines_range[0], a:lines_range[1])
    let l:lines_list = []
    let l:cur_line = ''
    if a:filetype_comments != ''
        let l:pattern = '^\s*' . escape(a:filetype_comments, '*')
    endif
    for l:line in l:full_lines_list
        if a:filetype_comments != ''
            " strip filetype comments:
            let l:prefix_len = strlen(matchstr(l:line, l:pattern))
            if l:prefix_len != 0
                let l:line = strpart(l:line, l:prefix_len)
            endif
        endif
        " ignore vim comments:
        let l:idx = match(l:line, '^\s*"\s*')
        if l:idx != -1
            continue
        endif
        " check if it is a continuation of previous line:
        let l:prefix_len = strlen(matchstr(l:line, '^\s*\\'))
        if l:prefix_len != 0
            " yes, it is a continuation of previous line
            let l:cur_line = l:cur_line . strpart(l:line, l:prefix_len)
            continue
        endif
        " no, it is not a continuation of previous line
        if strlen(l:cur_line) != 0
            let l:lines_list = add(l:lines_list, l:cur_line)
        endif
        let l:cur_line = l:line
    endfor
    let l:lines_list = add(l:lines_list, l:cur_line)
    let l:text = join(l:lines_list, "\n")
    " echo l:text
    execute l:text
endfunction

function! My_eval_vim()
    try
        let l:lines_range = My_get_region_lines('EVAL REGION BEGINS HERE: |',
                    \ 'EVAL REGION ENDS HERE.')
        let l:filetype_comments = My_get_filetype_comments(l:lines_range[0])
        let l:lines_range = [l:lines_range[0] + 1, l:lines_range[1] - 1]
        call My_eval_vim_lines(l:lines_range, l:filetype_comments)
    catch
        echo v:exception
    endtry
endfunction

function! My_vimscript_function_eval()
    try
        let l:lines_range = My_get_region_lines('^function', '^endfunction$')
        let l:function_line = getline(l:lines_range[0])
        let l:function_name = matchstr(l:function_line, ' [^ (]*')[1:]
        call My_eval_vim_lines(l:lines_range, '')
        return l:function_name
    catch
        echo v:exception
    endtry
endfunction

function! My_insert_eval_region(text)
    let l:has_shebang = 0
    if &filetype == "c" || &filetype == "cpp"
                \ || &filetype == "rust" || &filetype == "java"
        let l:cmd = 'i/* EVAL REGION BEGINS HERE: |* |' . a:text
                    \ . 'EVAL REGION ENDS HERE. */k0'
    elseif &filetype == "dot"
        let l:cmd = 'i/* EVAL REGION BEGINS HERE: |* | * ' . a:text
                    \ . '* EVAL REGION ENDS HERE. */k0'
    elseif &filetype == "sh" || &filetype == "python" || &filetype == "toml"
        let l:has_shebang = 1
        let l:cmd = 'i# EVAL REGION BEGINS HERE: |# |# ' . a:text
                    \ . '# EVAL REGION ENDS HERE.k'
    elseif &filetype == "perl"
        let l:has_shebang = 1
        let l:cmd = 'i# EVAL REGION BEGINS HERE: |# |' . a:text
                    \ . 'EVAL REGION ENDS HERE.k'
    elseif &filetype == "vim"
        let l:cmd = 'i" EVAL REGION BEGINS HERE: |" |' . a:text
                    \ . 'EVAL REGION ENDS HERE.k0'
    else
        let l:cmd = 'iEVAL REGION BEGINS HERE: ||' . a:text
                    \ . 'EVAL REGION ENDS HERE.k0'
    endif

    let l:formatoptions = &formatoptions
    setlocal formatoptions-=t
    setlocal formatoptions-=c
    let l:line = line('.')
    if l:line == 1
        if l:has_shebang
            call append(l:line, ['', '', ''])
            normal 2j
        else
            call append(0, ['', ''])
            normal 2k
        endif
    elseif l:line == line('$')
        call append(l:line, ['', ''])
        normal 2j
    else
        call append(l:line - 1, ['', '', ''])
        normal 2k
    endif
    execute ':normal! ' . l:cmd
    execute 'setlocal formatoptions=' . l:formatoptions
    if a:text == ''
        startinsert!
    endif
endfunction

function! My_insert_eval_variable()
    let l:text = 'let g:My_eval_var = "' . g:My_eval_var . '"'
    call My_insert_eval_region(l:text)
endfunction

let g:My_use_denite_errors = 0
function! My_goto_error(error)
    " if bufexists('[denite]-default') " 'denite-filter'
    if g:My_use_denite_errors
        if a:error == 'current'
            let l:cmd = ''
        elseif a:error == 'next'
            let l:cmd = '-cursor-pos=+1'
        elseif a:error == 'previous'
            let l:cmd = '-cursor-pos=-1'
        else
            let l:cmd = '-cursor-pos=' . a:error
        endif
        execute 'silent Denite -resume -immediately ' . l:cmd
        return
    endif

    " if len(getloclist(0)) != 0
    "     let l:prefix = 'l'
    " else
    "     let l:prefix = 'c'
    " endif
    let l:prefix = 'c'

    let l:action = l:prefix . l:prefix . '!'
    if a:error == 'current'
        let l:action = l:action
    elseif a:error == 'next'
        let l:action = 'silent ' . l:action . ' | ' . l:prefix . 'next!'
    elseif a:error == 'previous'
        let l:action = 'silent ' . l:action . ' | ' . l:prefix . 'previous!'
    else
        let l:action = l:action . ' ' . a:error
    endif

    let l:err_buf_name = g:My_vim_errors_file
    let l:in_err_win = 0
    if bufname('%') == l:err_buf_name
        let l:in_err_win = 1
        execute 'wincmd p'
    endif
    try
        execute l:action
    catch
        if l:in_err_win
            execute 'wincmd p'
        endif
        return
    endtry
    " let l:err_buf_win = win_findbuf(bufnr(l:err_buf_name))[0]
    let l:err_buf_win = bufwinid(l:err_buf_name)
    if l:err_buf_win == -1
        return
    endif
    " let l:cur_win = winnr()
    " execute l:cur_win . 'wincmd w'
    let l:cur_win = bufwinid('%')
    let l:cur_pos = getpos('.')
    let l:quickfix_open = 0
    for winnr in range(1, winnr('$'))
        if getwinvar(winnr, '&syntax') == 'qf'
            let l:quickfix_open = 1
            break
        endif
    endfor
    execute 'copen'
    let l:err = getpos('.')
    if ! l:quickfix_open
        execute 'cclose'
    else
        execute 'wincmd p'
    endif
    call win_gotoid(l:err_buf_win)
    call setpos('.', [l:err_buf_name, l:err[1], l:err[2], l:err[3]])
    execute 'normal zz'
    call win_gotoid(l:cur_win)
    call setpos('.', l:cur_pos)
    execute 'normal zz'
    if l:in_err_win
        execute 'wincmd p'
    endif
endfunction

function! My_edit_vimrc()
    let l:file = $DOTRC . "/windows/home_settings/.vimrc"
    execute 'tabedit ' . l:file
    function! My_done_editing_vimrc(file)
        if g:My_is_windows
            silent execute '!cp "' . a:file . '" "' . $MYVIMRC . '"'
        else
            silent execute '!source ' . $DOTRC .
            \ '/other_files/config_file.sh && config_generate -h .vimrc'
        endif
        " execute the following command manually:
        " source $MYVIMRC
    endfunction
    execute 'autocmd! BufLeave ' . l:file . ' call My_done_editing_vimrc("'
    \ . l:file . '")'
endfunction

function! My_rtags_preprocess()
    function! s:My_rtags_preprocess_file_handler(result)
        let l:preprocessed_file = '/tmp/vim_rtags_preprocessed_formatted.i'
        execute 'vnew ' . l:preprocessed_file
        normal ggdG
        call append(0, a:result)
        silent execute 'wq'
        silent execute '!beautify.sh ' . l:preprocessed_file
        execute 'vsplit ' . l:preprocessed_file
    endfunction
    call rtags#ExecuteThen({'-E' : expand("%:p")},
                \ [function('s:My_rtags_preprocess_file_handler')])
    " Original code:
    "
    " function! rtags#PreprocessFileHandler(result)
    "     vnew
    "     call append(0, a:result)
    " endfunction
    " function! rtags#PreprocessFile()
    "     call rtags#ExecuteThen({ '-E' : expand("%:p") },
    "                 \ [function('rtags#PreprocessFileHandler')])
    " endfunction
endfunction

function! My_call_graph_paste_location()
    let l:clipboard = @+
    let l:colon_idx = stridx(l:clipboard, ':')
    let l:line_num = strpart(l:clipboard, l:colon_idx + 1)
    let l:file_path = strpart(l:clipboard, 0, l:colon_idx)
    let l:file_path = system("realpath '" . l:file_path . "'")[:-2]
    let l:file_dir = system("dirname '" . l:file_path . "'")[:-2]
    let l:cmd = "git -C '" . l:file_dir . "' rev-parse --show-toplevel"
    let l:git_root_dir = system(l:cmd)[:-2]
    let l:prefix_len = strlen(l:git_root_dir)
    let l:file = strpart(l:file_path, l:prefix_len + 1)
    let l:str = "'" . l:file . "', " . l:line_num . ')'
    call append(line('.'), l:str)
    " normal $F,;2lDJ0
    normal J0
endfunction

function! My_print_cur_line_to_terminal()
    " let l:cur_line = getline('.')
    " " Escape hash characters:
    " let l:cur_line = substitute(l:cur_line, '#', '\\#', 'g')
    " " echo l:cur_line
    " execute '!echo -e "Copy the following:\n" && echo "' . l:cur_line . '"'
    execute 'w|!echo -e "\nCopy the following:\n" && sed -n ' . line('.') . 'p '
                \ "'" . expand('%:f') . "'"
endfunction

" Fugitive (git):
" :copen - open quickfix window
" In status window use:
" g?    show help
" "-" to "git add" or "git reset" file (depending on where your cursor is).
" <c-n> to go to next file, <c-p> to go to previous file.
" <Enter> to open current file in window below.
" ca    :Gcommit --amend
" diff next file:
" nmap <Leader>gdn :q<CR>:q<CR>:exec ':Gdiff ' . g:Gdiff_arg<CR>
" nmap <Leader>gw :Gwrite<CR><C-w>k
" nmap <Leader>gr :Gread<CR>:w<CR><C-w>k<C-n>
" nmap <Leader>ga <C-w>k-

" vimagit:
let g:magit_default_fold_level=2
let g:magit_discard_untracked_do_delete=1
let g:magit_discard_hunk_mapping='X'

nmap <C-k> :call My_swap_keyboard_layout()<CR>
vmap <C-k> <Esc>:call My_swap_keyboard_layout()<CR>gv
imap <C-k> <Esc>:call My_swap_keyboard_layout()<CR>gi

" Apply macro to selected lines:
vmap i :normal @
" Block comment (use gcb<motion> in normal mode):
vmap gb :TCommentBlock<CR>
" Use Y to copy until end of line:
nmap Y y$
" Exit from vim:
nmap <C-d> :q<CR>
" Move current tab left and right (move tab):
nmap <silent> <S-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nmap <silent> <S-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>
" Scroll horizontally:
nmap <C-l> zl
nmap <C-h> zh
" Mouse vertical scroll with the wheel: scrool 1 line instead of 3:
map <ScrollWheelUp> <C-Y>
map <ScrollWheelDown> <C-E>
" Open location in QuickFix window:
autocmd FileType qf nmap <buffer> o :call My_open_location()<CR>
" Reformat C/C++ source code:
" nmap <C-u> :%d<CR>:r !uncrustify -f %<CR>:1,1d<CR>
" nmap <C-u> :%d<CR>:r !astyle.sh %<CR>

" which-key (hotkeys):
set timeoutlen=1000
let g:which_key_map =  {}

let g:which_key_map.b = { 'name' : '+buffers',
\   'b' : [':Denite buffer', 'select'],
\   'n' : [':bnext', 'next'],
\   'p' : [':bprevious', 'previous'],
\ }

let g:which_key_map.c = { 'name' : '+compile/clipboard',
\   'l' : [':call My_copy_location()', 'copy full location to clipboard'],
\ }

let g:which_key_map.d = {'name' : '+diff',
\   '=' : ['<c-w>=', 'restore diff panels (after "t")'],
\   'j' : [':let g:My_use_denite_errors = 1 | Denite -resume', 'resume denite'],
\   'k' : [':bdelete [denite]-default', 'kill denite'],
\   'm' : [':MyModifyLine', 'modify line'],
\   'q' : [':windo MyCloseWindowIfTemporary', 'close temporary windows'],
\   't' : [':resize +1000 | vertical resize +1000', 'show this panel only, hide'
\             . ' another one'],
\   'u' : [':diffupdate', 'diffupdate (recalculate diff)'],
\ }

let g:which_key_map.e = {'name' : '+errors',
\   'c' : [':call My_goto_error("current")', 'current error'],
\   'm' : [':botright pedit ' . g:My_vim_errors_file . ' | set readonly',
\             'my list errors'],
\   'n' : [':call My_goto_error("next")', 'next error'],
\   'p' : [':call My_goto_error("previous")', 'previous error'],
\   'q' : [':copen', 'quickfix list errors (restore errors window)'],
\ }

" :EditVifm :SplitVifm :VsplitVifm :DiffVifm :TabVifm
let g:which_key_map.f = {'name' : '+files',
\   ',' : [':EditVifm', 'vifm'],
\   'e' : {'name' : '+edit',
\     'v' : [':call My_edit_vimrc()', 'vimrc'],
\    },
\   'f' : [':call My_fuzzy_open_file()', 'fuzzy'],
\   't' : [':call My_open_file("cur_win", "tabedit")', 'under cursor tabedit'],
\ }

" \   's' : [':Gstatus', 'status'],
" Use 'P' in *.fugitiveblame to reblame at parent, see: help fugitive-:Gblame
let g:which_key_map.g = {'name' : '+git',
\   'b' : [':Gblame', 'blame'],
\   'c' : {'name' : '+commit',
\     'a' : [':Gcommit --amend', 'amend'],
\     'o' : [':Gcommit', 'default'],
\   },
\   'd' : {'name' : '+diff',
\     'h' : [':Gdiff HEAD', 'HEAD'],
\     'i' : [':Gdiff', 'index'],
\   },
\   'e' : [':MyRunShellCmd git show --name-only --pretty= HEAD',
\             'show files in HEAD'],
\   's' : [':wa | Magit', 'status'],
\   'u' : [':e', 'update (reload) buffer'],
\ }

let g:which_key_map.i = { 'name' : '+insert',
\   'e' : [':call My_insert_eval_region("")', 'insert eval region'],
\   's' : [':call My_insert_snippet()', 'insert snippet'],
\   'v' : [':call My_insert_eval_variable()', 'insert eval variable'],
\ }

" Jump to character within current line:
" nmap <Leader>l <Plug>(easymotion-sl)
" vmap <Leader>l <Plug>(easymotion-sl)
" Jump to character within screen:
nmap <Leader>jj <Plug>(easymotion-s)
vmap <Leader>jj <Plug>(easymotion-s)
nmap <Leader>jw <Plug>(easymotion-bd-W)
vmap <Leader>jw <Plug>(easymotion-bd-W)
let g:which_key_map.j = { 'name' : '+jump',
\ }

let g:which_key_map.l = { 'name' : '+location/layout/LSP',
\   'c' : [':call My_call_graph_paste_location()', 'call graph paste location'],
\   'd' : [':call LanguageClient_textDocument_definition()',
\          'LSP jump to definition'],
\   'h' : [':call LanguageClient_textDocument_hover()', 'LSP hower'],
\   'i' : [':call LanguageClient_textDocument_documentSymbol()',
\          'LSP symbol info'],
\   'l' : [':SLoad __LAST__', 'layout load'],
\   'n' : [':lne', 'location list next'],
\   'p' : [':lp', 'location list previous'],
\   's' : [':SSave! __LAST__', 'layout save'],
\ }

" let g:LanguageClient_serverCommands = { 'c': ['clangd',
"             \ '-compile-commands-dir=your_dir'], }
" let g:LanguageClient_serverCommands = { 'c':
"             \ ['/media/files/workspace/ccls/Release/ccls'], }
" silent UpdateRemotePlugins
" LanguageClientStart

let g:which_key_map.m = { 'name' : '+my',
\   'e' : [':call My_eval_vim()', 'eval vim'],
\ }

" If we use:
" \   'c' : ['0"zD', 'clear current line'],
" we get error: Vim(call):E116: Invalid arguments for function feedkeys
" Problem: double quote in string. Therefore we will remap this later
" (see LABEL_1).
let g:which_key_map.o = { 'name' : '+other',
\   'c' : ['0D', 'clear current line'],
\   'f' : [':execute g:My_eval_var', 'evaluate variable'],
\   't' : [':call My_print_cur_line_to_terminal()',
\             'print current line to terminal for copying'],
\ }

let g:which_key_map.r = { 'name' : '+rtags',
\   'C' : [':let g:My_use_denite_errors = 0 | call rtags#FindSuperClasses()',
\             'FindSuperClasses'],
\   'F' : [':let g:My_use_denite_errors = 0 | call rtags#FindRefsCallTree()',
\             'FindRefsCallTree'],
\   'J' : [':call rtags#JumpTo(g:SAME_WINDOW, { "--declaration-only" : "" })',
\             'JumpTo SAME_WINDOW --declaration-only'],
\   'S' : [':call rtags#JumpTo(g:H_SPLIT)', 'JumpTo H_SPLIT'],
\   'T' : [':call rtags#JumpTo(g:NEW_TAB)', 'JumpTo NEW_TAB'],
\   'V' : [':call rtags#JumpTo(g:V_SPLIT)', 'JumpTo V_SPLIT'],
\   'b' : [':call rtags#JumpBack()', 'JumpBack'],
\   'c' : [':let g:My_use_denite_errors = 0 | call rtags#FindSubClasses()',
\             'FindSubClasses'],
\   'd' : [':call rtags#Diagnostics()', 'Diagnostics'],
\   'e' : [':call My_rtags_preprocess()', 'Preprocess'],
\   'f' : [':let g:My_use_denite_errors = 0 | call rtags#FindRefs()',
\             'FindRefs'],
\   'h' : [':call rtags#ShowHierarchy()', 'ShowHierarchy'],
\   'i' : [':call rtags#SymbolInfo()', 'SymbolInfo'],
\   'j' : [':call rtags#JumpTo(g:SAME_WINDOW)', 'JumpTo SAME_WINDOW'],
\   'l' : [':let g:My_use_denite_errors = 0 | call rtags#ProjectList()',
\             'ProjectList'],
\   'n' : [':let g:My_use_denite_errors = 0 | call rtags#FindRefsByName'
\          . '(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))',
\             'FindRefsByName'],
\   'p' : [':call rtags#JumpToParent()', 'JumpToParent'],
\   'r' : [':call rtags#ReindexFile()', 'ReindexFile'],
\   's' : [':let g:My_use_denite_errors = 0 | call rtags#FindSymbols'
\          . '(input("Pattern? ", "", "customlist,rtags#CompleteSymbols"))',
\             'FindSymbols'],
\   't' : [':%s/\t/    /g', 'replace tabs with spaces'],
\   'v' : [':let g:My_use_denite_errors = 0 | call rtags#FindVirtuals()',
\             'FindVirtuals'],
\   'w' : [':call rtags#RenameSymbolUnderCursor()', 'RenameSymbolUnderCursor'],
\ }

noremap <c-LeftMouse> <LeftMouse>:call rtags#JumpTo(g:SAME_WINDOW)<CR>
noremap <c-RightMouse> <LeftMouse>:let g:My_use_denite_errors = 0<CR>:call rtags#FindRefs()<CR>
noremap <c-w> <c-o>

" nmap <F3> :set hlsearch!<CR> " set/unset search highlighting
" \   's' : [':call Swoop()', 'fuzzy search in this file'],
" \   's' : ['/', 'fuzzy search in this file'],
let g:which_key_map.s = {'name' : '+search/select/spell/symbol',
\   '/' : [':let @/ = @+', 'search for text in clipboard'],
\   'c' : [':let @/ = ""', 'clear search (no highlight)'],
\   'j' : [':call My_grep_recursive()', 'ripgrep in last used dir'],
\   'l' : {'name' : '+spellang',
\     'd' : [':setlocal spell spelllang=de', 'deutsch'],
\     'e' : [':setlocal spell spelllang=en', 'english'],
\     'n' : [':setlocal nospell', 'nospell (disable)'],
\     'r' : [':setlocal spell spelllang=ru', 'russian'],
\    },
\   'm' : [':call My_vifm_choose("ripgrep")', 'ripgrep in dir to be selected'],
\   'p' : [':call My_select_project()', 'select project'],
\   's' : [':let g:My_use_denite_errors = 1 | Denite -start-filter line', 'fuzzy search in this file'],
\ }
" Put in $DOTRC_S/home_settings/.vimrc:
" let g:which_key_map.s.P = [
"             \ ':let g:My_eval_var = "MyRunShellCmd make -C /tmp"'
"             \ , 'my single project']

let g:which_key_map.w = { 'name' : '+windows/whitespace',
\   '-' : [':split', 'split horizontally'],
\   'a' : [':wa', 'save all files'],
\   'd' : [':%s/\s\+$//e', 'delete whitespaces at the end of lines'],
\   'h' : [':wincmd h', 'focus left'],
\   'j' : [':wincmd j', 'focus bottom'],
\   'k' : [':wincmd k', 'focus top'],
\   'l' : [':wincmd l', 'focus right'],
\   'p' : [':wincmd p', 'focus previous'],
\   'v' : [':vsplit', 'split vertically'],
\ }

" let g:which_key_map.q = { 'name' : '+quit',
" \ }

function! My_register_which_key(var, keypresses)
    try
        for [l:key, l:value] in items(a:var)
            if l:key == 'name'
                continue
            endif
            call My_register_which_key(l:value, a:keypresses . l:key)
        endfor
    catch
        let l:cmd = ':nnoremap <leader>' . a:keypresses . ' '
        let l:cmd = l:cmd . substitute(a:var[0], ' | ', '<CR>:', 'g')
        if a:var[0][0] == ':'
            let l:cmd = l:cmd . '<CR>'
        endif
        execute l:cmd
    endtry
endfunction

if globpath(&runtimepath, "autoload/which_key.vim") != ""
    call which_key#register('<Space>', "g:which_key_map")
    nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
    vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
else
    call My_register_which_key(g:which_key_map, '')
endif
" LABEL_1: remap <Leader>oc again (fixes error due to double quote, see above):
nmap <Leader>oc 0"zD

function! My_source_vimrc_s()
    let l:file = $DOTRC_S . "/home_settings/.vimrc"
    if filereadable(l:file)
        execute 'source ' . l:file
    endif
endfunction
call My_source_vimrc_s()

" :help CTRL-W_ge
