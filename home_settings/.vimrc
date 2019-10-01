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
" Toggle language: <C-K>
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
" let g:swoopUseDefaultKeyMap = 0 " we use denite instead

let g:my_user_id = system('id -u')

" Initialize pathogen plugin (update runtimepath variable):
execute pathogen#infect()

if globpath(&runtimepath, "autoload/denite.vim") != ""
    " Use ripgrep:
    call denite#custom#var('file/rec', 'command', ['rg', '--files', '--glob', '!.git'])
    call denite#custom#var('grep', 'command', ['rg'])
    call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep', '--no-heading'])
    call denite#custom#var('grep', 'recursive_opts', [])
    call denite#custom#var('grep', 'pattern_opt', ['--regexp'])
    call denite#custom#var('grep', 'separator', ['--'])
    call denite#custom#var('grep', 'final_opts', [])
endif

autocmd FileType denite call My_denite_settings()
function! My_denite_settings() abort
    nnoremap <silent><buffer><expr> <CR> denite#do_map('do_action')
    nnoremap <silent><buffer><expr> d denite#do_map('do_action', 'delete')
    nnoremap <silent><buffer><expr> p denite#do_map('do_action', 'preview')
    nnoremap <silent><buffer><expr> q denite#do_map('quit')
    nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
    " nnoremap <silent><buffer><expr> t denite#do_map('toggle_select')
    " nnoremap <silent><buffer><expr> <Space> denite#do_map('toggle_select').'j'
endfunction

" function! s:my_denite_split(context)
"     let split_action = 'vsplit'
"     if winwidth(winnr('#')) <= 2 * (&tw ? &tw : 80)
"         let split_action = 'split'
"     endif
"     execute split_action . ' ' . a:context['targets'][0].action__path
" endfunction
" call denite#custom#action('file', 'my_split', function('s:my_denite_split'))

colorscheme molokai
" Fix colorscheme:
hi Search ctermfg=0 ctermbg=12
hi DiffAdd ctermbg=234
hi DiffDelete ctermbg=16
hi StatusLine ctermfg=232 ctermbg=46
hi StatusLineNC ctermfg=232 ctermbg=252
" Highlight spaces and tabs in the end of the line as errors (trailing whitespaces):
match Error /\s\+$/
" commented-out because it highlights spaces in "which-key" (leader) window:
" autocmd WinEnter * match Error /\s\+$/
" Highlight column (right after last that can be used):
set colorcolumn=81
hi ColorColumn ctermbg=234

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
set clipboard=unnamedplus " map clipboard to unnamedplus register '+'
" Preserve copied text in clipboard on exit:
autocmd VimLeave * call system("clipboard.sh", getreg('+'))
set diffopt+=vertical " need for "fugitive-:Gdiff"
" set lines=25
" set columns=83
" set langmenu=ru_RU.UTF-8

" Set maximum number of tab pages to be opened by the "-p" command line argument:
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
autocmd BufEnter vifmrc setlocal filetype=vifm
autocmd BufEnter *vifm/colors/* setlocal filetype=vifm
autocmd BufEnter *.i setlocal filetype=c
autocmd BufEnter *.ii setlocal filetype=cpp
autocmd BufEnter *.lds,*.lds.S,*.lds.h setlocal filetype=ld
autocmd BufEnter .spacemacs setlocal filetype=lisp
autocmd BufEnter *.gdb setlocal filetype=gdb " my filetype extension
autocmd BufEnter *.cmm setlocal filetype=jtag_script " my filetype
autocmd BufEnter menurc setlocal filetype=claws_mail_menurc " my filetype
" autocmd BufEnter *.gl setlocal filetype=glanguage " my filetype
autocmd BufEnter * if &filetype == "" | setlocal filetype=unknown | endif

" Set correct syntax:
autocmd FileType asm setlocal syntax=armasm

" Tab settings:
let g:detectindent_preferred_expandtab = 1
let g:detectindent_preferred_indent = 4
let g:detectindent_preferred_when_mixed = 1
let g:detectindent_max_lines_to_analyse = 1024
autocmd FileType rust,c,cpp,sh,expect,cmake,vim,python,perl,lua,php,json
\ call My_apply_tab_settings()
function! My_apply_tab_settings()
    if g:my_user_id != 0
        DetectIndent
    endif
    if My_is_linux_kernel_source_file(expand("%:p"))
        setlocal tabstop=8
    else
        setlocal tabstop=4
    endif
    if &filetype == "vim"
        setlocal shiftwidth=4
    endif
endfunction
" autocmd FileType rust,cpp,sh,expect,cmake,vim,python,perl,lua,php
" \ setlocal shiftwidth=0 | setlocal tabstop=4 | setlocal expandtab
" autocmd FileType c,cpp setlocal shiftwidth=0
" \ | if My_is_linux_kernel_source_file(expand("%:p"))
" \     | setlocal tabstop=8 | setlocal noexpandtab
" \ | else
" \     | setlocal tabstop=4 | setlocal expandtab
" \ | endif
" autocmd BufNewFile,BufRead,BufEnter */dotrc/* setlocal expandtab

" Auto insert <EOL> and move last word to next line if it reaches 81 column
autocmd FileType c,cpp setlocal textwidth=80 | setlocal formatoptions+=t
" autocmd FileType c,cpp setlocal cindent | setlocal noautoindent

autocmd FileType vim if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "execute 'call ' . My_vimscript_function_eval() . '()'" | endif

autocmd FileType sh,python,perl if ! exists('g:My_eval_var')
    \ | let g:My_eval_var = "silent wa | MyRunShellCmd chmod +x ./"
    \ . expand("%:t") . " && ./" . expand("%:t") | endif

autocmd FileType make if ! exists('g:My_eval_var')
    \ | let g:My_eval_var = "silent wa | MyRunShellCmd make -f ./"
    \ . expand("%:t") | endif

autocmd FileType c if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "silent wa | MyRunShellCmd clang -g3 -Weverything -pedantic "
    \ . expand("%:t") . " -o /tmp/" . expand("%:t") . ".out && /tmp/"
    \ . expand("%:t") . ".out" | endif

autocmd FileType cpp if ! exists('g:My_eval_var') | let g:My_eval_var =
    \ "silent wa | MyRunShellCmd clang++ -g3 -Weverything -pedantic "
    \ . expand("%:t") . " -o /tmp/" . expand("%:t") . ".out && /tmp/"
    \ . expand("%:t") . ".out" | endif

function! My_is_linux_kernel_source_file(full_path)
    let l:idx_linux = stridx(a:full_path, "linux")
    if l:idx_linux == -1
        return 0
    endif
    let l:idx_slash = stridx(a:full_path, "/", l:idx_linux)
    let l:linux_root_dir = strpart(a:full_path, 0, l:idx_slash) . "/"
    let l:subdirs = ['arch', 'crypto', 'drivers', 'fs', 'init', 'ipc', 'kernel', 'mm', 'net', 'security', 'sound']
    for l:subdir in l:subdirs
        if ! isdirectory(l:linux_root_dir . l:subdir)
            return 0
        endif
    endfor
    return 1
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
            let l:dir = join(readfile(self.chosen))
            " -default-action=my_split -post-action=suspend
            let g:My_use_denite_errors = 1
            execute 'Denite -start-filter -path=' . l:dir . ' grep:::!'
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

let g:My_vim_errors_file = '/tmp/vim_errors' . tr(bufname('%'), '/', '_') . '.err'

function! My_run_shell_cmd(cmd)
    silent! execute 'noautocmd silent botright pedit ' . g:My_vim_errors_file
    noautocmd wincmd P
    " set buftype=nofile
    setlocal filetype=my_shell_cmd_output
    set noreadonly
    normal ggdG
    " exe 'noautocmd r! $DOTRC/other_files/vifm_run_command.sh ' . a:cmd . ' 2>&1'
    " AnsiEsc
    execute 'noautocmd silent r!
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
    normal 0ggdd
    silent w
    set readonly
    execute 'cgetfile ' . g:My_vim_errors_file
    let g:My_use_denite_errors = 0
    wincmd p
    if v:shell_error == 0
        echo "Command succeeded (exit code = 0)"
    else
        echo "Command failed (exit code = " . v:shell_error . ")"
    endif
endfunction
command! -nargs=1 MyRunShellCmd :call My_run_shell_cmd('<args>')

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

function! My_swap_keyboard_layout()
    if &iminsert == 1 " If current layout is russian
        " then switch to english
        set iminsert=0
        set imsearch=0
    else " if current layout is english
        " then switch to russian
        set iminsert=1
        set imsearch=1
    endif
    call My_update_status_line('', 'normal')
endfunction

function! My_insert_snippet()
    if &filetype == "c" || &filetype == "cpp"
        let l:ext = expand('%:e')
        if l:ext == 'h' || l:ext == 'hpp'
            let l:file_name = expand('%:t')
            let l:guard_name = tr(toupper(l:file_name), '.', '_')
            call append(0, ['#ifndef ' . l:guard_name, '#define ' . l:guard_name])
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
            normal dd2k
        endif
    elseif &filetype == "sh"
        call append(0, [
\ "#!/bin/bash",
\ "",
\ "echo \"\\$# = |$#|\"; echo \"\\$0 = |$0|\"; echo \"\\$@ = |$@|\"",
\ ])
        normal dd
    elseif &filetype == "python"
        call append(0, [
\ "#!/usr/bin/python3",
\ "",
\ "import sys; print(sys.argv)",
\ ])
        normal dd
    endif
endfunction

function! My_window_is_temporary()
    let l:dir_name = expand('%:p:h')
    let l:file_name = expand('%:t')
    if l:dir_name == '/usr/share/vim/vim74/doc' || &l:buftype == 'help' || &l:buftype == 'quickfix' || &l:buftype == 'nofile' || l:file_name == 'swoopBuf'
        " || l:file_name == 'search-results.agsv'
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
function! My_close_window_if_temporary()
    if My_window_is_temporary() != 0
        execute ':q!'
    endif
endfunction
command! MyCloseWindowIfTemporary call My_close_window_if_temporary()

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

function! My_copy_location(in_file, strip_part)
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

" vim-commentary:
autocmd FileType unknown setlocal commentstring=#\ %s

if exists('loaded_tcomment')
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
    " Was not used:
    " call tcomment#type#Define('c_block', g:tcommentBlockC2)
    " call tcomment#type#Define('cpp_block', g:tcommentBlockC2)
endif

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
" sp | wincmd j | e term://f s | set norelativenumber | set nonumber | startinsert

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

function! My_get_eval_first_last_lines()
    let l:lines_range = My_get_region_lines('EVAL REGION BEGINS HERE: |', 'EVAL REGION ENDS HERE.')
    return [l:lines_range[0] + 1, l:lines_range[1] - 1]
endfunction

function! My_get_prefix(first_line_num)
    let l:first_line = join(getbufline('%', a:first_line_num - 1), "\n")
    let l:start = stridx(l:first_line, '|')
    let l:end = stridx(l:first_line, '|', l:start + 1)
    return strpart(l:first_line, l:start + 1, l:end - l:start - 1)
endfunction

function! My_eval_vim()
    try
        let l:lines_range = My_get_eval_first_last_lines()
        let l:prefix = My_get_prefix(l:lines_range[0])
        let l:pattern = '\s*' . escape(l:prefix, '*')
        let l:full_lines_list = getbufline('%', l:lines_range[0], l:lines_range[1])
        let l:lines_list = []
        for l:line in l:full_lines_list
            " strip filetype comments:
            let l:prefix_len = strlen(matchstr(l:line, l:pattern))
            if l:prefix_len != 0
                let l:line = strpart(l:line, l:prefix_len)
            endif
            " ignore vim comments:
            let l:idx = match(l:line, '\s*" ')
            if l:idx != -1
                continue
            endif
            let l:lines_list = add(l:lines_list, l:line)
        endfor
        let l:text = join(l:lines_list, "\n")
    catch
        echo v:exception
        return
    endtry
    " echo l:text
    execute l:text
endfunction

function! My_insert_eval_region(text)
    let l:formatoptions = &formatoptions
    set formatoptions-=t
    set formatoptions-=c
    let l:text_prefix = ''
    if &filetype == "c" || &filetype == "cpp"
        normal O/* EVAL REGION BEGINS HERE: |* |EVAL REGION ENDS HERE. */2k
        let l:text_prefix = ' '
    elseif &filetype == "sh" || &filetype == "python"
        normal O# EVAL REGION BEGINS HERE: |# |# # EVAL REGION ENDS HERE.2k
    elseif &filetype == "vim"
        normal O$d0xi" EVAL REGION BEGINS HERE: |" |EVAL REGION ENDS HERE.$d0x2k
    else
        normal OEVAL REGION BEGINS HERE: ||EVAL REGION ENDS HERE.2k
    endif
    " call append(line('.'), l:text_prefix . a:text)
    execute ':normal! A' . l:text_prefix . a:text
    execute 'set formatoptions=' . l:formatoptions
    if a:text == ''
        startinsert!
    endif
endfunction

function! My_insert_eval_variable()
    let l:text = 'let g:My_eval_var = "' . g:My_eval_var . '"'
    call My_insert_eval_region(l:text)
endfunction

function! My_vimscript_function_eval()
    try
        let l:lines_range = My_get_region_lines('^function', '^endfunction$')
        let l:lines_list = getbufline('%', l:lines_range[0], l:lines_range[1])
        let l:function_body = join(l:lines_list, "\n")
        let l:function_name = matchstr(l:lines_list[0], ' [^ (]*')[1:]
    catch
        echo v:exception
        return
    endtry
    execute l:function_body
    return l:function_name
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
        execute 'Denite -resume -immediately ' . l:cmd
        return
    endif

    if len(getloclist(0)) != 0
        let l:prefix = 'l'
    else
        let l:prefix = 'p'
    endif
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
    let l:file = $DOTRC . "/home_settings/.vimrc"
    execute 'tabedit ' . l:file
    function! My_done_editing_vimrc(file)
        silent execute "!source " . $DOTRC . "/other_files/config_file.sh && config_generate -h .vimrc"
        source $MYVIMRC
    endfunction
    execute 'autocmd! BufLeave ' . l:file . ' call My_done_editing_vimrc("' . l:file . '")'
endfunction

" To insert echo (for Makefile) use the following macro:
" let @E = 'i	@echo "|$()|"hhi'

" Fugitive (git):
" :copen - open quickfix window
" In status window use:
" g?    show help
" "-" to "git add" or "git reset" file (depending on where your cursor is).
" <c-n> to go to next file, <c-p> to go to previous file.
" <Enter> to open current file in window below.
" ca    :Gcommit --amend
" nmap <Leader>gdn :q<CR>:q<CR>:exec ':Gdiff ' . g:Gdiff_arg<CR> " diff next file
" nmap <Leader>gw :Gwrite<CR><C-w>k
" nmap <Leader>gr :Gread<CR>:w<CR><C-w>k<C-n>
" nmap <Leader>ga <C-w>k-

" vimagit:
let g:magit_default_fold_level=2
let g:magit_discard_untracked_do_delete=1
let g:magit_discard_hunk_mapping='X'

nmap <C-;> :call My_swap_keyboard_layout()<CR>
vmap <C-;> <Esc>:call My_swap_keyboard_layout()<CR>gv
imap <C-;> <Esc>:call My_swap_keyboard_layout()<CR>gi
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
" Open location in QuickFix window:
autocmd FileType qf nmap <buffer> o :call My_open_location()<CR>
" Reformat C/C++ source code:
" nmap <C-u> :%d<CR>:r !uncrustify -f %<CR>:1,1d<CR>
" nmap <C-u> :%d<CR>:r !astyle.sh %<CR>

" which-key (hotkeys):
set timeoutlen=200
let g:which_key_map =  {}

let g:which_key_map.b = { 'name' : '+buffers',
\   'b' : [':Denite buffer', 'select'],
\   'n' : [':bnext', 'next'],
\   'p' : [':bprevious', 'previous'],
\ }

" nmap <F11> :call My_copy_location( expand('%:p'), '/home/slava/workspace/project_root_dir/' )<CR>
let g:which_key_map.c = { 'name' : '+compile/clipboard',
\   'l' : [':call My_copy_location(expand("%:p"), "")', 'copy full location to clipboard'],
\ }

let g:which_key_map.d = {'name' : '+diff',
\   'j' : [':Denite -resume', 'resume denite'],
\   'k' : [':bdelete [denite]-default', 'kill denite'],
\   'm' : [':MyModifyLine', 'modify line'],
\   'q' : [':windo MyCloseWindowIfTemporary', 'close temporary windows'],
\   's' : [':%s/\s\+$//e', 'delete whitespaces at the end of lines'],
\   't' : [':resize +1000 | vertical resize +1000', 'show this panel only, hide another one'],
\   'u' : [':diffupdate', 'diffupdate (recalculate diff)'],
\   '=' : ['<c-w>=', 'restore diff panels (after "t")'],
\ }

let g:which_key_map.e = {'name' : '+errors',
\   'c' : [':call My_goto_error("current")', 'current error'],
\   'l' : [':lopen', 'location list errors'],
\   'm' : [':botright pedit ' . g:My_vim_errors_file . ' | set readonly', 'my list errors'],
\   'n' : [':call My_goto_error("next")', 'next error'],
\   'p' : [':call My_goto_error("previous")', 'previous error'],
\   'q' : [':copen', 'quickfix list errors'],
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
let g:which_key_map.g = {'name' : '+git',
\   's' : [':wa | Magit', 'status'],
\   'u' : [':e', 'update (reload) buffer'],
\   'b' : [':Gblame', 'blame'],
\   'c' : {'name' : '+commit',
\     'o' : [':Gcommit', 'default'],
\     'a' : [':Gcommit --amend', 'amend'],
\   },
\   'd' : {'name' : '+diff',
\     'i' : [':Gdiff', 'index'],
\     'h' : [':Gdiff HEAD', 'HEAD'],
\   },
\   'e' : [':MyRunShellCmd git show --name-only --pretty= HEAD', 'show files in HEAD'],
\ }

let g:which_key_map.i = { 'name' : '+insert',
\   's' : [':call My_insert_snippet()', 'snippet'],
\   'e' : [':call My_insert_eval_region("")', 'insert eval region'],
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

" let g:LanguageClient_serverCommands = { 'c': ['clangd', '-compile-commands-dir=your_dir'], }
" let g:LanguageClient_serverCommands = { 'c': ['/media/files/workspace/ccls/Release/ccls'], }
" silent UpdateRemotePlugins
" LanguageClientStart

let g:which_key_map.m = { 'name' : '+my',
\   'e' : [':call My_eval_vim()', 'eval vim'],
\ }

let g:which_key_map.o = { 'name' : '+other',
\   'c' : ['$d0x', 'clear current line'],
\   'f' : [':execute g:My_eval_var', 'evaluate variable'],
\ }

let g:which_key_map.r = { 'name' : '+rtags',
\ }

" nmap <F3> :set hlsearch!<CR> " set/unset search highlighting
" \   's' : [':call Swoop()', 'fuzzy search in this file'],
" \   's' : ['/', 'fuzzy search in this file'],
let g:which_key_map.s = {'name' : '+search/spell/symbol',
\   '/' : [':let @/ = @+', 'search for text in clipboard'],
\   'c' : [':let @/ = ""', 'clear search (no highlight)'],
\   's' : [':Denite -start-filter line', 'fuzzy search in this file'],
\   'm' : [':call My_vifm_choose("ripgrep")', 'ripgrep'],
\   'l' : {'name' : '+spellang',
\     'e' : [':setlocal spell spelllang=en', 'english'],
\     'd' : [':setlocal spell spelllang=de', 'deutsch'],
\     'r' : [':setlocal spell spelllang=ru', 'russian'],
\     'n' : [':setlocal nospell', 'nospell (disable)'],
\    },
\ }

let g:which_key_map.w = { 'name' : '+windows',
\   '-' : [':split', 'split horizontally'],
\   'v' : [':vsplit', 'split vertically'],
\   'h' : [':wincmd h', 'focus left'],
\   'l' : [':wincmd l', 'focus right'],
\   'k' : [':wincmd k', 'focus top'],
\   'j' : [':wincmd j', 'focus bottom'],
\   'p' : [':wincmd p', 'focus previous'],
\ }

" let g:which_key_map.q = { 'name' : '+quit',
" \ }

if globpath(&runtimepath, "autoload/which_key.vim") != ""
    call which_key#register('<Space>', "g:which_key_map")
    nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<CR>
    vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<CR>
endif

" :help CTRL-W_ge
