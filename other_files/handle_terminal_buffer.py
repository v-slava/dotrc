#!/usr/bin/python3

import re
import os
import sys
import unittest
import subprocess
from enum import Enum, auto

def parse_last_cmd(visible_lines):
    import socket
    import getpass
    user_name = getpass.getuser()
    host_name = socket.gethostname()
    separator = '#' if (user_name == 'root') else '\$'
    pattern = f'^{user_name}@{host_name} (.+) {separator} (.*)$'
    cmd_re = re.compile(pattern)
    last_line = visible_lines[len(visible_lines) - 1]
    match = cmd_re.match(last_line)
    assert match
    bash_cwd = os.path.expanduser(match.group(1))
    cmd_lines = []
    cmd = ''
    for line in reversed(visible_lines[:-1]):
        match = cmd_re.match(line)
        if match:
            cmd = match.group(2).strip()
            break
        cmd_lines.insert(0, line)
    return cmd, cmd_lines, bash_cwd

def remove_dots(path_list):
    if '.' in path_list:
        path_list.remove('.')
    if '..' in path_list:
        path_list.remove('..')
    return path_list

def remove_not_existed(path_list):
    ret = []
    for path in path_list:
        full_path = os.path.join(bash_cwd, path)
        if os.path.exists(path) or os.path.exists(full_path):
            ret.append(path)
    return ret

def get_path_list___git_status(lines):
    file_re = re.compile('^\t.*(modified|deleted):   (.+)$')
    class State(Enum):
        FIRST = auto()
        EMPTY = auto()
        STAGED_START = auto()
        NEXT_TRACKED_EMPTY = auto()
        NEXT_UNTRACKED_EMPTY = auto()
        TRACKED_EMPTY = auto()
        UNSTAGED_START = auto()
        UNSTAGED_COMMENT_ADD = auto()
        UNTRACKED_START = auto()
        UNTRACKED_COMMENT_ADD = auto()
        UNTRACKED_EMPTY = auto()
        TRACKED = auto()
        UNTRACKED = auto()
    state = State.FIRST
    files_list = []
    def process___State_FIRST():
        return State.EMPTY if line.strip() == '' else state
    def process___State_EMPTY():
        return {
                'Changes to be committed:' : State.STAGED_START,
                'Changes not staged for commit:' : State.UNSTAGED_START,
                'Untracked files:' : State.UNTRACKED_START,
                'no changes added to commit (use "git add" and/or "git commit -a")' : State.EMPTY,
                } [line.rstrip()]
    def process___State_STAGED_START():
        assert '  (use "git reset HEAD <file>..." to unstage)' == line.rstrip()
        return State.NEXT_TRACKED_EMPTY
    def process___State_NEXT_TRACKED_EMPTY():
        assert '' == line.rstrip()
        return State.TRACKED_EMPTY
    def process___State_TRACKED_EMPTY():
        match = file_re.match(line)
        assert match
        files_list.append(match.group(2).rstrip())
        return State.TRACKED
    def process___State_TRACKED():
        match = file_re.match(line)
        if match:
            files_list.append(match.group(2).rstrip())
        else:
            assert '' == line.rstrip()
        return State.TRACKED if match else State.EMPTY
    def process___State_UNSTAGED_START():
        assert '  (use "git add <file>..." to update what will be committed)' \
                == line.rstrip()
        return State.UNSTAGED_COMMENT_ADD
    def process___State_UNSTAGED_COMMENT_ADD():
        assert '  (use "git checkout -- <file>..." to discard changes in ' + \
                'working directory)' == line.rstrip()
        return State.NEXT_TRACKED_EMPTY
    def process___State_UNTRACKED_START():
        assert '  (use "git add <file>..." to include in what will be ' + \
                'committed)' == line.rstrip()
        return State.NEXT_UNTRACKED_EMPTY
    def process___State_NEXT_UNTRACKED_EMPTY():
        assert '' == line.rstrip()
        return State.UNTRACKED_EMPTY
    def process___State_UNTRACKED_EMPTY():
        file_path = line.strip(' \t\uffff')
        assert '' != file_path
        files_list.append(file_path)
        return State.UNTRACKED
    def process___State_UNTRACKED():
        file_path = line.strip(' \t\uffff')
        if file_path:
            files_list.append(file_path)
        return State.TRACKED if file_path else State.EMPTY
    for line in lines:
        state = eval('process___' + str(state).replace('.', '_') + '()')
    return files_list

def get_path_list___find(lines):
    return remove_dots([line.rstrip() for line in lines])

def get_path_list___ls_l(lines):
    ret = []
    for line in lines[1:]:
        l = line.rstrip()[::-1]
        if l[0] == "'":
            rev_path = l[1:].split("'", 1)[0]
        else:
            rev_path = l.split(None, 1)[0]
        ret.append(rev_path[::-1])
    return remove_dots(ret)

def get_path_list___ls(lines):
    end = ' '
    begin = True
    ret = []
    for line in lines:
        for sym in line:
            if begin:
                if sym.isspace():
                    continue
                begin = False
                if sym == "'":
                    end = sym
                    elem = ''
                else:
                    end = None
                    elem = sym
            else:
                if end:
                    if sym != end:
                        elem += sym
                        continue
                else:
                    if not sym.isspace():
                        elem += sym
                        continue
                ret.append(elem)
                begin = True
    return remove_dots(ret)

def get_path_list___unknown(lines):
    ret = []
    words = ' '.join(lines).split()
    for word in words:
        if word.startswith('//'):
            continue
        if os.path.exists(os.path.join(bash_cwd, word)):
            ret.append(word)
            continue
        if not ':' in word:
            continue
        split = word.split(':', 3)
        if not os.path.exists(os.path.join(bash_cwd, split[0])):
            continue
        path = split[0]
        try:
           val = int(split[1])
           path += f':{split[1]}'
        except ValueError:
            ret.append(path)
            continue
        if len(split) < 3:
            continue
        try:
           val = int(split[2])
           path += f':{split[2]}'
        except ValueError:
            pass
        ret.append(path)
    return list(set(ret))

def get_func(cmd):
    if not cmd:
        return get_path_list___unknown
    def regex_matches(regex):
        for r in regex:
            cmd_r = f'^[ ]*{r}[ ]*$'
            if re.match(cmd_r, cmd):
                return True
        return False
    if regex_matches(['git[ ]+status', 'gs']):
        return get_path_list___git_status
    elif regex_matches(['find.*']):
        return get_path_list___find
    elif regex_matches(['l[sa][ ]+.*l.*', 'll.*']):
        return get_path_list___ls_l
    elif regex_matches(['l[sa].*']):
        return get_path_list___ls
    return get_path_list___unknown

def copy_to_clipboard(path):
    import shutil
    if shutil.which('xclip'):
        cmd = ['xclip', '-selection', 'clipboard']
    elif shutil.which('xsel'):
        cmd = ['xsel', '--clipboard', '-i']
    else:
        raise Exception('Neither "xclip" nor "xsel" is installed')
    subprocess.run(cmd, check = True, input = path, encoding = 'utf8')

def fuzzy_select_path(path_list):
    import tempfile
    with tempfile.NamedTemporaryFile(mode = 'w', encoding = 'utf8') as o_f:
        o_f.write('\n'.join(path_list))
        o_f.flush()
        with tempfile.NamedTemporaryFile(mode = 'r', encoding = 'utf8') as i_f:
            ofn = o_f.name
            ifn = i_f.name
            subprocess.run(['x-terminal-emulator', '-title', 'temp_window',
                '-e', 'bash', '-c', f'fzf > {ifn} < {ofn}'])
            return i_f.read().rstrip()

def do_main():
    visible_lines = [line.rstrip('\n') for line in sys.stdin.readlines()]
    global bash_cwd
    cmd, cmd_lines, bash_cwd = parse_last_cmd(visible_lines)
    get_path_list = get_func(cmd)
    path_list = get_path_list(cmd_lines)
    if not path_list:
        raise Exception("Didn't find any path in last command's output")
    path = path_list[0] if len(path_list) == 1 else fuzzy_select_path(path_list)
    path = path if len(path.split()) == 1 else f"'{path}'"
    copy_to_clipboard(path)

class MyUnitTests(unittest.TestCase):
    def test___parse_last_cmd(self):
        expected_cmd = 'git status'
        expected_lines = [
            'line 1',
            'line2',
                ]
        visible_lines = [
            'slava@laptop /media/files/workspace/dotrc $ gs   ',
            '',
            'slava@laptop workspace/dotrc $   ' + expected_cmd + '      ',
                ] + expected_lines + [
            'slava@laptop ~/h $ '
                ]
        cmd, cmd_lines, bash_cwd = parse_last_cmd(visible_lines)
        self.assertEqual(expected_cmd, cmd)
        self.assertEqual(expected_lines, cmd_lines)
        self.assertEqual('/home/slava/my', bash_cwd)

    def test___get_func(self):
        self.assertEqual(get_path_list___git_status, get_func('git  status'))
        self.assertEqual(get_path_list___git_status, get_func(' gs  '))
        self.assertEqual(get_path_list___find, get_func(' find -name "*.c"  '))
        self.assertEqual(get_path_list___ls_l, get_func(' ls  -l   '))
        self.assertEqual(get_path_list___ls_l, get_func('ls -a -l '))
        self.assertEqual(get_path_list___ls_l, get_func('ll'))
        self.assertEqual(get_path_list___ls_l, get_func('ll -a'))
        self.assertEqual(get_path_list___ls, get_func('ls'))
        self.assertEqual(get_path_list___ls, get_func('la'))
        self.assertEqual(get_path_list___unknown, get_func('unknown command'))

    def test___git_status_1(self):
        cmd_lines = [
'On branch master                                                             ',
'Your branch is up to date with \'origin/master\'.                            ',
'                                                                             ',
'Changes to be committed:                                                     ',
'  (use "git reset HEAD <file>..." to unstage)                                ',
'                                                                             ',
'	￿modified:   home_settings/.Xresources                             ',
'                                                                             ',
'Changes not staged for commit:                                               ',
'  (use "git add <file>..." to update what will be committed)                 ',
'  (use "git checkout -- <file>..." to discard changes in working directory)  ',
'                                                                             ',
'	￿deleted:   home_settings/.X resources                             ',
'	￿￿modified:   other_files/handle terminal_buffer.py         ',
'                                                                             ',
'Untracked files:                                                             ',
'  (use "git add <file>..." to include in what will be committed)             ',
'                                                                             ',
'	￿￿￿home_settings/asdf qwer                           ',
'                                                                             ',
]
        actual_files_list = get_path_list___git_status(cmd_lines)
        expected_files_list = [
                'home_settings/.Xresources',
                'home_settings/.X resources',
                'other_files/handle terminal_buffer.py',
                'home_settings/asdf qwer',
                ]
        self.assertEqual(expected_files_list, actual_files_list)

    def test___git_status_2(self):
        cmd_lines = [
'On branch master                                                             ',
'Your branch is ahead of \'origin/master\' by 1 commit.                       ',
'  (use "git push" to publish your local commits)                             ',
'                                                                             ',
'Changes not staged for commit:                                               ',
'  (use "git add <file>..." to update what will be committed)                 ',
'  (use "git checkout -- <file>..." to discard changes in working directory)  ',
'                                                                             ',
'	￿￿￿￿￿modified:   README.md             ',
'	￿￿modified:   other_files/handle_terminal_buffer.py         ',
'                                                                             ',
'no changes added to commit (use "git add" and/or "git commit -a")            ',
]
        actual_files_list = get_path_list___git_status(cmd_lines)
        expected_files_list = [
                'README.md',
                'other_files/handle_terminal_buffer.py',
                ]
        self.assertEqual(expected_files_list, actual_files_list)

    def test___ls(self):
        cmd_lines = [
"misc_info other                                                              ",
"'Telegram Desktop'   'part1   part2'   asdf.pdf                              ",
]
        actual_path_list = get_path_list___ls(cmd_lines)
        expected_path_list = [
                'misc_info',
                'other',
                'Telegram Desktop',
                'part1   part2',
                'asdf.pdf',
                ]
        self.assertEqual(expected_path_list, actual_path_list)

    def test___ls_l_1(self):
        cmd_lines = [
"total 36                                                                     ",
"drwxr-xr-x  8 slava slava  4096 Mar  4 01:13 .                               ",
"-rw-r--r--  1 slava slava     0 Mar  3 20:46 'part1   part2'                 ",
"drwxr-xr-x 10 slava slava  4096 Feb 20 04:18 ..                              ",
"-rw-------  1 slava slava 33211 Feb 28 20:30  asdf.pdf                       ",
]
        actual_path_list = get_path_list___ls_l(cmd_lines)
        expected_path_list = [
                'part1   part2',
                'asdf.pdf',
                ]
        self.assertEqual(expected_path_list, actual_path_list)

    def test___ls_l_2(self):
        cmd_lines = [
"total 0                                                                      ",
]
        actual_path_list = get_path_list___ls_l(cmd_lines)
        expected_path_list = []
        self.assertEqual(expected_path_list, actual_path_list)

def main():
    if (len(sys.argv) == 2) and (sys.argv[1] == 'copy_path_to_clipboard'):
        try:
            do_main()
        except Exception as e:
            import traceback
            text = 'Exception: ' + str(e) + '\n\n'
            text = text + ''.join(traceback.format_tb(e.__traceback__))
            subprocess.run(['zenity', '--error', '--no-wrap', '--title',
                str(sys.argv), '--text', text])
            raise
    else:
        unittest.main()

if __name__ == '__main__':
    main()
