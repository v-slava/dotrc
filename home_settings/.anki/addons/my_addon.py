# fill              = Ctrl+f
# clear             = Ctrl+Alt+c
# submit card       = Ctrl+j

# # Prefixes german (maskulinum, femininum, neutrum):
# add "der " prefix = Ctrl+Alt+m
# add "die " prefix = Ctrl+Alt+f
# add "das " prefix = Ctrl+Alt+n

# # Prefixes for english:
# add "to "  prefix = Ctrl+Alt+t
# add "a "   prefix = Ctrl+Alt+a
# add "an "  prefix = Alt+Shift+a

from anki.hooks import wrap
from aqt.editor import Editor
from aqt.utils import showInfo
from aqt.qt import QKeySequence
from anki.utils import json

from shutil import rmtree
from tempfile import NamedTemporaryFile
import os

# from subprocess import *
from subprocess import call,check_call

from aqt import addcards
from aqt.utils import tooltip
from anki.sound import clearAudioQueue

english = "0"
# russian = "1"
russian = "0"
def set_keyboard_layout(layout):
    # Switch keyboard layout to english:
    check_call(["/media/files/programs/keyboard_layout", "--set_layout", layout])

def refresh_all_fields(self, data, cursor_position):
    self.web.eval("setFields(%s, %d);" % (json.dumps(data), cursor_position))
    self.web.eval("setFonts(%s);" % (json.dumps(self.fonts())))

def fill_button_pressed(self):
    # Settings:
    HOME = "/home/slava"
    SOUNDS = HOME + "/other/GoldenDict/sound_en/sound_en.dsl.files.zip"
    MEDIA = HOME + "/.anki/slava/collection.media"
    # Switch i3-wm workspace to goldendict, browser:
    check_call(["i3-msg", "workspace 2"])
    # Read english word:
    self.web.eval("focusField(%d);" % 1)
    self.web.eval("focusField(%d);" % 0)
    english_word = self.note.fields[0]
    # Translate english word:
    check_call(["goldendict", english_word])
    # Copy english word to newly created data:
    data = []
    data.append(("english", english_word))
    self.note.fields[0] = english_word
    data.append(("russian", ""))
    self.note.fields[1] = ""
    data.append(("usage_example", ""))
    self.note.fields[2] = ""
    # No audio for german:
    data.append(("audio", ""))
    self.note.fields[3] = ""
    '''
    # Add audio if available:
    audio_file = MEDIA + "/" + english_word + ".mp3"
    if os.path.isfile(audio_file):
        ret = 0
    else:
        ret = call(["unzip", SOUNDS, english_word + ".mp3", "-d", MEDIA])
    if ret == 0:
        audio_field_content = "[sound:" + english_word + ".mp3]"
        check_call(["mplayer", "--volume=70", audio_file])
    else:
        audio_field_content = ""
        showInfo("No audio found")
    data.append(("audio", audio_field_content))
    self.note.fields[3] = audio_field_content
    '''
    # Add empty image:
    data.append(("image", self.note.fields[4]))
    refresh_all_fields(self, data, 4) # 4 = field to place cursor to (image)
    # spawn browser to look for images:
    # check_call(["wget", "--user-agent=Mozilla/5.0", "-O", IMAGE_WEB_PAGE, url])
    # "&start=" + str(num)
    check_call(["x-www-browser", "https://www.google.com/search?tbm=isch&q=" + english_word])
    # Switch i3-wm workspace to anki:
    check_call(["i3-msg", "workspace 1"])
    # showInfo("No image found")
    # Set focus on translation:
    self.web.eval("focusField(%d);" % 1)
    set_keyboard_layout(russian)

def clear_button_pressed(self):
    data = []
    data.append(("english", ""))
    self.note.fields[0] = ""
    data.append(("russian", ""))
    self.note.fields[1] = ""
    data.append(("usage_example", ""))
    self.note.fields[2] = ""
    data.append(("audio", ""))
    self.note.fields[3] = ""
    data.append(("image", ""))
    self.note.fields[4] = ""
    refresh_all_fields(self, data, 0)
    set_keyboard_layout(english)

def update_button_pressed(self):
    set_keyboard_layout(english)
    self.web.eval("focusField(%d);" % 0)

def set_english_word(self, english_word):
    data = []
    data.append(("english", english_word))
    self.note.fields[0] = english_word
    data.append(("russian", self.note.fields[1]))
    data.append(("usage_example", self.note.fields[2]))
    data.append(("audio", self.note.fields[3]))
    data.append(("image", self.note.fields[4]))
    refresh_all_fields(self, data, 1)
    set_keyboard_layout(russian)

def edit_button_pressed(self):
    set_keyboard_layout(english)
    # Read english word:
    self.web.eval("focusField(%d);" % 1)
    self.web.eval("focusField(%d);" % 0)
    english_word = self.note.fields[0]
    tmp_file = NamedTemporaryFile()
    tmp_file.write(english_word)
    tmp_file.flush()
    set_keyboard_layout(english)
    check_call(["e", "--wait", tmp_file.name])
    tmp_file.seek(0)
    updated_english_word = tmp_file.read()
    tmp_file.close()
    set_english_word(self, updated_english_word)
    set_keyboard_layout(russian)
    self.web.eval("focusField(%d);" % 1)

class add_prefix_mode():
    default = 0
    uppercase = 1

def add_prefix(self, prefix, mode = add_prefix_mode.default):
    field_0 = self.note.fields[0]
    if mode == add_prefix_mode.uppercase:
        field_0 = field_0[0].upper() + field_0[1:]
    field_0 = prefix + field_0
    set_english_word(self, field_0)

def sich_button_pressed(self):
    add_prefix(self, "sich ")

def der_button_pressed(self):
    add_prefix(self, "der ", add_prefix_mode.uppercase)

def die_button_pressed(self):
    add_prefix(self, "die ", add_prefix_mode.uppercase)

def das_button_pressed(self):
    add_prefix(self, "das ", add_prefix_mode.uppercase)

def to_button_pressed(self):
    add_prefix(self, "to ")

def a_button_pressed(self):
    add_prefix(self, "a ")

def an_button_pressed(self):
    add_prefix(self, "an ")

def setup_my_button(self, text, tooltip, shortcut, handler):
    # size=False tells Anki not to use a small button
    button = self._addButton(text, lambda s=self: handler(self), text=text, size=False)
    button.setShortcut(QKeySequence(shortcut))
    button.setToolTip(tooltip + ': ' + shortcut)

# This is modified version of function AddCards::addCards from addcards.py:
def my_addCards(self):
    self.saveNow()
    self.saveAddModeVars()
    note = self.note
    note = self.parentWindow.addNote(note)
    if not note:
        return False
    tooltip(_("Added"), period=500)
    # stop anything playing
    clearAudioQueue()
    self.parentWindow.onReset(keep=True)
    self.parentWindow.mw.col.autosave()
    return True

def add_images(self, root_dir, anki_dir):
    # Need to place focus on field #4 (image):
    self.web.eval("focusField(%d);" % 4)
    num = 0
    os.mkdir(anki_dir)
    for filename in os.listdir(root_dir):
        extension = os.path.splitext(filename)[1]
        if not extension in ['.png', '.jpeg', '.jpg']:
            continue
        before = os.path.join(root_dir, filename)
        after = os.path.join(anki_dir, str(num) + extension)
        os.rename(before, after)
        num = num + 1
    for filename in os.listdir(anki_dir):
        f = os.path.join(anki_dir, filename)
        self.addMedia(f, True)

def submit_button_pressed(self):
    root_dir = u'/media/files/downloads'
    anki_dir = os.path.join(root_dir, 'anki')
    if os.path.exists(anki_dir):
        rmtree(anki_dir)
    add_images(self, root_dir, anki_dir)
    # self.parentWindow.addCards()
    succeeded = my_addCards(self)
    if succeeded:
        set_keyboard_layout(english)
        # It seems like some delay is required before we can delete files..
        # rmtree(anki_dir)

def setup_all_my_buttons(self):
    setup_my_button(self, 'Submit', 'Submit card', 'Ctrl+j', submit_button_pressed)
    setup_my_button(self, 'Fill', 'Fill all fields', 'Ctrl+f', fill_button_pressed)
    setup_my_button(self, 'Clear', 'Clear all fields', 'Ctrl+Alt+c', clear_button_pressed)
    setup_my_button(self, 'Update', 'Update english word', 'Alt+u', update_button_pressed)
    setup_my_button(self, 'Edit', 'Edit english word in editor', 'Ctrl+Alt+e', edit_button_pressed)
    setup_my_button(self, 'sich', 'Add prefix "sich " (reflexiv)', 'Ctrl+s', sich_button_pressed)
    setup_my_button(self, 'der', 'Add prefix "der " (Maskulinum)', 'Ctrl+Alt+m', der_button_pressed)
    setup_my_button(self, 'die', 'Add prefix "die " (Femininum)', 'Ctrl+Alt+f', die_button_pressed)
    setup_my_button(self, 'das', 'Add prefix "das " (Neutrum)', 'Ctrl+Alt+n', das_button_pressed)
    setup_my_button(self, 'to', 'Add prefix "to " (verb)', 'Ctrl+Alt+t', to_button_pressed)
    setup_my_button(self, 'a', 'Add prefix "a " (noun)', 'Ctrl+Alt+a', a_button_pressed)
    setup_my_button(self, 'an', 'Add prefix "an " (noun)', 'Alt+Shift+a', an_button_pressed)

Editor.setupButtons = wrap(Editor.setupButtons, setup_all_my_buttons)
