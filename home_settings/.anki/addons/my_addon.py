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

from shutil import rmtree,move
import os.path

# from subprocess import *
from subprocess import call,check_call

from aqt import addcards
from aqt.utils import tooltip
from anki.sound import clearAudioQueue

KEYBOARD_LAYOUT = "/media/files/other/programs/keyboard_layout"

def add_image(self, english_word):
	TMP_DIR = "/tmp/anki"
	TMP_DIR_IMAGE_FILE = TMP_DIR + "/image.jpg"
	FILE_NAME = english_word + ".jpg"
	TMP_DIR_OUT_FILE = TMP_DIR + "/" + FILE_NAME
	TMP_DIR_STATUS_FILE = TMP_DIR + "/status"
	IMAGE_WEB_PAGE = TMP_DIR + "/web_page.html"
	# Delete old temporary folder contents:
	if os.path.exists(TMP_DIR):
		rmtree(TMP_DIR)
	# Create empty temporary folder:
	os.makedirs(TMP_DIR)
	# Start from first google search result:
	num = 0
	# Switch keyboard layout to english:
	check_call([KEYBOARD_LAYOUT, "--set_layout", "0"])
	while (True):
		# Download images web-page:
		check_call(["wget", "--user-agent=Mozilla/5.0", "-O", IMAGE_WEB_PAGE,
			"https://www.google.com/search?tbm=isch&start=" + str(num) + "&q=" + english_word])
		# Launch browser (uzbl) in order to select an image:
		check_call(["uzbl", "file://" + IMAGE_WEB_PAGE])
		# There are four possible scenarios:
		#
		# 1) User selects an image (middle mouse click on image selected):
		#      as a selection result, uzbl calls external python script, which
		#      extracts selected image link from IMAGE_WEB_PAGE, downloads
		#      appropriate image and names it TMP_DIR_IMAGE_FILE.
		#
		#      On the very begin of execution the script writes "downloading" to
		#      TMP_DIR_STATUS_FILE. At the end (in case of success) it writes
		#      "done" to TMP_DIR_STATUS_FILE.
		#
		# 2) User requests next page (hotkey "n"):
		#      uzbl calls external python script with argument "next", which in
		#      turn writes word "next" in TMP_DIR_STATUS_FILE.
		#
		# 3) User requests termination without image (hotkey "q"):
		#      uzbl calls external python script with argument "quit", which in
		#      turn writes word "quit" in TMP_DIR_STATUS_FILE.
		#
		# 4) User specifies different search request (hotkey "d"):
		#      uzbl calls external python script with arguments
		#      "request NEW_REQEUST", which in turn writes line
		#      "request: NEW_REQUEST" in TMP_DIR_STATUS_FILE.
		#
		if not os.path.isfile(TMP_DIR_STATUS_FILE):
			showInfo("No status file found (uzbl has been terminated abnormally?). Please check " + TMP_DIR_STATUS_FILE)
			return
		with open(TMP_DIR_STATUS_FILE, 'r') as f:
			content = f.read()
		if (content == "done"):
			if os.path.isfile(TMP_DIR_IMAGE_FILE):
				move(TMP_DIR_IMAGE_FILE, TMP_DIR_OUT_FILE)
				# Need to place focus on field #4 (image):
				# self.web.eval("focusField(%d);" % 4)
				self.addMedia(TMP_DIR_OUT_FILE, True)
			else:
				showInfo("No image found although " + TMP_DIR_STATUS_FILE + " reports done")
			return
		elif (content == "quit"):
			return
		elif (content[:len("request: ")] == "request: "):
			english_word = content[len("request: "):]
			num = 0
		elif (content != "next"):
			showInfo("Unexpected contents of " + TMP_DIR_STATUS_FILE + ": |" + content + "|")
			return
		else:
			num = num + 20

def refresh_all_fields(self, data, cursor_position):
	self.web.eval("setFields(%s, %d);" % (json.dumps(data), cursor_position))
	self.web.eval("setFonts(%s);" % (json.dumps(self.fonts())))

def fill_button_pressed(self):
	# Settings:
	HOME = "/home/volkov"
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
	# Add image (if any):
	add_image(self, english_word)
	# Switch i3-wm workspace to anki:
	check_call(["i3-msg", "workspace 1"])
	# Set focus on translation:
	self.web.eval("focusField(%d);" % 1)
	# Switch keyboard layout to russian:
	check_call([KEYBOARD_LAYOUT, "--set_layout", "1"])

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
	# Switch keyboard layout to english:
	check_call([KEYBOARD_LAYOUT, "--set_layout", "0"])

class add_prefix_mode():
	default = 0
	uppercase = 1

def add_prefix(self, prefix, mode = add_prefix_mode.default):
	field_0 = self.note.fields[0]
	if mode == add_prefix_mode.uppercase:
		field_0 = field_0[0].upper() + field_0[1:]
	field_0 = prefix + field_0
	data = []
	data.append(("english", field_0))
	self.note.fields[0] = field_0
	data.append(("russian", self.note.fields[1]))
	data.append(("usage_example", self.note.fields[2]))
	data.append(("audio", self.note.fields[3]))
	data.append(("image", self.note.fields[4]))
	refresh_all_fields(self, data, 1)

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

def submit_button_pressed(self):
	# self.parentWindow.addCards()
	# succeeded = True
	succeeded = my_addCards(self)
	# Switch keyboard layout to english if submit succeeded:
	if succeeded:
		check_call([KEYBOARD_LAYOUT, "--set_layout", "0"])

def setup_all_my_buttons(self):
	setup_my_button(self, 'Submit', 'Submit card', 'Ctrl+j', submit_button_pressed)
	setup_my_button(self, 'Fill', 'Fill all fields', 'Ctrl+f', fill_button_pressed)
	setup_my_button(self, 'Clear', 'Clear all fields', 'Ctrl+Alt+c', clear_button_pressed)
	setup_my_button(self, 'sich', 'Add prefix "sich " (reflexiv)', 'Ctrl+s', sich_button_pressed)
	setup_my_button(self, 'der', 'Add prefix "der " (Maskulinum)', 'Ctrl+Alt+m', der_button_pressed)
	setup_my_button(self, 'die', 'Add prefix "die " (Femininum)', 'Ctrl+Alt+f', die_button_pressed)
	setup_my_button(self, 'das', 'Add prefix "das " (Neutrum)', 'Ctrl+Alt+n', das_button_pressed)
	setup_my_button(self, 'to', 'Add prefix "to " (verb)', 'Ctrl+Alt+t', to_button_pressed)
	setup_my_button(self, 'a', 'Add prefix "a " (noun)', 'Ctrl+Alt+a', a_button_pressed)
	setup_my_button(self, 'an', 'Add prefix "an " (noun)', 'Alt+Shift+a', an_button_pressed)

Editor.setupButtons = wrap(Editor.setupButtons, setup_all_my_buttons)

