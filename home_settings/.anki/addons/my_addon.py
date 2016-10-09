# fill              = Ctrl+f
# clear             = Ctrl+Alt+c
# submit card       = Ctrl+enter

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

def fill_button_pressed(self):
	# Settings:
	HOME = "/home/volkov"
	SOUNDS = HOME + "/other/GoldenDict/sound_en/sound_en.dsl.files.zip"
	MEDIA = HOME + "/.anki/slava/collection.media"
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
	# Add empty image:
	data.append(("image", self.note.fields[4]))
	# Refresh all fields:
	self.web.eval("setFields(%s, %d);" % (json.dumps(data), 4)) # 4 = field to place cursor to (image)
	self.web.eval("setFonts(%s);" % (json.dumps(self.fonts())))
	# Add image (if any):
	add_image(self, english_word)
	# Set focus on translation:
	self.web.eval("focusField(%d);" % 1)

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
	# Refresh fields:
	self.web.eval("setFields(%s, %d);" % (json.dumps(data), 0)) # 0 = field to place cursor to
	self.web.eval("setFonts(%s);" % (json.dumps(self.fonts())))

def to_button_pressed(self):
	showInfo("to_button_pressed: " + self.note.fields[0])

def setup_my_button(self, text, tooltip, shortcut, handler):
	# size=False tells Anki not to use a small button
	button = self._addButton(text, lambda s=self: handler(self), text=text, size=False)
	button.setShortcut(QKeySequence(shortcut))
	button.setToolTip(tooltip + ': ' + shortcut)

def setup_all_my_buttons(self):
	setup_my_button(self, 'Fill', 'Fill all fields', 'Ctrl+f', fill_button_pressed)
	setup_my_button(self, 'Clear', 'Clear all fields', 'Ctrl+Alt+c', clear_button_pressed)
	setup_my_button(self, 'to', 'Add prefix "to "', 'Ctrl+Alt+t', to_button_pressed)

Editor.setupButtons = wrap(Editor.setupButtons, setup_all_my_buttons)

