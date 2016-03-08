# fill = ctrl+j
# add  = ctrl+enter

from anki.hooks import wrap
from aqt.editor import Editor
from aqt.utils import showInfo
from aqt.qt import QKeySequence
from anki.utils import json

from shutil import rmtree,move
import os.path

# from subprocess import call
# from subprocess import check_output

from subprocess import *

def get_image(english_word, MEDIA):
	TMP_DIR = "/tmp/anki"
	TMP_DIR_IMAGE_FILE = TMP_DIR + "/image.jpg"
	FILE_NAME = english_word + ".jpg"
	MEDIA_IMAGE_FILE = MEDIA + "/" + FILE_NAME
	IMAGE_WEB_PAGE = TMP_DIR + "/web_page.html"
	# Delete old temporary folder contents:
	if os.path.exists(TMP_DIR):
		rmtree(TMP_DIR)
	# Create empty temporary folder:
	os.makedirs(TMP_DIR)
	# Download images web-page:
	check_call(["wget", "--user-agent=Mozilla/5.0", "-O", IMAGE_WEB_PAGE, "https://www.google.com/search?tbm=isch&q=" + english_word])
	# Launch browser (uzbl) in order to select an image:
	check_call(["uzbl", "file://" + IMAGE_WEB_PAGE])
	# As a selection result, uzbl calls external python script, which extracts
	# selected image link from IMAGE_WEB_PAGE, download appropriate image
	# and names it TMP_DIR_IMAGE_FILE. If user hasn't selected an image (just
	# closed uzbl), than IMAGE_FILE will be absent. In this case we will return
	# empty string.
	if os.path.isfile(TMP_DIR_IMAGE_FILE):
		# Copy file to MEDIA:
		move(TMP_DIR_IMAGE_FILE, MEDIA_IMAGE_FILE)
		return "<img src=\"" + FILE_NAME + "\" />" # "<img src="father.jpg" />"
	return ""

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
	if os.path.isfile(MEDIA + "/" + english_word + ".mp3"):
		ret = 0
	else:
		ret = call(["unzip", SOUNDS, english_word + ".mp3", "-d", MEDIA])
	if ret == 0:
		audio_field_content = "[sound:" + english_word + ".mp3]"
	else:
		audio_field_content = ""
		showInfo("No audio found")
	data.append(("audio", audio_field_content))
	self.note.fields[3] = audio_field_content

	# Add image (may be empty):
	image_field_content = get_image(english_word, MEDIA)
	data.append(("image", image_field_content))
	self.note.fields[4] = image_field_content

	# Refresh fields:
	self.web.eval("setFields(%s, %d);" % (json.dumps(data), 1)) # 1 = field to place cursor to
	self.web.eval("setFonts(%s);" % (json.dumps(self.fonts())))

def setup_fill_button(self):
	# size=False tells Anki not to use a small button
	fill_button = self._addButton("fill_button", lambda s=self: fill_button_pressed(self), text="Fill", size=False)
	fill_shortcut = "Ctrl+j"
	fill_button.setShortcut(QKeySequence(fill_shortcut))
	fill_button.setToolTip("Fill all fields: " + fill_shortcut)

Editor.setupButtons = wrap(Editor.setupButtons, setup_fill_button)

# def my_add_card():
# 	my_note = mw.col.newNote()
# 	my_note.fields[0] = "mother"
# 	my_note.fields[1] = "mother_translation"
# 	my_note.fields[2] = "mother_example"
# 	my_note.fields[3] = "[sound:sludge.mp3]"
# 	my_note.fields[4] = "<img src=\"allegiance.jpg\" />"
# 	mw.col.addNote(my_note)

