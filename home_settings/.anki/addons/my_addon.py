# fill = ctrl+j
# add  = ctrl+enter

from anki.hooks import wrap
from aqt.editor import Editor
from aqt.utils import showInfo
from aqt.qt import QKeySequence
from anki.utils import json

# from subprocess import call
# from subprocess import check_output

from subprocess import *

def fill_button_pressed(self):
	# Settings:
	HOME = "/home/volkov"
	SOUNDS = HOME + "/other/GoldenDict/sound_en/sound_en.dsl.files.zip"
	# MEDIA = HOME + "/.anki/slava/collection.media"
	MEDIA = HOME + "/downloads"

	# Read english word:
	self.web.eval("focusField(%d);" % 1)
	self.web.eval("focusField(%d);" % 0)
	english_word = self.note.fields[0]

	# Translate english word:
	call(["goldendict", english_word])

	# Copy english word to newly created data:
	data = []
	data.append(("english", english_word))
	self.note.fields[0] = english_word
	data.append(("russian", ""))
	self.note.fields[1] = ""
	data.append(("usage_example", ""))
	self.note.fields[2] = ""

	# Add audio if available:
	ret = call(["unzip", SOUNDS, english_word + ".mp3", "-d", MEDIA])
	if ret == 0:
		audio_field_content = "[sound:" + english_word + ".mp3]"
	else:
		audio_field_content = ""
		showInfo("No audio found")
	data.append(("audio", audio_field_content))
	self.note.fields[3] = audio_field_content

	# Launch browser (uzbl) in order to search for the image:
	ret = call(["uzbl", "https://www.google.com/search?tbm=isch&q=" + english_word])

	# data.append(("image", "<img src=\"father.jpg\" />"))
	# self.note.fields[4] = "<img src=\"father.jpg\" />"
	data.append(("image", ""))
	self.note.fields[4] = ""

	# Refresh fields:
	self.web.eval("setFields(%s, %d);" % (json.dumps(data), 0)) # 0 = field to place cursor to
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
