#!/usr/bin/python

# Main method:
def get_output_string(arg):
	if not is_valid_mp3_file_name(arg):
		return arg
	arg = delete_before_dot_mp3(arg, " (mp3cut.net)")
	arg = delete_before_dot_mp3(arg, "-spaces.ru")
	arg = arg.lower().replace(' ', '_')
	arg = add_underscores_around_dash(arg)
	arg = replace_braces_with_underscores(arg)
	arg = replace_multiple_underscores_with_single(arg)
	arg = delete_single_underscore_at_begin(arg)
	arg = delete_single_underscore_at_end(arg)
	arg = delete_dots(arg)
	arg = process_acoustic_version(arg)
	arg = delete_excessive_artist_name(arg)
	return arg

import re
import unittest

def get_pos_dot_mp3(arg):
	return arg.rfind('.')

def is_valid_mp3_file_name(arg):
	if len(arg) < 5:
		return False
	return arg[get_pos_dot_mp3(arg):] == ".mp3"

def delete_before_dot_mp3(arg, suffix):
	pos_after_suffix_end = get_pos_dot_mp3(arg)
	pos_suffix_begin = pos_after_suffix_end - len(suffix)
	if pos_suffix_begin <= 0:
		return arg
	if arg[pos_suffix_begin:pos_after_suffix_end] != suffix:
		return arg
	return arg[:pos_suffix_begin] + ".mp3"

def add_underscores_around_dash(arg):
	return re.sub("-", "_-_", arg)

def replace_braces_with_underscores(arg):
	return re.sub("[\(\)]", "_", arg)

def replace_multiple_underscores_with_single(arg):
	return re.sub("_+", "_", arg)

def delete_single_underscore_at_begin(arg):
	if arg[:1] == '_':
		return arg[1:]
	return arg

def delete_single_underscore_at_end(arg):
	pos = get_pos_dot_mp3(arg) - 1
	if arg[pos] == '_':
		return arg[:pos] + ".mp3"
	return arg

def delete_dots(arg):
	return re.sub("\.", "", arg[:-4]) + ".mp3"

def process_acoustic_version(arg):
	return re.sub("acoustic_version", "acoustic", arg)

def delete_excessive_artist_name(arg):
	pos_dash = arg.find('-')
	if pos_dash < 2:
		return arg
	artist = arg[:pos_dash - 1]
	last_pos_artist2 = pos_dash + 2 + len(artist)
	if artist != arg[pos_dash + 2:last_pos_artist2]:
		return arg
	if arg[last_pos_artist2] == '.':
		return arg
	return artist + "_-_" + arg[last_pos_artist2 + 1:]

class MyUnitTests(unittest.TestCase):
	def convert(self, arg, expected):
		self.assertEqual(expected, get_output_string(arg))

	def test_delete_suffix_1(self):
		self.convert("artist_-_track_name (mp3cut.net).mp3",
		             "artist_-_track_name.mp3")

	def test_delete_suffix_2(self):
		self.convert("artist_-_track_name-spaces.ru.mp3",
		             "artist_-_track_name.mp3")

	def test_to_lowercase(self):
		self.convert("Artist_-_Track_Name.mp3",
		             "artist_-_track_name.mp3")

	def test_replace_spaces_with_underscores(self):
		self.convert("artist - track name.mp3",
		             "artist_-_track_name.mp3")

	def test_add_underscores_around_dash(self):
		self.convert("artist1-artist2-track.mp3",
		             "artist1_-_artist2_-_track.mp3")

	def test_replace_braces_with_underscores(self):
		self.convert("artist_-_track(acoustic)name.mp3",
		             "artist_-_track_acoustic_name.mp3")

	def test_replace_multiple_underscores_with_single(self):
		self.convert("artist__name_-_track___name.mp3",
		             "artist_name_-_track_name.mp3")

	def test_delete_single_underscore_at_begin(self):
		self.convert("_artist_-_track_name.mp3",
		             "artist_-_track_name.mp3")

	def test_delete_single_underscore_at_end(self):
		self.convert("artist_-_track_name_.mp3",
		             "artist_-_track_name.mp3")

	def test_delete_dots(self):
		self.convert("artist1_feat._artist2_-_track.mp3",
		             "artist1_feat_artist2_-_track.mp3")

	def test_process_acoustic_version(self):
		self.convert("artist_-_track_acoustic_version.mp3",
		             "artist_-_track_acoustic.mp3")

	def test_delete_excessive_artist_name(self):
		self.convert("word1_word22_-_word1_word22_word333_word4444_word55555.mp3",
		             "word1_word22_-_word333_word4444_word55555.mp3")

	def test_do_nothing(self):
		self.convert("word1_word22_-_word1_word22.mp3",
		             "word1_word22_-_word1_word22.mp3")

if __name__ == '__main__':
    unittest.main()
