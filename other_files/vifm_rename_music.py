#!/usr/bin/python3

# Main method:
def get_output_string(arg):
    if not is_valid_mp3_file_name(arg):
        return arg
    arg = delete_before_dot_mp3(arg, " (mp3cut.net)")
    arg = delete_before_dot_mp3(arg, "-spaces.ru")
    arg = delete_before_dot_mp3(arg, " [zippy.audio]")
    arg = arg.lower().replace(' ', '_').replace('(', '_').replace(')', '_').replace('-', "_-_").replace('&', '_')
    arg = arg.replace("acoustic_version", "acoustic").replace("i'm", "i_am").replace("you're", "you_are")
    arg = arg.replace("'ll", "_will").replace("n't", "_not")
    arg = arg.replace("'", "")
    arg = replace_multiple_underscores_with_single(arg)
    arg = delete_single_underscore_at_begin(arg)
    arg = delete_single_underscore_at_end(arg)
    arg = delete_dots(arg)
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

    def test_delete_single_quotes(self):
        self.convert("artist's.mp3", "artists.mp3")

    def test_delete_suffix_1(self):
        self.convert("artist_-_track_name (mp3cut.net).mp3",
                     "artist_-_track_name.mp3")

    def test_delete_suffix_2(self):
        self.convert("artist_-_track_name-spaces.ru.mp3",
                     "artist_-_track_name.mp3")

    def test_to_lowercase(self):
        self.convert("Artist_-_Track_Name.mp3",
                     "artist_-_track_name.mp3")

    def test_to_lowercase_russian(self):
        self.convert(r'Привет.mp3', r'привет.mp3')

    def test_replace_spaces_with_underscores(self):
        self.convert("artist - track name.mp3",
                     "artist_-_track_name.mp3")

    def test_add_underscores_around_dash(self):
        self.convert("artist1-artist2-track.mp3",
                     "artist1_-_artist2_-_track.mp3")

    def test_replace_and_with_underscore(self):
        self.convert("artist1_&_artist2_-_track.mp3",
                     "artist1_artist2_-_track.mp3")

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

    def test_rename_i_am(self):
        self.convert("artist_-_i'm_alive.mp3",
                     "artist_-_i_am_alive.mp3")

    def test_rename_will(self):
        self.convert("artist_-_it'll_be_fine.mp3",
                     "artist_-_it_will_be_fine.mp3")

    def test_rename_not(self):
        self.convert("artist_-_doesn't_know.mp3",
                     "artist_-_does_not_know.mp3")

if __name__ == '__main__':
    unittest.main()
