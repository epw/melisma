#! /usr/bin/env python

import melisma
import unittest

class KeySigTestCase(unittest.TestCase):
    def test_pitch_class(self):
        key = melisma.KeySig(0)
        self.assertEqual(key.pitch_class(), "c")
        key = melisma.KeySig(1)
        self.assertEqual(key.pitch_class(), "g")
        key = melisma.KeySig(18)
        self.assertEqual(key.pitch_class(), "ges")

class NoteTestCase(unittest.TestCase):
    def test_pitch_class(self):
        key = melisma.KeySig(0)

        self.assertEqual(melisma.Note(0, 1).pitch_class(key), "c")
        self.assertEqual(melisma.Note(13, 1).pitch_class(key), "des")

        key = melisma.KeySig(18)

        self.assertEqual(melisma.Note(0, 1).pitch_class(key), "ges")
        self.assertEqual(melisma.Note(2, 1).pitch_class(key), "aes")

    def test_write(self):
        key = melisma.KeySig(0)
        self.assertEqual(melisma.Note(0, 1).write(key), "c4")
        self.assertEqual(melisma.Note(12, 4).write(key), "c'1")
        self.assertEqual(melisma.Note(-1, 0.5).write(key), "b8")
        self.assertEqual(melisma.Note(-13, 0.25).write(key), "b,16")

class PieceTestCase(unittest.TestCase):
    def test_current_key(self):
        music = melisma.Piece(melisma.KeySig(0), melisma.Tempo(120))
        music.push(melisma.KeySig(12))
        self.assertEqual(music.current_key().pitch, 12)

if __name__ == "__main__":
    unittest.main()
