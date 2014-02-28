#! /usr/bin/env python

import melisma
import unittest

class KeySigTestCase(unittest.TestCase):
    def test_pitch_class(self):
        key = melisma.KeySig(0)
        self.assertEqual(key.pitch_class(), "C")
        key = melisma.KeySig(1)
        self.assertEqual(key.pitch_class(), "G")
        key = melisma.KeySig(18)
        self.assertEqual(key.pitch_class(), "Ges")

class NoteTestCase(unittest.TestCase):
    def test_pitch_class(self):
        key = melisma.KeySig(0)

        self.assertEqual(melisma.Note(0, 1).pitch_class(key), "C",
                         "Wrong pitch class returned")
        self.assertEqual(melisma.Note(13, 1).pitch_class(key), "Des",
                         "Wrong pitch class returned")

        key = melisma.KeySig(18)

        self.assertEqual(melisma.Note(0, 1).pitch_class(key), "Ges",
                         "Wrong pitch class returned")
        self.assertEqual(melisma.Note(2, 1).pitch_class(key), "Aes",
                         "Wrong pitch class returned")


class PieceTestCase(unittest.TestCase):
    def test_current_key(self):
        music = melisma.Piece(melisma.KeySig(0), melisma.Tempo(120))
        music.push(melisma.KeySig(12))
        self.assertEqual(music.current_key().pitch, 12,
                         "Incorrect key retrieved")

if __name__ == "__main__":
    unittest.main()
