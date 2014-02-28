#! /usr/bin/env python

import math
import sys

note_letters = ["c", "des", "d", "ees", "e", "f", "ges", "g", "aes", "a",
                "bes", "b"]

class KeySig(object):
    """The current key signature is kept as a number of semitones from middle
C."""
    pitch = 0 # Semitones away from middle C

    def __init__(self, pitch=0):
        self.pitch = pitch

    @classmethod
    def name(cls, letter, octave=0):
        return KeySig(note_letters.index(letter) + octave * 12)

    def write(self):
        return "\\key " + self.pitch_class() + " \\major"

    def pitch_class(self):
        return note_letters[self.pitch % 12]

class Tempo(object):
    """The tempo is a number of beats per minute."""

    bpm = 120 # Beats per minute

    def __init__(self, bpm=120):
        self.bpm = bpm

    def write(self):
        return "\\tempo 4 = " + str(self.bpm)

def composition(n):
    powers = []
    i = math.floor(math.log(n, 2))
    while i >= 0:
        if 2**i <= n:
            powers.append(int(2**i))
            n -= 2**i
        i -= 1
    return powers

class Note(object):
    """A note is a pitch and a duration. The pitch is stored as a number of
semitones offset from the current key signature.

This class also encodes rests, by using None for the pitch."""

    pitch = None # Semitones away from current key, or None for a rest
    duration = 1.0 # Beats
    voice = 0 # Each voice advances along the beats with only its own notes
    attrs = [] # Other special qualities

    def __init__(self, pitch, duration, voice=0, attrs=[]):
        self.pitch = pitch
        self.duration = duration
        self.voice = voice
        if type(attrs) == list:
            self.attrs = attrs
        else:
            self.attrs = [attrs]

    def write(self, key):
        durations = []
        if self.duration >= 1:
            durations = map(lambda x: int(round(4/x)),
                            composition(self.duration))
        else:
            durations = [int(4 / self.duration)]
            
        note_type = int (round(4 / self.duration))
        if self.pitch == None:
            rests = []
            for duration in durations:
                rests.append ("r" + str(duration))
            return ' '.join(rests)

        octave = (key.pitch + self.pitch) / 12
        if octave < 0:
            octave_char = ","
            octave += 1
        else:
            octave_char = "'"
        notes = []
        for duration in durations:
            notes.append ("%s%s%d" % (self.pitch_class(key),
                                      octave_char * abs(octave),
                                      duration))
        description = "~ ".join(notes)

        if "dotted" in self.attrs:
            description += "."
        if "fermata" in self.attrs:
            description += "\\fermata"
        return description

    def pitch_class(self, key):
        interval = (key.pitch + self.pitch) % 12
        return note_letters[interval]

def step_to_interval(step):
    if step < 3:
        return step * 2
    elif step < 7:
        return step * 2 - 1
    return 12

class Piece(object):
    """A Piece is a piece of music, represented as a list of the objects
defined above."""

    music = []

    def __init__(self, key, tempo):
        """All music must start with a key and a tempo."""
        self.music = [key, tempo]

    def write(self):
        print """\\version "2.16.0"
\\score {
  \\new Staff \\with {midiInstrument = #"acoustic grand"}
  {"""
        writing_music = self.music[:]
        key = None
        while writing_music:
            element = writing_music.pop(0)
            if type(element) == Note:
                voices = [[element]]
                while writing_music:
                    note = writing_music.pop(0)
                    if type(note) != Note:
                        writing_music = [note] + writing_music
                        break
                    while len(voices) <= note.voice:
                        voices.append([])
                    voices[note.voice].append(note)
                print "    <<"
                n = 0
                for voice in voices:
                    n += 1
                    print "      {"
                    for note in voice:
                        print "        " + note.write(key)
                    print "      }"
                    if n < len(voices) - 1:
                        print r"      \\"
                print "    >>"
            else:
                if type(element) == KeySig:
                    key = element
                print "    " + element.write()
        print """  }
  \\layout { }
  \\midi { }
}"""

    def current_key(self):
        """Get the most recently set key."""
        for element in self.music[::-1]:
            if type(element) == KeySig:
                return element

    def push(self, obj):
        """Add any object to the music."""
        self.music.append(obj)

    def push_rest(self, duration, voice=0):
        self.music.append(Note(None, duration, voice))

    def push_key_note(self, step, duration, voice=0, attrs=[]):
        """Push a particular note of the current key signature."""
        interval = step_to_interval(step)
        self.push(Note(interval, duration, voice=voice, attrs=attrs))

    def push_triad_note(self, root, step, duration, quality="major", voice=0,
                        attrs=[]):
        """Push a particular note of a triad chord."""
        if step == 0:
            interval = 0
        elif step == 1:
            if quality == "major" or quality == "augmented":
                interval = 4
            elif quality == "minor" or quality == "diminished":
                interval = 3
            else:
                raise ValueError("Quality must be major, minor, diminished, "
                                 + "or augmented")
        elif step == 2:
            if quality == "major" or quality == "minor":
                interval = 7
            elif quality == "diminished":
                interval = 6
            elif quality == "augmented":
                interval = 8
            else:
                raise ValueError("Quality must be major, minor, diminished, "
                                 + "or augmented")
        else:
            raise ValueError("Step must be 0, 1, or 2 for a triad chord")

        self.push(Note(root + interval, duration, voice=voice, attrs=attrs))

def phrase(music, quality="major"):
    music.push_triad_note(0, 0, 1, voice=0, quality=quality)
    music.push_triad_note(0, 1, 1, voice=1, quality=quality)
    music.push_rest(2, voice=1)
    music.push_rest(1, voice=1)
    music.push_triad_note(0, 2, 1, voice=2, quality=quality)
    music.push_rest(2, voice=2)
    music.push_rest(1, voice=2)

    music.push_triad_note(0, 1, 1, voice=0, quality=quality)
    music.push_triad_note(0, 2, 1, voice=0, quality=quality)
    music.push_rest(1, voice=0)


def main():
    music = Piece(KeySig.name("g", 1), Tempo(120))

    phrase(music)
    phrase(music)
    phrase(music)
    music.push(KeySig.name("d", 1))
    phrase(music)
    phrase(music)
    music.push(KeySig.name("c", 1))
    phrase(music)
    phrase(music, "diminished")

    music.push_key_note(0, 3, attrs=["fermata"])

    music.write ()

if __name__ == "__main__":
    main()
