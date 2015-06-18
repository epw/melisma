#! /usr/bin/env python

import math
import sys

note_letters = ["c", "des", "d", "ees", "e", "f", "ges", "g", "aes", "a",
                "bes", "b"]

class Component(object):
    """Parent class for allowed members of music list."""
    pass

class KeySig(Component):
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

class Tempo(Component):
    """The tempo is a number of beats per minute."""

    bpm = 120 # Beats per minute

    def __init__(self, bpm=120):
        self.bpm = bpm

    def write(self):
        return "\\tempo 4 = " + str(self.bpm)

class TimeSig(Component):

    npm = 4
    nf = 4

    def __init__(self, notes_per_measure, note_fraction):
        self.npm = notes_per_measure
        self.nf = note_fraction

    def write(self):
        return "\\time %d/%d" % (self.npm, self.nf)


def composition(n):
    powers = []
    i = math.floor(math.log(n, 2))
    while i >= 0:
        if 2**i <= n:
            powers.append(int(2**i))
            n -= 2**i
        i -= 1
    return powers

class Note(Component):
    """A note is a pitch and a duration. The pitch is stored as a number of
semitones offset from the current key signature.

This class also encodes rests, by using None for the pitch."""

    pitch = None # Semitones away from current key, or None for a rest
    duration = None # Type of note as whole number or (numerator, denominator)
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
        if self.pitch:
            octave = (key.pitch + self.pitch) / 12
            if octave < 0:
                octave_char = ","
                octave += 1
            else:
                octave_char = "'"
        description = "%s%s%d" % (self.pitch_class(key),
                                  octave_char * abs(octave),
                                  duration))
        if "fermata" in self.attrs:
            description += "\\fermata"
        return description

    def pitch_class(self, key):
        if self.pitch == None:
            return "r"
        interval = (key.pitch + self.pitch) % 12
        return note_letters[interval]

def step_to_interval(step):
    if step < 3:
        return step * 2
    elif step < 7:
        return step * 2 - 1
    return 12

class MeasureException(Exception):
    def __init__(self, expected, actual):
        self.expected = expected
        self.actual = actual
    def __str__(self):
        return "Expected %d beats in measure, got %d" % (self.expected, self.actual)

class Piece(object):
    """A Piece is a piece of music, represented as a list of the objects
defined above."""

    music = []
    instrument = "acoustic grand"

    def __init__(self, key, tempo, timesig=None, output=sys.stdout):
        """All music must start with a key and a tempo."""
        self.music = [key, tempo, timesig or TimeSig(4, 4)]
        self.instrument = "acoustic grand"
        self.output = output
        self.alias()

    def __enter__(self):
        return self

    def __exit__(self, unused_type, unused_value, unused_traceback):
        self.write()

    def write(self):
        self.output.write("""\\version "2.14.0"
\\score {
  \\new Staff \\with {midiInstrument = #"%s"}
  {
""" % self.instrument)
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
                self.output.write("    <<\n")
                n = 0
                for voice in voices:
                    n += 1
                    self.output.write("      {\n")
                    for note in voice:
                        self.output.write("        " + note.write(key) + "\n")
                    self.output.write("      }\n")
                    if n < len(voices) - 1:
                        self.output.write(r"      \\" + "\n")
                self.output.write("    >>\n")
            else:
                if type(element) == KeySig:
                    key = element
                self.output.write("    " + element.write() + "\n")
        self.output.write("""  }
  \\layout { }
  \\midi { }
}
""")

    def alias(self):
        """Give shorter names to common API methods."""
        self.key = lambda: self.current_element(KeySig)
        self.tempo = lambda: self.current_element(Tempo)
        self.time = lambda: self.current_element(TimeSig)
        self.pr = self.push_rest
        self.pkn = self.push_key_note
        self.ptn = self.push_triad_note

    def current_element(self, cls):
        """Get the most recently set element of a Component class."""
        for element in self.music[::-1]:
            if type(element) == cls:
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

    def push_measure(self, *notes):
        pass

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


def lilypond(name):
    subprocess.call(["/usr/bin/lilypond", "%s.ly" % name])

def timidity(name):
    subprocess.call(["timidity", "--output-24bit", "-A120", "%s.midi" % name])


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
