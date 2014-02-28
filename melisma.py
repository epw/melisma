#! /usr/bin/env python

note_letters = ["c", "des", "d", "ees", "e", "f", "ges", "g", "aes", "a",
                "bes", "b"]

class KeySig(object):
    """The current key signature is kept as a number of semitones from middle
C."""
    pitch = 0 # Semitones away from middle C

    def __init__(self, pitch=0):
        self.pitch = pitch

    def write(self, _=None):
        return "\\key " + self.pitch_class() + " \\major"

    def pitch_class(self):
        return note_letters[self.pitch % 12]

class Tempo(object):
    """The tempo is a number of beats per minute."""

    bpm = 120 # Beats per minute

    def __init__(self, bpm=120):
        self.bpm = bpm

    def write(self, _=None):
        return "\\tempo 4 = " + str(self.bpm)

class Note(object):
    """A note is a pitch and a duration. The pitch is stored as a number of
semitones offset from the current key signature.

This class also encodes rests, by using None for the pitch."""

    pitch = None # Semitones away from current key, or None for a rest
    duration = 1.0 # Beats

    def __init__(self, pitch, duration):
        self.pitch = pitch
        self.duration = duration

    def write(self, key):
        octave = (key.pitch + self.pitch) / 12
        if octave < 0:
            octave_char = ","
            octave += 1
        else:
            octave_char = "'"
        return "%s%s%d" % (self.pitch_class(key),
                           octave_char * abs(octave),
                           int(round(4 / self.duration)))

    def pitch_class(self, key):
        interval = (key.pitch + self.pitch) % 12
        return note_letters[interval]

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
        for element in self.music:
            print "    " + element.write(self.current_key())
        print """  }
  \\layout { }
  \\midi { }
}"""

    def push(self, obj):
        """Add any object to the music."""
        self.music.append(obj)

    def current_key(self):
        """Get the most recently set key."""
        for element in self.music[::-1]:
            if type(element) == KeySig:
                return element

    def _step_to_interval(self, step):
        if step < 3:
            return step * 2
        elif step < 7:
            return step * 2 - 1
        return 12

    def push_key_note(self, step, duration):
        """Push a particular note of the current key signature."""
        interval = self._step_to_interval(step)
        self.push(Note(interval, duration))

    def push_triad_note(self, root, step, duration, quality="major"):
        """Push a particular note of a triad chord."""
        if step == 0:
            interval = 0
        elif step == 1:
            if quality == "major" or quality == "augmented":
                interval = 4
            elif quality == "minor" or quality == "diminished":
                interval = 3
            else:
                raise ValueError("Quality must be major, minor, diminished, or augmented")
        elif step == 2:
            if quality == "major" or quality == "minor":
                interval = 7
            elif quality == "diminished":
                interval = 6
            elif quality == "augmented":
                interval = 8
            else:
                raise ValueError("Quality must be major, minor, diminished, or augmented")
        else:
            raise ValueError("Step must be 0, 1, or 2 for a triad chord")

        self.push(Note(root + interval, duration))

def main():
    music = Piece(KeySig(12), Tempo(120))
    music.push_triad_note(0, 0, 0.5)
    music.push_triad_note(0, 1, 0.5)
    music.push_triad_note(0, 2, 0.5)

    music.write ()

if __name__ == "__main__":
    main()
