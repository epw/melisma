#! /usr/bin/env python

class KeySig(object):
    """The current key signature is kept as a number of semitones from middle
C."""
    pitch = 0 # Semitones away from middle C

    def __init__(self, pitch=0):
        self.pitch = pitch

    def pitch_class(self):
        return ["C", "G", "D", "A", "E", "B", "Ges", "Des", "Aes", "Ees",
                "Bes", "F"][self.pitch % 12]

class Tempo(object):
    """The tempo is a number of beats per minute."""

    bpm = 120 # Beats per minute

    def __init__(self, bpm=120):
        self.bpm = bpm

class Note(object):
    """A note is a pitch and a duration. The pitch is stored as a number of
semitones offset from the current key signature.

This class also encodes rests, by using None for the pitch."""

    pitch = None # Semitones away from current key, or None for a rest
    duration = 1.0 # Beats

    def __init__(self, pitch, duration):
        self.pitch = pitch
        self.duration = duration

    def pitch_class(self, key):
        interval = (key.pitch + self.pitch) % 12
        return ["C", "Des", "D", "Ees", "E", "F", "Ges", "G", "Aes", "A",
                "Bes", "B"][interval]

class Piece(object):
    """A Piece is a piece of music, represented as a list of the objects
defined above."""

    music = []

    def __init__(self, key, tempo):
        """All music must start with a key and a tempo."""
        self.music = [key, tempo]

    def push(self, obj):
        self.music.append(obj)

    def current_key(self):
        """Get the most recently set key."""
        for element in self.music[::-1]:
            if type(element) == KeySig:
                return element

def main():
    music = Piece(KeySig(0), Tempo(120))
    for pitch in range(12):
        music.push(Note(pitch, 1))


    print """\\version "2.16.0"
\\score {
  \\new Staff \\with {midiInstrument = #"acoustic grand"}
  {"""
    for element in music.music:
        if type(element) == KeySig:
            pass # Need to implement
        elif type(element) == Tempo:
            print "  \\tempo 4 = " + str(element.bpm)
        elif type(element) == Note:
            print "  "


if __name__ == "__main__":
    main()
