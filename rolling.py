#! /usr/bin/env python

import subprocess

from melisma import *

def lilypond():
    subprocess.call(["/usr/bin/lilypond", "rolling.ly"])

def timidity():
    subprocess.call(["timidity", "--output-24bit", "-A120", "rolling.midi"])

def main():
    f = open("rolling.ly", "w")
    with Piece(KeySig.name("c", 0), Tempo(60), output=f) as m:
        m.instrument = "acoustic grand"
        m.pkn(0, .25)
        m.pkn(2, .25)
        m.pkn(0, .25)

    lilypond()
    timidity()
    
if __name__ == "__main__":
    main()
