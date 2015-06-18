#! /usr/bin/env python

import subprocess

from melisma import *

def main():
    f = open("rolling.ly", "w")
    with Piece(KeySig.name("c", 0), Tempo(60), output=f) as m:
        m.instrument = "acoustic grand"
        m.pkn(0, .25)
        m.pkn(2, .25)
        m.pkn(0, .25)

if __name__ == "__main__":
    main()
