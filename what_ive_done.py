#! /usr/bin/env python

from melisma import *

def high_phrase (music):
    music.push(KeySig.name("bes", 1))

    music.push_key_note(-2, .25)
    music.push_key_note(2, .25)
    music.push_key_note(-2, .25)
    music.push_key_note(2, .25)
    music.push_key_note(3, .25)
    music.push_key_note(2, .25)
    music.push_key_note(-2, .25)
    music.push_key_note(2, .25)

def main():
    music = Piece(KeySig.name("bes", 2), Tempo(70))

    high_phrase(music)
    music.push_rest(4, 1)
    high_phrase(music)
    music.push_rest(4, 1)
    high_phrase(music)
    music.push_key_note(-10, 4, voice=1)
    high_phrase(music)
    music.push_key_note(-8, 4, voice=1)
    high_phrase(music)
    music.push_key_note(-3, 4, voice=1)

    music.write()

if __name__ == "__main__":
    main()
