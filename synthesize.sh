#! /bin/bash

set -e

python "$1" > `basename "$1" .py`.ly
lilypond `basename "$1" .py`.ly
timidity -Ov `basename "$1" .py`.midi
