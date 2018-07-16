(defpackage #:melisma-instruments
  (:use #:cl #:alexandria))

(in-package #:melisma-instruments)

(defmacro define-instrument (name string)
  `(progn (alexandria:define-constant ,name ,string :test 'equal)
	  (export '(,name))))

(define-instrument +accordion+ "accordion")

(define-instrument +drum-snare+ "sn")
(define-instrument +drum-bass+ "bd")
(define-instrument +drum-cym+ "crashcymbal")

(define-instrument +accordion+ "accordion")
(define-instrument +acoustic-bass+ "acoustic bass")
(define-instrument +acoustic-grand+ "acoustic grand")
(define-instrument +acoustic-guitar-nylon+ "acoustic guitar (nylon)")
(define-instrument +acoustic-guitar-steel+ "acoustic guitar (steel)")
(define-instrument +agogo+ "agogo")
(define-instrument +alto-sax+ "alto sax")
(define-instrument +applause+ "applause")
(define-instrument +bagpipe+ "bagpipe")
(define-instrument +banjo+ "banjo")
(define-instrument +baritone-sax+ "baritone sax")
(define-instrument +bassoon+ "bassoon")
(define-instrument +bird-tweet+ "bird tweet")
(define-instrument +blown-bottle+ "blown bottle")
(define-instrument +brass-section+ "brass section")
(define-instrument +breath-noise+ "breath noise")
(define-instrument +bright-acoustic+ "bright acoustic")
(define-instrument +celesta+ "celesta")
(define-instrument +cello+ "cello")
(define-instrument +choir-aahs+ "choir aahs")
(define-instrument +church-organ+ "church organ")
(define-instrument +clarinet+ "clarinet")
(define-instrument +clav+ "clav")
(define-instrument +concertina+ "concertina")
(define-instrument +contrabass+ "contrabass")
(define-instrument +distorted-guitar+ "distorted guitar")
(define-instrument +drawbar-organ+ "drawbar organ")
(define-instrument +dulcimer+ "dulcimer")
(define-instrument +electric-bass-finger+ "electric bass (finger)")
(define-instrument +electric-bass-pick+ "electric bass (pick)")
(define-instrument +electric-grand+ "electric grand")
(define-instrument +electric-guitar-clean+ "electric guitar (clean)")
(define-instrument +electric-guitar-jazz+ "electric guitar (jazz)")
(define-instrument +electric-guitar-muted+ "electric guitar (muted)")
(define-instrument +electric-piano-1+ "electric piano 1")
(define-instrument +electric-piano-2+ "electric piano 2")
(define-instrument +english-horn+ "english horn")
(define-instrument +fiddle+ "fiddle")
(define-instrument +flute+ "flute")
(define-instrument +french-horn+ "french horn")
(define-instrument +fretless-bass+ "fretless bass")
(define-instrument +fx-1-rain+ "fx 1 (rain)")
(define-instrument +fx-2-soundtrack+ "fx 2 (soundtrack)")
(define-instrument +fx-3-crystal+ "fx 3 (crystal)")
(define-instrument +fx-4-atmosphere+ "fx 4 (atmosphere)")
(define-instrument +fx-5-brightness+ "fx 5 (brightness)")
(define-instrument +fx-6-goblins+ "fx 6 (goblins)")
(define-instrument +fx-7-echoes+ "fx 7 (echoes)")
(define-instrument +fx-8-sci-fi+ "fx 8 (sci-fi)")
(define-instrument +glockenspiel+ "glockenspiel")
(define-instrument +guitar-fret-noise+ "guitar fret noise")
(define-instrument +guitar-harmonics+ "guitar harmonics")
(define-instrument +gunshot+ "gunshot")
(define-instrument +harmonica+ "harmonica")
(define-instrument +harpsichord+ "harpsichord")
(define-instrument +helicopter+ "helicopter")
(define-instrument +honky-tonk+ "honky-tonk")
(define-instrument +kalimba+ "kalimba")
(define-instrument +koto+ "koto")
(define-instrument +lead-1-square+ "lead 1 (square)")
(define-instrument +lead-2-sawtooth+ "lead 2 (sawtooth)")
(define-instrument +lead-3-calliope+ "lead 3 (calliope)")
(define-instrument +lead-4-chiff+ "lead 4 (chiff)")
(define-instrument +lead-5-charang+ "lead 5 (charang)")
(define-instrument +lead-6-voice+ "lead 6 (voice)")
(define-instrument +lead-7-fifths+ "lead 7 (fifths)")
(define-instrument +lead-8-bass+lead+ "lead 8 (bass+lead)")
(define-instrument +marimba+ "marimba")
(define-instrument +melodic-tom+ "melodic tom")
(define-instrument +music-box+ "music box")
(define-instrument +muted-trumpet+ "muted trumpet")
(define-instrument +oboe+ "oboe")
(define-instrument +ocarina+ "ocarina")
(define-instrument +orchestra-hit+ "orchestra hit")
(define-instrument +orchestral-harp+ "orchestral harp")
(define-instrument +overdriven-guitar+ "overdriven guitar")
(define-instrument +pad-1-new-age+ "pad 1 (new age)")
(define-instrument +pad-2-warm+ "pad 2 (warm)")
(define-instrument +pad-3-polysynth+ "pad 3 (polysynth)")
(define-instrument +pad-4-choir+ "pad 4 (choir)")
(define-instrument +pad-5-bowed+ "pad 5 (bowed)")
(define-instrument +pad-6-metallic+ "pad 6 (metallic)")
(define-instrument +pad-7-halo+ "pad 7 (halo)")
(define-instrument +pad-8-sweep+ "pad 8 (sweep)")
(define-instrument +pan-flute+ "pan flute")
(define-instrument +percussive-organ+ "percussive organ")
(define-instrument +piccolo+ "piccolo")
(define-instrument +pizzicato-strings+ "pizzicato strings")
(define-instrument +recorder+ "recorder")
(define-instrument +reed-organ+ "reed organ")
(define-instrument +reverse-cymbal+ "reverse cymbal")
(define-instrument +rock-organ+ "rock organ")
(define-instrument +seashore+ "seashore")
(define-instrument +shakuhachi+ "shakuhachi")
(define-instrument +shamisen+ "shamisen")
(define-instrument +shanai+ "shanai")
(define-instrument +sitar+ "sitar")
(define-instrument +slap-bass-1+ "slap bass 1")
(define-instrument +slap-bass-2+ "slap bass 2")
(define-instrument +soprano-sax+ "soprano sax")
(define-instrument +steel-drums+ "steel drums")
(define-instrument +string-ensemble-1+ "string ensemble 1")
(define-instrument +string-ensemble-2+ "string ensemble 2")
(define-instrument +synth-bass-1+ "synth bass 1")
(define-instrument +synth-bass-2+ "synth bass 2")
(define-instrument +synth-drum+ "synth drum")
(define-instrument +synth-voice+ "synth voice")
(define-instrument +synthbrass-1+ "synthbrass 1")
(define-instrument +synthbrass-2+ "synthbrass 2")
(define-instrument +synthstrings-1+ "synthstrings 1")
(define-instrument +synthstrings-2+ "synthstrings 2")
(define-instrument +taiko-drum+ "taiko drum")
(define-instrument +telephone-ring+ "telephone ring")
(define-instrument +tenor-sax+ "tenor sax")
(define-instrument +timpani+ "timpani")
(define-instrument +tinkle-bell+ "tinkle bell")
(define-instrument +tremolo-strings+ "tremolo strings")
(define-instrument +trombone+ "trombone")
(define-instrument +trumpet+ "trumpet")
(define-instrument +tuba+ "tuba")
(define-instrument +tubular-bells+ "tubular bells")
(define-instrument +vibraphone+ "vibraphone")
(define-instrument +viola+ "viola")
(define-instrument +violin+ "violin")
(define-instrument +voice-oohs+ "voice oohs")
(define-instrument +whistle+ "whistle")
(define-instrument +woodblock+ "woodblock")
(define-instrument +xylophone+ "xylophone")