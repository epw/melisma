WWW = $(HOME)/www/melisma

main: melisma.py melisma_test.py
	python melisma_test.py
	python melisma.py > music.ly
	lilypond music.ly
	timidity -Ov music.midi
	cp music.pdf music.ogg $(WWW)/.
