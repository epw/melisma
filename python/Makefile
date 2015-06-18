WWW = $(HOME)/www/melisma

melisma: melisma.ogg

melisma.ogg: melisma.py melisma_test.py
	python melisma_test.py
	synthesize.sh melisma.py
	cp melisma.pdf melisma.ogg $(WWW)/.

what_ive_done: what_ive_done.ogg

what_ive_done.ogg: melisma.ogg what_ive_done.py
	synthesize.sh what_ive_done.py
	cp what_ive_done.pdf what_ive_done.ogg $(WWW)/.
