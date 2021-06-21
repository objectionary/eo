.SHELLFLAGS = -e -x -c

.ONESHELL:

all: eolang-paper.pdf

clean:
	rm -rf .DS_Store *.aux *.bbl *.bcf *.blg *.fdb_latexmk *.fls *.log *.run.xml *.pdf *.out
	rm -rf _minted-*

%.pdf: %.tex main.bib *.tex
	latexmk -pdf $<
	texqc --ignore 'You have requested document class' $< \
		--ignore 'Token not allowed in a PDF string'
	texsc $<
	biblint check main.bib