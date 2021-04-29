all: article.pdf

install:
	gem install texsc
	gem install texqc

fast:
	pdflatex --shell-escape article.tex

clean:
	rm -rf .DS_Store *.aux *.bbl *.bcf *.blg *.fdb_latexmk *.fls *.log *.run.xml *.pdf *.out
	rm -rf _minted-article

%.pdf: %.tex main.bib *.tex
	latexmk -pdf $<
	texqc --ignore 'You have requested document class' $< \
		--ignore 'Token not allowed in a PDF string' \
		--ignore '\\vspace should only be used to provide space'
	texsc $<
	biblint check main.bib