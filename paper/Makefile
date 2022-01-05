# The MIT License (MIT)
#
# Copyright (c) 2020-2021 Yegor Bugayenko
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

.SHELLFLAGS = -e -x -c
.ONESHELL:

TLROOT=/usr/local/texlive/2021/texmf-dist
PACKAGES=ffcode to-be-determined
REPO=cqfn/eo
LATEXOPTS=-shell-escape -interaction=nonstopmode -halt-on-error

zip: *.tex sections/*.tex
	rm -rf package
	mkdir package
	cd package
	cp ../eolang-paper.tex .
	cp ../main.bib .
	cp ../eo.sty .
	for p in $(PACKAGES); do cp $(TLROOT)/tex/latex/$${p}/$${p}.sty .; done
	cp -r ../sections .
	version=$$(curl --silent -H "Accept: application/vnd.github.v3+json" https://api.github.com/repos/$(REPO)/releases/latest | jq -r '.tag_name')
	echo "Version is: $${version}"
	date=$$(date +%Y/%m/%d)
	echo "Date is: $${date}"
	hash=$$(git log -n 1 --pretty='format:%h')
	echo "Hash is: $${hash}"
	gsed -i "s|0\.0\.0|$${version}|" eolang-paper.tex
	gsed -i "s|00\.00\.0000|$${date}|" eolang-paper.tex
	gsed -i "s|00000000|$${hash}|" eolang-paper.tex
	pdflatex $(LATEXOPTS) eolang-paper.tex >/dev/null
	bibtex eolang-paper
	pdflatex $(LATEXOPTS) eolang-paper.tex >/dev/null
	pdflatex $(LATEXOPTS) eolang-paper.tex >/dev/null
	rm -rf _minted-* *.aux *.bcf *.blg *.fdb_latexmk *.fls *.log *.run.xml *.out *.exc
	zip -x eolang-paper.pdf -r paper-$${version}.zip *
	mv paper-$${version}.zip ..
	cd ..

clean:
	git clean -dfX

