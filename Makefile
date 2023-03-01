.PHONY: all, clean

all: probgen-2022-poster.pdf

probgen-2022-poster.pdf : refs.bib

clean: 
	-rm *.aux *.log *.bbl *.blg

%.pdf : %.tex %.bbl
	while ( pdflatex $<;  grep -q "Rerun to get" $*.log ) do true ; done

%.aux : %.tex
	-pdflatex $<

%.bbl : %.aux
	bibtex $<

%.png : %.pdf
	convert -density 300 $< -flatten $@

%.pdf : %.svg
	inkscape $< --export-pdf=$@

%.pdf : %.ink.svg
	inkscape $< --export-pdf=$@
