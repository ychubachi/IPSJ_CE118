FILENAME  = document
TEX       = $(FILENAME).tex
IDX       = $(FILENAME).idx
DVI       = $(FILENAME).dvi
PDF       = $(FILENAME).pdf

distclean:
	make clean
clean:
	rm -f *.aux *.dvi *.idx *.ilg *.ind *.log *.out *.toc *.bbl *.blg *~ *.synctex.gz $(PDF) R/figure/*.xbb
