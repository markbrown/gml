TEXINPUTS = $(wildcard *.tex)
PARTS = {intro,gml,vm,compiling,rendering,exercises}.html

.PHONY: install
install: all
	mkdir -p ../pdf
	mkdir -p ../html
	cp gml-scala-example.pdf ../pdf
	cp gml-scala-example.html ../html
	cp index.html ../html
	cp $(PARTS) ../html
	cp *_motif.gif ../html

.PHONY: all
all: gml-scala-example.pdf gml-scala-example.html index.html

gml-scala-example.pdf: $(TEXINPUTS)
	pdflatex gml-scala-example.tex
	pdflatex gml-scala-example.tex
	pdflatex gml-scala-example.tex

gml-scala-example.html: $(TEXINPUTS)
	hevea -o gml-scala-example.html gml-scala-example-hevea.tex
	hevea -o gml-scala-example.html gml-scala-example-hevea.tex
	hevea -o gml-scala-example.html gml-scala-example-hevea.tex

index.html: gml-scala-example.html
	hacha -tocbis $<

.PHONY: clean
clean:
	rm -f *.aux *.lof *.log *.toc
	rm -f *.haux *.htoc

.PHONY: realclean
realclean: clean
	rm -f gml-scala-example.{css,html,pdf}
	rm -f index.html
	rm -f $(PARTS)
	rm -f {contents,next,previous}_motif.gif

