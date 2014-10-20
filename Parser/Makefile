all: 
	happy -gca ParVar.y
	alex -g LexVar.x
	ghc --make TestVar.hs -o TestVar

doc: DocVar.pdf

DocVar.pdf: DocVar.tex
	pdflatex DocVar.tex

clean: 
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocVar.ps

distclean: clean
	-rm -f DocVar.* LexVar.* ParVar.* LayoutVar.* SkelVar.* PrintVar.* TestVar.* AbsVar.* TestVar ErrM.* SharedString.* ComposOpVar.* Var.dtd XMLVar.* Makefile*
	

