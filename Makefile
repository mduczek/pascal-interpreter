all: 
	ghc --make -iStatic:Runtime:Parser Main.hs -o interpreter

clean:
	-rm Runtime/*.hi Runtime/*.o
	-rm Parser/*.hi Parser/*.o
	-rm Static/*.hi Static/*.o
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocVar.ps
