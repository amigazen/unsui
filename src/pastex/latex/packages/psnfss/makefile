TEXMF=/usr/local/TeX/lib
TEX=tex
LATEX=latex

default: psfonts 

all: psfonts adobe lucida extra

extra:  charter nimbus utopia

psfonts:
	$(TEX) psfonts.ins 
	$(LATEX) psfonts.dtx

adobe:
	$(TEX) adobe.ins 
	$(LATEX) adobe.dtx

lucida:
	$(TEX) lucida.ins 
	$(LATEX) lucida.dtx

charter:
	$(TEX) charter.ins
	$(LATEX) charter.dtx

nimbus:
	$(TEX) nimbus.ins
	$(LATEX) nimbus.dtx

utopia:
	$(TEX) utopia.ins
	$(LATEX) utopia.dtx

clean:
	-rm *.fd *.sty *.log *.dvi *.idx *.aux *.toc answer *.tmp  lucfont.tex

install:
	-mkdir -p ${TEXMF}/tex/latex/psnfss
	/bin/mv *.fd *.sty ${TEXMF}/tex/latex/psnfss



