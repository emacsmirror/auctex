
infodir=/usr/local/info
TEX=tex

all: preview-latex.dvi preview-latex

dist: preview-latex INSTALL README preview-latex_toc.html preview-latex.ps

preview-latex_toc.html: preview-latex.texi
	texi2html -split_node preview-latex.texi

preview-latex.dvi: preview-latex.texi
	-$(TEX) "\nonstopmode\input preview-latex.texi"
        -texindex preview-latex.ky
        -texindex preview-latex.fn
        -texindex preview-latex.vr
	-texindex preview-latex.cp
	-$(TEX) "\nonstopmode\input preview-latex.texi"

preview-latex.ps: preview-latex.dvi
	dvips preview-latex.dvi -o preview-latex.ps

preview-latex: preview-latex.texi install.texi readme.texi copying.texi
	-makeinfo preview-latex.texi

INSTALL: install.texi
	-makeinfo --no-headers install.texi --output INSTALL

README: readme.texi
	-makeinfo --no-headers readme.texi --output README

install: preview-latex
	cp preview-latex preview-latex-* $(infodir)
	@echo "**********************************************************"
	@echo "** The \`preview-latex' info files are now installed."
	@echo "** Edit \`$(infodir)/dir'"
	@echo "** to add an entry for file \`preview-latex'"
	@echo "**********************************************************"

clean: 
	rm -f *.dvi *.ps *.aux *.cp *.fn *.info *.ky *.log *~ \#*\# *.tp *.vr *.pg *.toc *.tp *.bak *.cps *.kys *.tps *.fns *.vrs *.pgs preview-latex preview-latex-? *.html INSTALL README ChangeLog



