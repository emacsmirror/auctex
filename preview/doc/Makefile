
infodir=/usr/local/info
TEX=tex

all: preview-latex.dvi preview-latex

dist: preview-latex INSTALL README preview-latex_toc.html preview-latex.ps

preview-latex/index.html: preview-latex.texi install.texi readme.texi \
	copying.texi problems.texi
	makeinfo --html preview-latex.texi
	# Small manual: use 'makeinfo --html' ????
	# texi2html goofs terribly on macros.
html-docs: preview-latex/index.html

preview-latex.dvi: preview-latex.texi install.texi readme.texi copying.texi \
	problems.texi
	-$(TEX) "\nonstopmode\input preview-latex.texi"
	-texindex preview-latex.cp
	-$(TEX) "\nonstopmode\input preview-latex.texi"

preview-latex.ps: preview-latex.dvi
	dvips preview-latex.dvi -o preview-latex.ps

preview-latex.info: preview-latex.texi install.texi readme.texi copying.texi \
	problems.texi
	-makeinfo preview-latex.texi

INSTALL: install.texi
	-makeinfo --no-headers --no-validate install.texi --output INSTALL

README: readme.texi
	-makeinfo --no-headers --no-validate readme.texi --output README

PROBLEMS: problems.texi
	-makeinfo --no-headers --no-validate problems.texi --output PROBLEMS

install: preview-latex.info
	cp preview-latex.info preview-latex-*.info $(infodir)
	@echo "**********************************************************"
	@echo "** The \`preview-latex' info files are now installed."
	@echo "** Edit \`$(infodir)/dir'"
	@echo "** to add an entry for file \`preview-latex'"
	@echo "**********************************************************"
	# install-info should be used.

clean: 
	rm -f *.dvi *.ps *.aux *.cp *.fn *.info *.ky *.log *~ \#*\# *.tp *.vr *.pg *.toc *.tp *.bak *.cps *.kys *.tps *.fns *.vrs *.pgs preview-latex preview-latex-? *.html INSTALL README PROBLEMS




