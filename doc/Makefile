
infodir=/usr/local/info
TEX=tex

all: auc-tex.dvi ref-card.dvi math-ref.dvi auctex

dist: auctex auc-tex_toc.html

auc-tex_toc.html: auc-tex.texi
	texi2html -split_node auc-tex.texi

math-ref.dvi: math-ref.tex
	$(TEX) math-ref
	@echo "**********************************************************"
	@echo "** doc/math-ref.dvi may now be printed (1 page)"
	@echo "**********************************************************"

ref-card.dvi: ref-card.tex
	$(TEX) ref-card
	@echo "**********************************************************"
	@echo "** doc/ref-card.dvi may now be printed (1 page)"
	@echo "**********************************************************"

auc-tex.dvi: auc-tex.texi
	-$(TEX) "\nonstopmode\input auc-tex.texi"
	-texindex auc-tex.ky
	-texindex auc-tex.fn
	-texindex auc-tex.vr
	-texindex auc-tex.cp
	-$(TEX) "\nonstopmode\input auc-tex.texi"
	@echo "**********************************************************"
	@echo "** If this gave trouble, maybe you need to install"
	@echo "** a newer version of texinfo?"
	@echo "** doc/auc-tex.dvi may now be printed (70+ pages)"
	@echo "**********************************************************"

auctex: auc-tex.texi
	-makeinfo auc-tex.texi
	@echo "**********************************************************"
	@echo "** If this gave trouble, maybe you need to install"
	@echo "** a newer version of texinfo?"
	@echo "**********************************************************"

install: auctex
	cp auctex auctex-* $(infodir)
	@echo "**********************************************************"
	@echo "** The \`auctex' info files are now installed."
	@echo "** Edit \`$(infodir)/dir'"
	@echo "** to add an entry for file \`auctex'"
	@echo "**********************************************************"

clean: allways
	rm -f *.dvi *.ps *.aux *.cp *.fn *.info *.ky *.log *~ \#*\# *.tp *.vr *.pg *.toc *.tp *.bak *.cps *.kys *.tps *.fns *.vrs *.pgs *.info-* *.html


allways:
