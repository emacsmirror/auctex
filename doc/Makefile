
INFODIR=$GNUHOME/info

all: auc-tex.dvi ref-card.dvi auc-tex.info

ref-card.dvi: ref-card.tex
	tex ref-card
	@echo "**********************************************************"
	@echo "** doc/ref-card.dvi may now be printed (1 page)"
	@echo "**********************************************************"

auc-tex.dvi: auc-tex.texi
	tex auc-tex.texi
	tex auc-tex.texi
	@echo "**********************************************************"
	@echo "** doc/auc-tex.dvi may now be printed (20 pages)"
	@echo "**********************************************************"

auc-tex.info: auc-tex.texi
	makeinfo auc-tex.texi

install: auc-tex.info
	cp auc-tex.info $(INFODIR)
	@echo "**********************************************************"
	@echo "** auc-tex.info may now be installed"
	@echo "** Edit \`$(INFODIR)/dir'"
	@echo "** to reflect the file \`auc-tex.info'"
	@echo "**********************************************************"

clean: allways
	rm -f *.dvi *.ps *.aux *.cp *.fn *.info *.ky *.log *~ \#*\# *.tp *.vr *.pg *.toc *.tp *.bak

allways:
