
infodir=/usr/local/info

all: auc-tex.dvi ref-card.dvi auc-tex.info

dist: auc-tex.info

ref-card.dvi: ref-card.tex
	tex ref-card
	@echo "**********************************************************"
	@echo "** doc/ref-card.dvi may now be printed (1 page)"
	@echo "**********************************************************"

auc-tex.dvi: auc-tex.texi
	-tex auc-tex.texi
	-texindex auc-tex.ky
	-texindex auc-tex.fn
	-texindex auc-tex.vr
	-texindex auc-tex.cp
	-tex auc-tex.texi
	@echo "**********************************************************"
	@echo "** If this gave trouble, maybe you need to install
	@echo "** a newer version of texinfo?
	@echo "** doc/auc-tex.dvi may now be printed (35 pages)"
	@echo "**********************************************************"

auc-tex.info: auc-tex.texi
	-makeinfo auc-tex.texi
	@echo "**********************************************************"
	@echo "** If this gave trouble, maybe you need to install
	@echo "** a newer version of texinfo?
	@echo "**********************************************************"

install: auc-tex.info
	cp auc-tex.info* $(infodir)
	@echo "**********************************************************"
	@echo "** auc-tex.info may now be installed"
	@echo "** Edit \`$(infodir)/dir'"
	@echo "** to reflect the file \`auc-tex.info'"
	@echo "**********************************************************"

clean: allways
	rm -f *.dvi *.ps *.aux *.cp *.fn *.info *.ky *.log *~ \#*\# *.tp *.vr *.pg *.toc *.tp *.bak *.cps *.kys *.tps *.fns *.vrs *.pgs *.info-*


allways:
