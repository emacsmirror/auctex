# Makefile - for the AUC TeX distribution.
#
# Maintainer: Per Abrahamsen <auc-tex@sunsite.auc.dk>
# Version: 9.7h
#
# Edit the makefile, type `make', and follow the instructions.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

# Where local software is found
prefix=/usr/local

# Where info files go.
infodir = $(prefix)/info

# Where local lisp files go.
lispdir = $(prefix)/share/emacs/site-lisp

# Where the AUC TeX emacs lisp files go.
aucdir=$(lispdir)/auctex

# Name of your emacs binary
EMACS=emacs

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Do not change the definition of autodir below, unless you also
# update TeX-auto-global in tex-init.el

# Where the automatically generated lisp files for your site go.
autodir=$(aucdir)/auto

# Using emacs in batch mode.
BATCH=$(EMACS) -batch -q -l lpath.el

# Specify the byte-compiler for compiling AUC TeX files
ELC= $(BATCH) -f batch-byte-compile

# Specify the byte-compiler for generating style files
AUTO= $(EMACS) -batch -l $(aucdir)/tex.elc \
	-l $(aucdir)/latex.elc -f TeX-auto-generate-global

# Specify the byte-compiler for compiling generated style files
AUTOC= $(ELC)

# How to move the byte compiled files to their destination.  
MV = mv

# How to copy the lisp files to their distination.
CP = cp -p

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

SHELL = /bin/sh

FTPDIR = /home/ftp/pub/Staff/Per.Abrahamsen/auctex
#FTPDIR = /home/ftp/pub/Staff/Per.Abrahamsen/mirror/ftp/auctex

WWWDIR = $(HOME)/.public_html/auctex
#WWWDIR = /home/ftp/pub/Staff/Per.Abrahamsen/mirror/www/auctex

REMOVE =  ltx-help.el

MINMAPSRC = auc-menu.el maniac.el outln-18.el all.el multi-prompt.el

CONTRIB = hilit-LaTeX.el bib-cite.el tex-jp.el font-latex.el
CONTRIBELC = bib-cite.elc font-latex.elc

AUCSRC = auc-old.el tex.el tex-buf.el latex.el tex-info.el multi-prompt.el
AUCELC = auc-old.elc tex.elc tex-buf.elc latex.elc tex-info.elc \
	multi-prompt.elc


STYLESRC = style/slides.el    style/foils.el    style/amstex.el \
	   style/article.el   style/book.el     style/letter.el \
	   style/report.el    style/amsart.el   style/amsbook.el \
	   style/epsf.el      style/psfig.el    style/latexinfo.el \
	   style/dutch.el     style/german.el   style/dk.el \
	   style/j-article.el style/j-book.el   style/j-report.el \
	   style/jarticle.el  style/jbook.el    style/jreport.el \
	   style/dinbrief.el  style/virtex.el   style/plfonts.el \
	   style/plhb.el      style/harvard.el	style/swedish.el

DOCFILES = doc/Makefile doc/auc-tex.texi doc/intro.texi doc/install.texi \
	doc/changes.texi doc/tex-ref.tex doc/math-ref.tex doc/history.texi

EXTRAFILES = COPYING PROBLEMS MSDOS VMS OS2 WIN-NT Makefile ChangeLog \
	lpath.el tex-site.el $(CONTRIB)

all:	lisp

lisp:
	$(ELC) $(AUCSRC) $(STYLESRC)

install:	install-lisp

tex.elc:	tex.el
	$(ELC) $(AUCSRC) $(STYLESRC)

contrib:
	$(ELC) bib-cite.el
	$(ELC) font-latex.el
# 	$(ELC) tex-jp.el              # Doesn't compile without MULE
# 	$(ELC) hilit-LaTeX.el         # Doesn't compile without X

install-lisp:	tex.elc
	if [ ! -d $(lispdir) ]; then mkdir $(lispdir); else true; fi ;
	if [ -f $(lispdir)/tex-site.el ]; \
	then \
	    echo "Leaving old tex-site.el alone."; \
	else \
	    sed -e 's#@AUCDIR#$(aucdir)/#' tex-site.el \
	    > $(lispdir)/tex-site.el ; \
        fi
	if [ ! -d $(aucdir) ]; then mkdir $(aucdir); else true; fi ; 
	if [ `/bin/pwd` != `(cd $(aucdir) && /bin/pwd)` ] ; \
	then \
	    if [ ! -d $(aucdir)/style ]; then mkdir $(aucdir)/style; \
	                                 else true; fi ; \
	    $(MV) $(AUCELC) $(aucdir) ; \
	    $(MV) style/*.elc $(aucdir)/style ; \
	    $(CP) $(AUCSRC) $(aucdir) ; \
	    $(CP) style/*.el $(aucdir)/style ; \
	else \
	    echo "Leaving compiled files in place."; \
	fi

install-contrib:
	$(MV) $(CONTRIBELC) $(aucdir)
	$(MV) bib-cite.elc $(aucdir)
	$(MV) font-latex.elc $(aucdir)
	$(CP) $(CONTRIB) $(aucdir)

install-info:
	-(cd doc; $(MAKE) install infodir=$(infodir))


install-auto:
	@echo "Use \"M-x TeX-auto-generate-global RET\" instead."


.el.elc:
	$(ELC) $<

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex
	(cd doc; $(MAKE) clean)

wc:
	wc $(AUCSRC) $(STYLESRC) 

dist:	
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	if [ "X$(OLD)" = "X" ]; then echo "No patch"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making distribution of auctex for release $(TAG)"
	@echo "**********************************************************"
	if [ -d auctex-$(TAG) ]; then rm -r auctex-$(TAG) ; fi
	rm -f $(WWWDIR)/version
	echo $(TAG) > $(WWWDIR)/version
	perl -pi.bak -e "s/Version: $(OLD)/Version: $(TAG)/" \
	    $(AUCSRC) $(EXTRAFILES)
	mv ChangeLog ChangeLog.old
	echo `date "+%a %b %d %T %Y "` \
	     " Per Abrahamsen  <abraham@dina.kvl.dk>" > ChangeLog
	echo >> ChangeLog
	echo "	* Version" $(TAG) released. >> ChangeLog
	echo >> ChangeLog
	cat ChangeLog.old >> ChangeLog
	cvs commit -m "Release $(OLD)++" tex.el
	rm -f tex.el.orig
	mv tex.el tex.el.orig
	sed -e '/defconst AUC-TeX-date/s/"[^"]*"/"'"`date`"'"/' \
	    -e '/defconst AUC-TeX-version/s/"[^"]*"/"'$(TAG)'"/' \
	    < tex.el.orig > tex.el
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	-cvs add $(AUCSRC) $(EXTRAFILES)
	-(cd doc; cvs add `echo $(DOCFILES) | sed -e s@doc/@@g` )
	-(cd style; cvs add `echo $(STYLESRC) | sed -e s@style/@@g` )
	cvs commit -m "Release $(TAG)"
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
	mkdir auctex-$(TAG) 
	mkdir auctex-$(TAG)/style
	mkdir auctex-$(TAG)/doc 
	cp $(AUCSRC) $(EXTRAFILES) auctex-$(TAG)
	cp $(STYLESRC) auctex-$(TAG)/style
	cp $(DOCFILES)  auctex-$(TAG)/doc
	(cd doc; $(MAKE) dist; cp auctex auctex-* ../auctex-$(TAG)/doc )
	(cd doc; cp INSTALLATION README CHANGES ../auctex-$(TAG)/ )
	cp doc/CHANGES $(FTPDIR)/CHANGES-$(TAG)
	cp doc/auc-tex.ps $(FTPDIR)
	cp ChangeLog $(FTPDIR)
	cp doc/*.html $(WWWDIR)/doc
	rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
	rm -f $(FTPDIR)/auctex.tar.Z $(FTPDIR)/auctex.zip
	tar -cf - auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
	tar -cf - auctex-$(TAG) | compress > $(FTPDIR)/auctex.tar.Z
	zip -r $(FTPDIR)/auctex auctex-$(TAG)
	(cd $(FTPDIR); ln -s auctex-$(TAG).tar.gz auctex.tar.gz)
	cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
	          -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex \
		> $(FTPDIR)/auctex-$(OLD)-to-$(TAG).patch ;  exit 0

patch:
	cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
	          -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex

min-map:
	-cvs add $(MINMAPSRC) 
	cvs commit -m "Update"
	cp $(MINMAPSRC) doc/math-ref.tex $(FTPDIR) 
