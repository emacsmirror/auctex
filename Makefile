# Makefile - for the AUC TeX distribution.
#
# $Id: Makefile,v 5.115 1995-02-14 19:43:53 amanda Exp $
#
# Edit the makefile, type `make', and follow the instructions.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

# Where local software is found
prefix=/usr/local

# Where architecture dependent local software go
exec_prefix = $(prefix)

# Where installed binaries go.
bindir = $(exec_prefix)/bin

# Where info files go.
infodir = $(prefix)/info

# Where manual pages go.
mandir=$(prefix)/man/man1

# Where the AUC TeX emacs lisp files go.
# Set this to "." to specify current directory.
#
# Make sure that this is the same directory as specified by
# TeX-lisp-directory in tex-site.el
#
aucdir=$(prefix)/lib/emacs/site-lisp/auctex

# Name of your emacs binary
EMACS=emacs-19.28

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Do not change the definition of autodir below, unless you also
# update TeX-auto-global in tex-init.el

# Where the automatically generated lisp files for your site go.
autodir=$(aucdir)/auto

# Using emacs in batch mode.
BATCH=$(EMACS) -batch -q lpath.el -f eval-current-buffer

# Specify the byte-compiler for compiling AUC TeX files
ELC= $(BATCH) -f batch-byte-compile

# Specify the byte-compiler for generating style files
AUTO= $(EMACS) -batch -l $(aucdir)/tex.elc \
	-l $(aucdir)/latex.elc -f TeX-auto-generate-global

# Specify the byte-compiler for compiling generated style files
AUTOC= $(ELC)

# Using TeX in batch mode.
TEX=tex

# Need K&R compiler.  Either `cc' or `gcc -traditional'
#CC = gcc -traditional
CC = cc

# Cflags.. Include `-DNEED_STRSTR' if you don't have strstr() in libc
CFLAGS = -O # -DNEED_STRSTR 

# How to run flex. (flex is needed, lex won't do)
# if you don't have `flex' you may use the other instead:

LEX = flex -8  lacheck.lex
#LEX = cp lacheck.noflex.c lex.yy.c 

# How to move the byte compiled files to their destination.  
MV = mv

# For OS/2 use
#LACHECK=lacheck.exe
LACHECK=lacheck

# For linking lacheck
LIBS=

##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

SHELL = /bin/sh

FTPDIR = /pack/ftp/pub/emacs-lisp/alpha

REMOVE =  double.el min-ispl.el min-map.el  ltx-math.el 

MINMAPSRC = column.el   auc-menu.el maniac.el \
	    outln-18.el auc-html.el easymenu.el \
	    xt-mouse.el all.el  cpp.el

CONTRIB = hilit-LaTeX.el bib-cite.el tex-jp.el 

AUCSRC = auc-old.el  tex.el \
	 tex-buf.el  latex.el    tex-info.el \
	 ltx-help.el easymenu.el auc-menu.el

STYLESRC = style/slides.el    style/foils.el    style/amstex.el \
	   style/article.el   style/book.el     style/letter.el \
	   style/report.el    style/amsart.el   style/amsbook.el \
	   style/epsf.el      style/psfig.el    style/latexinfo.el \
	   style/dutch.el     style/german.el   style/dk.el \
	   style/j-article.el style/j-book.el   style/j-report.el \
	   style/jarticle.el  style/jbook.el    style/jreport.el \
	   style/dinbrief.el  style/virtex.el   style/plfonts.el \
	   style/plhb.el      style/harvard.el

LACHECKFILES = lacheck/Makefile lacheck/lacheck.lex lacheck/lacheck.man \
	       lacheck/test.tex

LACHECKGEN = lacheck.c test.old

DOCFILES = doc/Makefile doc/auc-tex.texi doc/intro.texi doc/install.texi \
	doc/changes.texi doc/tex-ref.tex doc/math-ref.tex doc/history.texi

EXTRAFILES = COPYING PROBLEMS MSDOS VMS OS2 Makefile ChangeLog \
	lpath.el tex-site.el $(CONTRIB)

first:
	@echo ""
	@echo "	 ** Welcome to the AUC TeX installation suite **"
	@echo ""
	@echo "	 Edit the Makefile to suit your needs. Then run:"
	@echo 
	@echo "	  make all"
	@echo 
	@echo "	 and follow the instructions."
	@echo 
	@echo "	 Before you start, you should check that you have"
	@echo "	 TeXinfo package 2.16 or later installed (the texinfo.tex"
	@echo "	 file from 2.16 has version 2.86).  The version of"
	@echo "	 TeXinfo distributed with GNU Emacs 18.xx is TeXinfo 1."
	@echo

all: main
	@echo "**********************************************************"
	@echo "** Before running \`make install' you should edit the "
	@echo "** file \`tex-site.el' in this directory to suit your"
	@echo "** local needs.  Then run: \`make install'"
	@echo "** "
	@echo "** Expect some warnings from the Emacs 19 byte compiler."
	@echo "**********************************************************"

main: TDoc TLaCheck

install: LispInstall LaInstall DocInstall
	@echo 
	@echo "**********************************************************"
	@echo "** AUC TeX installation almost completed "
	@echo "** "
	@echo "** Now copy \`tex-site.el' to the directory where you put "
	@echo "** local lisp extensions (usually emacs/site-lisp) and "
	@echo "** insert"
	@echo "**   (require 'tex-site)"
	@echo "** in your \`.emacs' or \`site-start.el' file."
	@echo "** "
	@echo "** Still missing is the automatic extraction of symbols"
	@echo "** and environments from your sites TeX style files."
	@echo "** Beware, this takes some time and uses around 300k"
	@echo "** storage, depending on your the TeX style files. "
	@echo "** It is possible to use AUC TeX without this information."
	@echo "** "
	@echo "** To do this start emacs and type"
	@echo "** \"M-x TeX-auto-generate-global RET\"."
	@echo "** INPORTANT:  You must install tex-site.el first!"
	@echo "** "
	@echo "** You may want to print the following files:  "
	@echo "**    doc/auc-tex.dvi"
	@echo "**    doc/tex-ref.dvi"
	@echo "**********************************************************"
	@echo

install-auto:
	@echo "Use \"M-x TeX-auto-generate-global RET\" instead."

LaInstall: TLaCheck
	@echo "**********************************************************"
	@echo "** Installing LaCheck "
	@echo "**********************************************************"
	-(cd lacheck; $(MAKE) install bindir=$(bindir) mandir=$(mandir) \
	LACHECK=$(LACHECK) SHELL=$(SHELL))

DocInstall: 
	@echo "**********************************************************"
	@echo "** Preparing AUC TeX \`info' pages"
	@echo "**********************************************************"
	-(cd doc; $(MAKE) install infodir=$(infodir) TEX=$(TEX))

LispInstall:
	@echo "**********************************************************"
	@echo "** Byte compiling AUC TeX.  This may take a while..."
	@echo "** "
	@echo "** Expect some harmless warnings about free variables and "
	@echo "** undefined functions from the Emacs 19 byte compiler."
	@echo "**********************************************************"
	$(ELC) $(AUCSRC) $(STYLESRC)
	if [ ! -d $(aucdir) ]; then mkdir $(aucdir); else true; fi ; 
	if [ `/bin/pwd` != `(cd $(aucdir) && /bin/pwd)` ] ; \
	then \
	    if [ ! -d $(aucdir)/style ]; then mkdir $(aucdir)/style; \
	                                 else true; fi ; \
	    $(MV) *.elc $(aucdir) ; \
	    $(MV) style/*.elc $(aucdir)/style ; \
	else true; \
	fi

TLaCheck:
	@echo "**********************************************************"
	@echo "** Building LaCheck"
	@echo "**********************************************************"
	-( cd lacheck; $(MAKE) bindir=$(bindir) \
	   CC="$(CC)" CFLAGS="$(CFLAGS)" LEX="$(LEX)" \
	   LACHECK=$(LACHECK) LIBS=$(LIBS))

TDoc: 
	@echo "**********************************************************"
	@echo "** Making AUC TeX documentation"
	@echo "**********************************************************"
	-(cd doc; $(MAKE) TEX=$(TEX))

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex
	(cd doc; $(MAKE) clean)
	(cd lacheck; $(MAKE) clean)

wc:
	wc $(AUCSRC) $(STYLESRC) 

dist:	
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making distribution of auctex for release $(TAG)"
	@echo "**********************************************************"
	if [ -d auctex-$(TAG) ]; then rm -r auctex-$(TAG) ; fi
	rm -f /user/amanda/lib/www/auctex/version
	echo $(TAG) > /user/amanda/lib/www/auctex/version
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
	-(cd lacheck; cvs add `echo $(LACHECKFILES) | sed -e s@lacheck/@@g` )
	-(cd style; cvs add `echo $(STYLESRC) | sed -e s@style/@@g` )
	cvs commit -m "Release $(TAG)"
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
	mkdir auctex-$(TAG) 
	mkdir auctex-$(TAG)/style
	mkdir auctex-$(TAG)/doc auctex-$(TAG)/lacheck
	cp $(AUCSRC) $(EXTRAFILES) auctex-$(TAG)
	cp $(STYLESRC) auctex-$(TAG)/style
	cp $(LACHECKFILES) auctex-$(TAG)/lacheck
	(cd lacheck; $(MAKE) $(LACHECKGEN); \
	 cp $(LACHECKGEN) ../auctex-$(TAG)/lacheck )
	cp $(DOCFILES)  auctex-$(TAG)/doc
	(cd doc; $(MAKE) dist; cp auctex auctex-* ../auctex-$(TAG)/doc )
	(cd doc; cp INSTALLATION README CHANGES ../auctex-$(TAG)/ )
	cp doc/CHANGES $(FTPDIR)/CHANGES-$(TAG)
	cp ChangeLog $(FTPDIR)
	cp doc/*.html /user/amanda/lib/www/auctex/alpha-doc
	rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
	rm -f $(FTPDIR)/auctex.tar.Z $(FTPDIR)/auctex.zip
	tar -cf - auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
	tar -cf - auctex-$(TAG) | compress > $(FTPDIR)/auctex.tar.Z
	zip -r $(FTPDIR)/auctex auctex-$(TAG)
	(cd $(FTPDIR); ln -s auctex-$(TAG).tar.gz auctex.tar.gz)
	if [ "X$(OLD)" = "X" ]; then echo "No patch"; else \
	cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
	          -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex \
		> $(FTPDIR)/auctex-$(OLD)-to-$(TAG).patch ; fi ; exit 0

patch:
	cvs rdiff -r release_`echo $(OLD) | sed -e 's/[.]/_/g'` \
	          -r release_`echo $(TAG) | sed -e 's/[.]/_/g'` auctex

min-map:
	-cvs add $(MINMAPSRC) 
	cvs commit -m "Update"
	cp $(MINMAPSRC) doc/math-ref.tex $(FTPDIR) 
