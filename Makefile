# Makefile - for the AUC TeX distribution.
#
# $Id: Makefile,v 5.84 1994-03-17 18:37:47 amanda Exp $
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
EMACS=emacs-19.22

# For OS/2 use
#LACHECK=lacheck.exe
LACHECK=lacheck

# For gcc use
LIBS=
#LIBS=-ll

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
AUTO= $(EMACS) -batch -q -l $(aucdir)/tex-site.elc -l $(aucdir)/tex-auto.elc -f TeX-auto-generate-global

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


##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

SHELL = /bin/sh

FTPDIR = /pack/ftp/pub/emacs-lisp/alpha

REMOVE =  ISO-TEX

MINMAPSRC = min-ispl.el column.el   auc-html.el double.el \
	    powerkey.el easymenu.el min-map.el  ltx-math.el \
	    outln-18.el out-xtra.el

AUCSRC = min-map.el  auc-tex.el  auc-ver.el  tex-site.el tex-init.el \
	 tex-auto.el tex-cpl.el  tex-buf.el  tex-jp.el   dbg-eng.el  \
	 ltx-misc.el ltx-env.el  ltx-sec.el  tex-info.el easymenu.el \
	 tex-18.el   tex-19.el   tex-lcd.el  ltx-math.el \
	 outln-18.el out-xtra.el reporter.el

STYLESRC = style/slides.el    style/foils.el \
	   style/article.el   style/book.el     style/letter.el \
	   style/report.el    style/amsart.el \
	   style/epsf.el      style/psfig.el    style/latexinfo.el \
	   style/dutch.el     style/german.el   style/dk.el \
	   style/j-article.el style/j-book.el   style/j-report.el \
	   style/jarticle.el  style/jbook.el    style/jreport.el \
	   style/dinbrief.el  style/virtex.el   style/plfonts.el \
	   style/plhb.el      style/harvard.el

LACHECKFILES = lacheck/Makefile lacheck/lacheck.lex lacheck/lacheck.man \
	       lacheck/test.tex

LACHECKGEN = lacheck.c test.old

DOCFILES = doc/Makefile doc/auc-tex.texi doc/ref-card.tex doc/math-ref.tex

EXTRAFILES = COPYING README PROBLEMS OEMACS VMS INSTALL \
	Makefile dbg-jp.el lpath.el

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
	@echo "** Still missing is the automatic extraction of symbols"
	@echo "** and environments from your sites TeX style files."
	@echo "** To do this, type \`make install-auto'."
	@echo "** Beware, this takes some time and uses around 300k"
	@echo "** storage, depending on your the TeX style files. "
	@echo "** "
	@echo "** It is possible to use AUC TeX without this information."
	@echo "** "
	@echo "** WARNING: Some old Emacs versions are broken, and will"
	@echo "** not allow the auto files to be installed this way."
	@echo "** You can instead install them from an interactive"
	@echo "** emacs session by typing"
	@echo "** \"M-x TeX-auto-generate-global RET\"."
	@echo "**********************************************************"
	@echo

install-auto:
	@echo "**********************************************************"
	@echo "** Extracting site information.	This may take a while..."
	@echo "**********************************************************"
	if [ ! -d $(autodir) ]; then mkdir $(autodir); else true; fi
	$(AUTO) 
	@echo "**********************************************************"
	@echo "** If this failed then check that you have set"
	@echo "** TeX-lisp-directory correctly in tex-site.el"
	@echo "**********************************************************"
	@echo
	@echo "**********************************************************"
	@echo "** Byte compiling.  This will take a while..."
	@echo "** Some files may fail to compile. Don't Panic!"
	@echo "**********************************************************"
	-$(AUTOC) $(autodir)/*.el
	@echo "**********************************************************"
	@echo "**  Some files may have failed to compile. Don't Panic! **"
	@echo "**  Congratulations! AUC TeX installation completed     **"
	@echo "**********************************************************"
	@echo "** You may want to print the following files:  "
	@echo "**    doc/auc-tex.dvi"
	@echo "**    doc/ref-card.dvi"
	@echo "** Now edit .emacs according to the documentation       **"
	@echo "**********************************************************"
	@echo

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
	$(ELC) $(AUCSRC) tex-load.el $(STYLESRC)
	if [ ! -d $(aucdir) ]; then mkdir $(aucdir); else true; fi ; 
	if [ `pwd` != `(cd $(aucdir) && pwd)` ] ; \
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

tex-load.el: $(AUCSRC) 
	mv tex-load.el loaddefs.el
	emacs -batch -f batch-update-autoloads $(AUCSRC) 
	mv loaddefs.el tex-load.el
	touch tex-load.el

dist:	tex-load.el
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making distribution of auctex for release $(TAG)"
	@echo "**********************************************************"
	rm  -f auc-ver.el
	if [ -d auctex-$(TAG) ]; then rm -r auctex-$(TAG) ; fi
	echo "(defconst AUC-TeX-version \"$(TAG)\"" > auc-ver.el
	echo '  "AUC TeX version number")'         >> auc-ver.el
	echo "(defconst AUC-TeX-date \"`date`\""   >> auc-ver.el
	echo '  "AUC TeX release date")'           >> auc-ver.el
	echo "(provide 'auc-ver)"	           >> auc-ver.el
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	-cvs add $(AUCSRC) tex-load.el $(EXTRAFILES)
	-(cd doc; cvs add `echo $(DOCFILES) | sed -e s@doc/@@g` )
	-(cd lacheck; cvs add `echo $(LACHECKFILES) | sed -e s@lacheck/@@g` )
	-(cd style; cvs add `echo $(STYLESRC) | sed -e s@style/@@g` )
	cvs commit -m "Release $(TAG)"
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`
	mkdir auctex-$(TAG) 
	mkdir auctex-$(TAG)/style
	mkdir auctex-$(TAG)/doc auctex-$(TAG)/lacheck
	cp $(AUCSRC) tex-load.el $(EXTRAFILES) auctex-$(TAG)
	cp $(STYLESRC) auctex-$(TAG)/style
	cp $(LACHECKFILES) auctex-$(TAG)/lacheck
	(cd lacheck; $(MAKE) $(LACHECKGEN); \
	 cp $(LACHECKGEN) ../auctex-$(TAG)/lacheck )
	cp $(DOCFILES)  auctex-$(TAG)/doc
	(cd doc; $(MAKE) dist; cp auctex auctex-* ../auctex-$(TAG)/doc )
	cp doc/*.html /user/amanda/lib/www/auctex/alpha-doc
	rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
	rm -f $(FTPDIR)/auctex-$(TAG).tar.Z
	tar -cf - auctex-$(TAG) | gzip --best > $(FTPDIR)/auctex-$(TAG).tar.gz
	tar -cf - auctex-$(TAG) | compress > $(FTPDIR)/auctex.tar.Z
	/home/pd/bin/zip -r $(FTPDIR)/auctex auctex-$(TAG)
	(cd $(FTPDIR); ln -s auctex-$(TAG).tar.gz auctex.tar.gz)

min-map:
	-cvs add $(MINMAPSRC) 
	cvs commit -m "Update"
	cp $(MINMAPSRC) doc/math-ref.tex $(FTPDIR) 
