#
# Makefile for the AUC TeX distribution
# $Id: Makefile,v 5.64 1993-09-06 22:26:40 amanda Exp $
#
# Edit the makefile, type `make', and follow the instructions.

##----------------------------------------------------------------------
##  YOU MUST EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

# Where local software is found
#prefix=/home/local/sys/gnu
prefix=/usr/local

# Where architecture dependent local software go
exec_prefix = $(prefix)

# Where installed binaries go.
bindir = $(exec_prefix)/bin

# Where info files go.
infodir = $(prefix)/info

# Where manual pages go.
mandir=$(prefix)/man/man1

# Where the standard emacs lisp files are located.
#elispdir=/home/dist/lib/emacs/lisp
elispdir=$(prefix)/lib/emacs/19.17/lisp

# Where the AUC TeX emacs lisp files go.
# Set this to "." to specify current directory.
#
# Make sure that this is the same directory as specified by
# TeX-lisp-directory in tex-site.el
#
#aucdir=/home/pd/share/emacs/auctex7.2
aucdir=$(prefix)/lib/emacs/site-lisp/auctex

# Name of your emacs binary
EMACS=emacs-19.19

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

REMOVE = tex-math.el

MINMAPSRC = min-map.el  remap.el    map-euro.el dvorak.el map-tex.el \
	    min-ind.el	min-ispl.el column.el # outline.el

MINMAPFILES = $(MINMAPSRC) outline.v18

AUCSRC = $(MINMAPSRC) auc-tex.el  auc-ver.el  tex-site.el tex-init.el \
	 tex-auto.el  tex-cpl.el  tex-buf.el  tex-jp.el   dbg-eng.el  \
	 ltx-math.el  ltx-misc.el ltx-env.el  ltx-sec.el  tex-info.el \
	 loaddefs.el  

FORMATSRC = format/VIRTEX.el \
	    format/TEX.el  format/LATEX.el  format/SLITEX.el  \
	    format/JTEX.el format/JLATEX.el format/JSLITEX.el \
	    format/FOILTEX.el format/AMSTEX.el format/AMSLATEX.el

STYLESRC = style/latex.el     style/slitex.el   style/foiltex.el \
	   style/article.el   style/book.el     style/letter.el \
	   style/report.el \
	   style/epsf.el      style/psfig.el    style/latexinfo.el \
	   style/dutch.el     style/german.el   style/dk.el \
	   style/j-article.el style/j-book.el   style/j-report.el \
	   style/jarticle.el  style/jbook.el    style/jreport.el

LACHECKFILES = lacheck/Makefile lacheck/lacheck.lex lacheck/lacheck.man \
	       lacheck/test.tex

LACHECKGEN = lacheck.c lacheck.1 test.old

DOCFILES = doc/Makefile doc/auc-tex.texi doc/ref-card.tex

EXTRAFILES = COPYING README PROBLEMS Makefile dbg-jp.el lpath.el outline.v18

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
	@echo "**********************************************************"

main: Doc LaCheck

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
	@echo "** Do NOT try to install the auto files with Lucid Emacs!"
	@echo "** Lucid Emacs batch mode is broken and will crash, at least"
	@echo "** in version 19.6 and earlier.  You can install them from"
	@echo "** an interactive emacs session by typing"
	@echo "** \"M-x TeX-auto-generate-global RET\"."
	@echo "**********************************************************"
	@echo

install-auto:
	@echo "**********************************************************"
	@echo "** Extracting site information.	This may take a while..."
	@echo "**********************************************************"
	if [ ! -d $(autodir) ]; then mkdir $(autodir); fi
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

LaInstall: LaCheck
	@echo "**********************************************************"
	@echo "** Installing LaCheck "
	@echo "**********************************************************"
	-(cd lacheck; $(MAKE) install bindir=$(bindir) mandir=$(mandir))

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
	$(ELC) $(AUCSRC) $(STYLESRC) $(FORMATSRC)
	if [ ! -d $(aucdir) ]; then mkdir $(aucdir); fi ; 
	if [ `pwd` != `(cd $(aucdir) && pwd)` ] ; \
	then \
	    if [ ! -d $(aucdir)/style ]; then mkdir $(aucdir)/style; fi ; \
	    if [ ! -d $(aucdir)/format ]; then mkdir $(aucdir)/format; fi ; \
	    $(MV) *.elc $(aucdir) ; \
	    $(MV) style/*.elc $(aucdir)/style ; \
	    $(MV) format/*.elc $(aucdir)/format ; \
	fi

LaCheck:
	@echo "**********************************************************"
	@echo "** Building LaCheck"
	@echo "**********************************************************"
	-(cd lacheck; $(MAKE) bindir=$(bindir) \
	  CC="$(CC)" CFLAGS="$(CFLAGS)" LEX="$(LEX)" )

Doc: 
	@echo "**********************************************************"
	@echo "** Making AUC TeX documentation"
	@echo "**********************************************************"
	-(cd doc; $(MAKE) TEX=$(TEX))

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex
	(cd doc; $(MAKE) clean)
	(cd lacheck; $(MAKE) clean)

dist:	
	-(cd lacheck; $(MAKE) dist)
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
	emacs -batch -f batch-update-autoloads .
	-cvs remove $(REMOVE) 
	-cvs add $(AUCSRC) $(EXTRAFILES) 
	-(cd doc; cvs add `echo $(DOCFILES) | sed -e s@doc/@@g` )
	-(cd lacheck; cvs add `echo $(LACHECKFILES) | sed -e s@lacheck/@@g` )
	-(cd style; cvs add `echo $(STYLESRC) | sed -e s@style/@@g` )
	-(cd format; cvs add `echo $(FORMATSRC) | sed -e s@format/@@g` )
	cvs commit 
	cvs tag release_$(TAG) 
	mkdir auctex-$(TAG) 
	mkdir auctex-$(TAG)/style auctex-$(TAG)/format 
	mkdir auctex-$(TAG)/doc auctex-$(TAG)/lacheck
	cp $(AUCSRC) $(EXTRAFILES) auctex-$(TAG)
	cp $(FORMATSRC) auctex-$(TAG)/format
	cp $(STYLESRC) auctex-$(TAG)/style
	cp $(LACHECKFILES) auctex-$(TAG)/lacheck
	(cd lacheck; $(MAKE) $(LACHECKGEN); \
	 cp $(LACHECKGEN) ../auctex-$(TAG)/lacheck )
	cp $(DOCFILES)  auctex-$(TAG)/doc
	(cd doc; $(MAKE) auc-info; cp auc-info* ../auctex-$(TAG)/doc )
	rm -f $(FTPDIR)/auctex-$(TAG).tar.gz $(FTPDIR)/auctex.tar.gz
	tar -cf - auctex-$(TAG)  | gzip > $(FTPDIR)/auctex-$(TAG).tar.gz
	(cd $(FTPDIR); ln -s auctex-$(TAG).tar.gz auctex.tar.gz)
