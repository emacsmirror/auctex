#
# Makefile for the AUC TeX distribution
# $Id: Makefile,v 5.43 1993-04-12 22:47:40 amanda Exp $
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
infodir = $(prefix)/lib/emacs/info

# Where the AUC TeX emacs lisp files go.
# Set this to "." to specify current directory.

# Where manual pages go.
mandir=$(prefix)/man/man1

# Where the standard emacs lisp files are located
elispdir=$(prefix)/lib/emacs/lisp

# Make sure that this is the same directory as specified by
# TeX-lisp-directory in tex-site.el

aucdir=$(elispdir)/auctex

##----------------------------------------------------------------------
## YOU MAY NEED TO EDIT THESE
##----------------------------------------------------------------------

# Do not change the definition of autodir below, unless you also
# update TeX-auto-global in tex-init.el

# Where the automatically generated lisp files for your site go.
autodir=$(aucdir)/auto

# Using emacs in batch mode.
EMACS=emacs -batch -q

# Specify the byte-compiler for compiling AUC TeX files
ELC=env EMACSLOADPATH=.:$(elispdir):$(elispdir)/bytecomp $(EMACS) -f batch-byte-compile

# Specify the byte-compiler for generating style files
AUTO= EMACSLOADPATH=$(aucdir):$(elispdir) $(EMACS) \
	-l tex-auto -f TeX-auto-generate-global

# Specify the byte-compiler for compiling generated style files
AUTOC= EMACSLOADPATH=$(aucdir):$(elispdir):$(elispdir)/bytecomp $(EMACS) -f batch-byte-compile

# Using TeX in batch mode.
TEX=tex

# Need K&R compiler.  Either `cc' or `gcc -traditional'
CC = gcc -traditional

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

FTPDIR = /pack/ftp/pub/emacs-lisp/alpha

MINMAPSRC = min-map.el	min-out.el  min-key.el ltx-dead.el tex-math.el \
	    min-ind.el	min-ispl.el kill-fix.el

MINMAPFILES = README_MINOR $(MINMAPSRC)

AUCSRC = $(MINMAPSRC) auc-tex.el  auc-ver.el  tex-site.el tex-init.el \
	 tex-auto.el  tex-misc.el tex-cpl.el  tex-buf.el  tex-jp.el \
	 ltx-misc.el  ltx-env.el  ltx-sec.el  dbg-eng.el  tex-foil.el

FORMATSRC = format/VIRTEX.el \
	    format/TEX.el  format/LATEX.el  format/SLITEX.el  \
	    format/JTEX.el format/JLATEX.el format/JSLITEX.el \
	    format/FOILTEX.el

STYLESRC = style/latex.el   style/slitex.el \
	   style/article.el style/book.el    style/letter.el \
	   style/foiltex.el

LACHECKFILES= lacheck/Makefile lacheck/lacheck.1 lacheck/lacheck.lex \
	lacheck/lacheck.man lacheck/lacheck.noflex.c

DOCFILES=doc/Makefile doc/auc-tex.texi doc/ref-card.tex

# dbg-jp.el can not be byte compiled with standard emacs
OTHERFILES = COPYING README README_MINOR PROBLEMS Makefile dbg-jp.el \
	 $(DOCFILES) $(LACHECKFILES)


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
	@echo "	 TeXinfo 2.16 or later installed.  The version of"
	@echo "  TeXinfo distributed with GNU Emacs 18.xx is TeXinfo 1."
	@echo
	@echo "	 TeXinfo2 is available for ftp at all major GNU sites."
	@echo "	 TeXinfo2 will also be part of GNU Emacs 19.xx."
	@echo

all: main
	@echo "**********************************************************"
	@echo "** Before running \`make install' you should edit the "
	@echo "** file \`tex-site.el' in this directory to suit your"
	@echo "** local needs.	Print out the file \`doc/auc-tex.dvi'"
	@echo "** and read the section \`Installation' if in doubt." 
	@echo "** Alternatively you may run \`make DocInstall' and read"
	@echo "** that information via Emacs' info system"
	@echo "** Then run: \`make install'"
	@echo "** The Emacs Lisp files will only be recompiled, if"
	@echo "** you have set aucdir to a different directory."
	@echo "**********************************************************"

main: Doc LaCheck

install: main LispInstall LaInstall DocInstall
	@echo 
	@echo "**********************************************************"
	@echo "** AUC TeX installation almost completed "
	@echo "** Still missing is the automatic extraction of symbols"
	@echo "** and environments from your sites TeX style files."
	@echo "** To do this, type \`make install-auto'."
	@echo "** Beware, this takes some time and uses around 300k"
	@echo "** storage, depending on your the TeX style files. "
	@echo "** It is possible to use AUC TeX without this information."
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
	-(cd lacheck; make install bindir=$(bindir) mandir=$(mandir))

DocInstall: Doc
	@echo "**********************************************************"
	@echo "** Preparing AUC TeX \`info' pages"
	@echo "**********************************************************"
	-(cd doc; make install infodir=$(infodir))

LispInstall:
	@echo "**********************************************************"
	@echo "** Byte compiling AUC TeX.  This may take a while..."
	@echo "**********************************************************"
	$(ELC) $(AUCSRC) $(STYLESRC) $(FORMATSRC)
	if [ "." != $(aucdir) ] ; \
	then \
	    if [ ! -d $(aucdir) ]; then mkdir $(aucdir); fi ; \
	    if [ ! -d $(aucdir)/style ]; then mkdir $(aucdir)/style; fi ; \
	    if [ ! -d $(aucdir)/format ]; then mkdir $(aucdir)/format; fi ; \
	    $(MV) *.elc $(aucdir) ; \
	    $(MV) style/*.elc $(aucdir)/style ; \
	    $(MV) format/*.elc $(aucdir)/format ; \
	fi

LaCheck:
	@echo "**********************************************************"
	@echo "** Building LaCheck
	@echo "**********************************************************"
	-(cd lacheck; make bindir=$(bindir) \
	  CC="$(CC)" CFLAGS="$(CFLAGS)" LEX="$(LEX)" )

Doc: 
	@echo "**********************************************************"
	@echo "** Making AUC TeX documentation
	@echo "**********************************************************"
	-(cd doc; make)

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex
	(cd doc; make clean)
	(cd lacheck; make clean)

dist:	
	-(cd lacheck; make dist)
	@if [ "X$$TAG" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making distribution of auctex for release $$TAG"
	@echo "**********************************************************"
	rm  -f auc-ver.el
	if [ -d auctex ]; then rm -r auctex; fi
	OUT=`echo $$TAG | sed s/release_//`; \
	   echo "(defconst AUC-TeX-version \"$$OUT\" \
	   \"AUC TeX version number\")" \
	   "(defconst AUC-TeX-date \"`date`\" \
	   \"AUC TeX release date\")" \
	   "(provide 'auc-ver)"	 > auc-ver.el
	cvs checkout -r $(TAG) auctex
	find auctex -name CVS -print | xargs rm -rf
	cp auc-ver.el auctex
	(cd auctex/doc; make auc-tex.info)
	(cd auctex/lacheck; make lacheck.c; make lacheck.1; cp lacheck.c lacheck.noflex.c)
	(cd auctex;  \
	echo AUC TeX $$TAG on `date` > FILELIST; \
	echo "----------------------------------------" >> FILELIST; \
	ident $(AUCSRC) $(OTHERFILES) >> FILELIST )
	OUT=auctex`echo $$TAG | sed s/release//`; \
	tar -cf - auctex | gzip > $$OUT.tar.z
	VER=`echo $$TAG | sed s/release_// | sed s/auctex_//` ; \
	(cd auctex; tar -cf - $(MINMAPFILES)) | \
	gzip -c >min-map_$$VER.tar.z
	rm -r auctex

# Removed.  Don't mail them, send them the address of a ftpmail client. 
#	if [ ! -d split ]; then mkdir split; else rm split/*; fi; \
#	cp auctex/FILELIST split; \
#	uuencode $$OUT.tar.z $$OUT.tar.z | split -200 - split/auc-tex-

#mail:
#	if [ "X$$WHO" = "X" ]; then echo "*** No reciepient(s) ***"; exit 1; fi
#	for U in $$WHO; do\
#	for F in `ls -1 split`; do\
#	echo Sending $$F to $$U ; \
#	Mail -s $$F $$U < split/$$F;\
#	sleep 10; \
#	done; done

ftp:	
	@if [ "X$$TAG" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	@echo "]; then echo "*** No tag ***"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making ftp copy of lacheck for whatever release it is"
	@echo "**********************************************************"
	-(cd lacheck; make ftp)
	@echo "**********************************************************"
	@echo "** Making ftp copy of minor maps for $$TAG"
	@echo "** Making ftp copy of outline minor mode for $$TAG"
	@echo "**********************************************************"
	VER=`echo $$TAG | sed s/release_// | sed s/auctex_//` ; \
	cp min-map_$$VER.tar.z $(FTPDIR) ; \
	cd $(FTPDIR) ; \
	rm -f min-map.tar.z min-out.tar.z ; \
	ln -s min-map_$$VER.tar.z min-map.tar.z ; \
	ln -s min-map_$$VER.tar.z min-out.tar.z
	@echo "**********************************************************"
	@echo "** Making ftp copy of auc-tex for  $$TAG"
	@echo "**********************************************************"
	OUT=`echo $$TAG | sed s/release_//` ; \
	cp auctex_$$OUT.tar.z $(FTPDIR) ; \
	cd $(FTPDIR) ; \
	rm -f auctex.tar.z ; \
	ln -s auctex_$$OUT.tar.z auctex.tar.z
