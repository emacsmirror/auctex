#
# Makefile for the AUC TeX distribution
# $Id: Makefile,v 5.20 1993-02-16 04:08:34 amanda Exp $
#

##----------------------------------------------------------------------
##  EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

prefix=/usr/local
exec_prefix = $(prefix)

# Where installed binaries go.
bindir = $(exec_prefix)/bin

# Where info files go.
infodir = $(prefix)/info

# Where emacs lisp files go.
elispdir=$(prefix)/elisp/auctex

# Where manual pages go.
mandir=$(prefix)/man/man1

# We run both Emacs and TeX in batch mode
EMACS=emacs
TEX=tex

# Need K&R compiler.  Either `cc' or `gcc -traditional'
CC = gcc -traditional

# Cflags.. Include `-DNEED_STRSTR' if you don't have strstr() in libc
CFLAGS = -O # -DNEED_STRSTR 

# How to run flex. (flex is needed, lex won't do)
# if you don't have `flex' you may use the other instead:

LEX = flex -8  lacheck.lex
#LEX = cp lacheck.noflex.c lex.yy.c 


##----------------------------------------------------------------------
##  BELOW THIS LINE ON YOUR OWN RISK!
##----------------------------------------------------------------------

FTPDIR = /home/priv/iesd/ftp/pub/emacs-lisp

MINMAPSRC = min-map.el min-out.el min-key.el ltx-dead.el tex-math.el

ELISPSRC= $(MINMAPSRC) auc-tex.el tex-cpl.el tex-misc.el tex-symb.el \
	ltx-env.el tex-dbg.el tex-names.el vir-symb.el \
	ltx-sec.el tex-buf.el tex-site.el auc-ver.el \
	tex-init.el min-key.el ltx-dead.el

LAHECKFILES= lacheck/Makefile lacheck/lacheck.1 lacheck/lacheck.lex \
	lacheck/lacheck.man lacheck/lacheck.noflex.c

DOCFILES=doc/Makefile doc/auc-tex.texi doc/ref-card.tex

OTHERFILES = COPYING README README_MINOR Makefile  $(DOCFILES) $(LACHECKFILES)

first:
	@echo ""
	@echo "  ** Welcome to the AUC TeX installation suite **"
	@echo ""
	@echo "  Edit the Makefile to suit your needs. Then run:"
	@echo 
	@echo "   make all"
	@echo 
	@echo "  and follow the instructions."
	@echo ""

all: main
	@echo "**********************************************************"
	@echo "** Before running \`make install' you should edit the "
	@echo "** file \`tex-site.el' in this directory to suit your"
	@echo "** local needs.  Print out the file \`doc/auc-tex.dvi'"
	@echo "** and read the section \`Installation' if in doubt." 
	@echo "** Alternatively you may run \`make DocInstall' and read"
	@echo "** that information via Emacs' info system"
	@echo "** Then run: \`make install'"
	@echo "** The Emacs Lisp files will only be recompiled, if"
	@echo "** you have set elispdir to a different directory."
	@echo "**********************************************************"

main: Doc LaCheck

install: main $(elispdir) LaInstall DocInstall
	@echo 
	@echo "**********************************************************"
	@echo "**   Congratulations! AUC TeX installation completed    **"
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

$(elispdir): $(ELISPSRC)  Makefile
	@echo "**********************************************************"
	@echo "** Byte compiling AUC TeX.  This may take a while..."
	@echo "**********************************************************"
	if [ ! -d $(elispdir) ]; then mkdir $(elispdir); fi
	cp $(ELISPSRC) $(elispdir)
	(touch /tmp/auc.$$$$; \
	echo "(setq load-path (cons \"$(elispdir)\" load-path))" >>  /tmp/auc.$$$$; \
	for EL in $(ELISPSRC); do \
	echo "(byte-compile-file \"$(elispdir)/$$EL\")" \
              >> /tmp/auc.$$$$; \
	done; \
	$(EMACS) -batch -l /tmp/auc.$$$$; \
	rm -f /tmp/auc.$$$$ )
	(for EL in $(ELISPSRC); do \
	chmod 644 $(elispdir)/$${EL}c; \
	rm $(elispdir)/$$EL; \
	done)


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
	@if [ "X$$TAG" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	@echo "]; then echo "*** No tag ***"; exit 1; fi
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
	   "(provide 'auc-ver)"  > auc-ver.el
	cvs checkout -r $(TAG) auctex
	find auctex -name CVS -print | xargs rm -rf
	cp auc-ver.el auctex
	(cd auctex/doc; make auc-tex.info)
	(cd auctex/lacheck; make lacheck.c; cp lacheck.c lacheck.noflex.c)
	(cd auctex;  \
	echo AUC TeX $$TAG on `date` > FILELIST; \
	echo "----------------------------------------" >> FILELIST; \
	ident $(ELISPSRC) $(OTHERFILES) >> FILELIST )
	OUT=auctex`echo $$TAG | sed s/release//`; \
	tar -cf - auctex | compress -c > $$OUT.tar.Z; \
	if [ ! -d split ]; then mkdir split; else rm split/*; fi; \
	cp auctex/FILELIST split; \
	uuencode $$OUT.tar.Z $$OUT.tar.Z | split -200 - split/auc-tex-
	(cd auctex; tar -cf - $(MINMAPSRC) MISC) | compress -c >minor-map.tar.Z
	rm -r auctex

mail:
	if [ "X$$WHO" = "X" ]; then echo "*** No reciepient(s) ***"; exit 1; fi
	for U in $$WHO; do\
	for F in `ls -1 split`; do\
	echo Sending $$F to $$U ; \
	Mail -s $$F $$U < split/$$F;\
	sleep 10; \
	done; done
	
ftp:	dist
	@if [ "X$$TAG" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	@echo "]; then echo "*** No tag ***"; exit 1; fi
	@echo "**********************************************************"
	@echo "** Making ftp copy of lacheck for whatever release it is"
	@echo "**********************************************************"
	-(cd lacheck; make ftp)
	@echo "**********************************************************"
	@echo "** Making ftp copy of minor maps for release $$TAG"
	@echo "** Making ftp copy of outline minor mode for release $$TAG"
	@echo "**********************************************************"
	VER=`echo $$TAG | sed s/release_// | sed s/auctex_//` ; \
	cp min-map_$VER.tar.Z $FTPDIR ; \
	cd $FTPDIR ; \
	rm min-map.tar.Z min-out.tar.Z ; \
	ln -s min-map_$VER.tar.Z min-map.tar.Z ; \
	ln -s min-map_$VER.tar.Z min-out.tar.Z
	@echo "**********************************************************"
	@echo "** Making ftp copy of auc-tex for release $$TAG"
	@echo "**********************************************************"
	OUT=`echo $$TAG | sed s/release_//` ; \
	cp $$OUT.tar.Z $FTPDIR ; \
	cd $FTPDIR ; \
	rm auctex.tar.Z ; \
	ln -s $$OUT.tar.Z auctex.tar.Z
