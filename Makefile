#
# Makefile for the AUC TeX distribution
# $Id: Makefile,v 5.16 1992-09-16 12:14:13 amanda Exp $
#

##----------------------------------------------------------------------
##  EDIT THE FOLLOWING LINES 
##----------------------------------------------------------------------

#
# For AUC TeX installation
#

EMACS=emacs
TEX=tex

ELISPDIR=/home/local/lib/emacs/local/auctex
INFODIR=/home/local/sys/gnu/info

#
# for LaCheck installation:
#

PD_DIR=/home/local/pd
BINDIR=$(PD_DIR)/bin
MANDIR=$(PD_DIR)/man

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

ELISPSRC= auc-tex.el min-map.el tex-cpl.el tex-misc.el tex-symb.el \
	ltx-env.el min-out.el tex-dbg.el tex-names.el vir-symb.el \
	ltx-sec.el tex-buf.el tex-math.el tex-site.el auc-ver.el
OTHERFILES = COPYING INTRO README Makefile lacheck/* doc/*

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
	@echo "** you have set ELISPDIR to a different directory."
	@echo "**********************************************************"

main: Doc LaCheck

install: main $(ELISPDIR) LaInstall DocInstall
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
	-(cd lacheck; make install BINDIR=$(BINDIR) MANDIR=$(MANDIR))

DocInstall: Doc
	@echo "**********************************************************"
	@echo "** Preparing AUC TeX \`info' pages"
	@echo "**********************************************************"
	-(cd doc; make install INFODIR=$(INFODIR))

$(ELISPDIR): $(ELISPSRC)  Makefile
	@echo "**********************************************************"
	@echo "** Byte compiling AUC TeX.  This may take a while..."
	@echo "**********************************************************"
	if [ ! -d $(ELISPDIR) ]; then mkdir $(ELISPDIR); fi
	cp $(ELISPSRC) $(ELISPDIR)
	(touch /tmp/auc.$$$$; \
	echo "(setq load-path (cons \"$(ELISPDIR)\" load-path))" >>  /tmp/auc.$$$$; \
	for EL in $(ELISPSRC); do \
	echo "(byte-compile-file \"$(ELISPDIR)/$$EL\")" \
              >> /tmp/auc.$$$$; \
	done; \
	$(EMACS) -batch -l /tmp/auc.$$$$; \
	rm -f /tmp/auc.$$$$ )
	(for EL in $(ELISPSRC); do \
	chmod 644 $(ELISPDIR)/$${EL}c; \
	rm $(ELISPDIR)/$$EL; \
	done)


LaCheck:
	@echo "**********************************************************"
	@echo "** Building LaCheck
	@echo "**********************************************************"
	-(cd lacheck; make BINDIR=$(BINDIR) \
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
	cvs export -r $(TAG) auctex
	cp auc-ver.el auctex
	(cd auctex/doc; make aux-tex.info)
	(cd auctex/lacheck; make lacheck.c)
	(cd auctex;  \
	echo AUC TeX $$TAG on `date` > FILELIST; \
	echo "----------------------------------------" >> FILELIST; \
	ident $(ELISPSRC) $(OTHERFILES) >> FILELIST )
	OUT=auctex`echo $$TAG | sed s/release//`; \
	tar -cf - auctex | compress -c > $$OUT.tar.Z; \
	if [ ! -d split ]; then mkdir split; else rm split/*; fi; \
	cp auctex/FILELIST split; \
	uuencode $$OUT.tar.Z $$OUT.tar.Z | split -200 - split/auc-tex-
	rm -r auctex

mail:
	if [ "X$$WHO" = "X" ]; then echo "*** No reciepient(s) ***"; exit 1; fi
	for U in $$WHO; do\
	for F in `ls -1 split`; do\
	echo Sending $$F to $$U ; \
	Mail -s $$F $$U < split/$$F;\
	sleep 10; \
	done; done
	
