#
# Makefile for the AUC TeX distribution
# $Id: Makefile,v 5.12 1992-07-22 13:06:09 krab Exp $
#

ELISPDIR=/home/local/lib/emacs/local/auctex
BINDIR=/home/local/pd/bin

CC = gcc
CFLAGS = -O
LDFLAGS = -ll
EMACS=emacs
TEX=tex
ELISPSRC= auc-tex.el min-map.el tex-cpl.el tex-misc.el tex-symb.el \
	ltx-env.el min-out.el tex-dbg.el tex-names.el vir-symb.el \
	ltx-sec.el tex-buf.el tex-math.el tex-site.el 
OTHERFILES = COPYING INTRO README Makefile

all: $(ELISPDIR) Doc LaCheck

$(ELISPDIR): $(ELISPSRC)  Makefile
	if [ ! -d $(ELISPDIR) ]; then mkdir $(ELISPDIR); fi
	cp $(ELISPSRC) $(ELISPDIR)
	(touch /tmp/auc.$$$$; \
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
	(cd lacheck; make)

Doc: 
	(cd doc; make)

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex


dist: 	
	if [ "X$$TAG" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	echo "Make distribution of auctex for release $$TAG"
	cvs checkout -r $(TAG) auctex
	rm -r auctex/CVS
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
	
