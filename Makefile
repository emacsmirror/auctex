#
# Makefile for the AUC TeX distribution
# $Id: Makefile,v 5.3 1992-03-18 19:04:13 krab Exp $
#

ELISPDIR=/user/krab/Lib/auc-tex/auc-tex
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

all: $(ELISPDIR) idetex refcard

$(ELISPDIR): $(ELISPSRC)  Makefile
	if [ ! -d $(ELISPDIR) ]; then mkdir $(ELISPDIR); fi
	cp $(ELISPSRC) $(ELISPDIR)
	(touch /tmp/auc.$$$$;\
	for EL in $(ELISPSRC); do\
	echo "(byte-compile-file \"$(ELISPDIR)/$$EL\")" \
              >> /tmp/auc.$$$$; \
	done; \
	$(EMACS) -batch -l /tmp/auc.$$$$; \
	rm -f /tmp/auc.$$$$ )
	(for EL in $(ELISPSRC); do\
	chmod 644 $(ELISPDIR)/$${EL}c;\
	rm $(ELISPDIR)/$$EL;\
	done)

idetex: idetex.l
	lex idetex.l
	$(CC) $(CFLAGS) -o idetex lex.yy.c $(LDFLAGS)
	cp idetex $(BINDIR)
	chmod 755 $(BINDIR)/idetex

refcard: ref-card.tex
	tex ref-card
	rm ref-card.log

clean:
	rm -rf *~ #*# lex.yy.c idetex auctex


dist:	
	if [ "X$$TAG" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	echo "Make distribution of auctex for release $$TAG"
	cvs checkout -r $(TAG) auctex
	rm -r auctex/CVS.adm
	(cd auctex;  \
	echo AUC TeX $$TAG on `date` > FILELIST; \
	echo "----------------------------------------" >> FILELIST; \
	ident $(ELISPSRC) $(OTHERFILES) >> FILELIST )
	OUT=auctex`echo $$TAG | sed s/release//`; \
	tar -cf - auctex | compress -c > $$OUT.Z; \
	rm -r auctex

	

