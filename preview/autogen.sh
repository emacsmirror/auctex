#!/bin/sh
if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi
cd doc
make -f Makefile.in MAKEINFO=makeinfo disttexts preview-latex.info
cd ..
