#!/bin/sh
if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi
rm -rf autom4te.cache
cd doc
make -f Makefile.in MAKEINFO=makeinfo PERL=perl disttexts preview-latex.info
cd ..
