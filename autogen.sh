#!/bin/sh
if test -z "$AUTOCONF"
then autoconf -I preview
else $AUTOCONF -I preview
fi
rm -rf autom4te.cache
cd doc
make -f Makefile.in MAKEINFO=makeinfo disttexts auctex.info
cd ..
cd preview
./autogen.sh
cd ..

