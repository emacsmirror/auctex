#!/bin/sh
test "x${AUTOCONF}" = x && AUTOCONF=autoconf
test "x${MAKEINFO}" = x && MAKEINFO=makeinfo
test "x${PERL}" = x && PERL=perl
${AUTOCONF}
rm -rf autom4te.cache
cd doc
make -f Makefile.in MAKEINFO="${MAKEINFO}" PERL="$PERL" disttexts auctex.info preview-latex.info
cd ..
cd preview
export AUTOCONF
export MAKEINFO
./autogen.sh
cd ..

