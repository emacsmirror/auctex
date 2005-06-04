#!/bin/sh
test "x${AUTOCONF}" = x && AUTOCONF=autoconf
test "x${MAKEINFO}" = x && MAKEINFO=makeinfo
test "x${PERL}" = x && PERL=perl
${AUTOCONF} -I ..
rm -rf autom4te.cache
cd doc
make -f Makefile.in MAKEINFO="$MAKEINFO" PERL="$PERL" disttexts preview-latex.info
cd ..
