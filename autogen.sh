#!/bin/sh
test "x${AUTOCONF}" != x || AUTOCONF=autoconf
test "x${MAKEINFO}" != x || MAKEINFO=makeinfo
test "x${PERL}" != x || PERL=perl
test "x${MAKE}" != x || MAKE=make
${AUTOCONF} || { echo "Error running ${AUTOCONF} in ." >&2 ; exit 1; }
cd preview
${AUTOCONF} -I.. || { echo "Error running ${AUTOCONF} in preview" >&2 ; exit 1; }
cd ..
rm -rf autom4te.cache preview/autom4te.cache
cd doc
${MAKE} -f Makefile.in MAKEINFO="${MAKEINFO}" PERL="$PERL" dist || { echo "Error running ${MAKE} in doc" >&2 ; exit 1; }
cd ..


