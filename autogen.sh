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
if test "x${AUCTEXDATE}" == x
    AUCTEXDATE=`sed -n '1s/^\([-0-9][-0-9]*\).*/\1/p' ChangeLog`
    test "X${AUCTEXDATE}" != X || { echo "Can't find date in ChangeLog" >&2 ; exit 1; }
fi

if test "x${AUCTEXVERSION}" == x
    AUCTEXVERSION=`sed -n '2,/^[0-9]/s/.*Version \(.*\) released\..*/\1/p' ChangeLog`
    test "X${AUCTEXVERSION}" != X || AUCTEXVERSION=${AUCTEXDATE}
fi

cd doc
rm -f version.texi
${MAKE} -f Makefile.in MAKEINFO="${MAKEINFO}" PERL="$PERL" AUCTEXDATE="$AUCTEXDATE" AUCTEXVERSION="$AUCTEXVERSION" dist || { echo "Error running ${MAKE} in doc" >&2 ; exit 1; }
cd ..


