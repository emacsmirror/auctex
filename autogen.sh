#!/bin/sh
test "x${AUTOCONF}" = x && AUTOCONF=autoconf
test "x${MAKEINFO}" = x && MAKEINFO=makeinfo
${AUTOCONF} -I preview
rm -rf autom4te.cache
cd doc
make -f Makefile.in MAKEINFO="${MAKEINFO}" disttexts auctex.info
cd ..
cd preview
export AUTOCONF
export MAKEINFO
./autogen.sh
cd ..

