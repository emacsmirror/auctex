#!/bin/sh
test "x${AUTOCONF}" = x && AUTOCONF=autoconf
${AUTOCONF} -I ..
rm -rf autom4te.cache
