#!/bin/sh
if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi

cd doc
# We don't want to rename intro.texi to readme.texi
# in order to keep CVS history.
makeinfo -D rawfile --no-headers --no-validate intro.texi >../README
makeinfo -D rawfile --no-headers --no-validate install.texi >../INSTALL
#makeinfo -D rawfile --no-headers --no-validate problems.texi >../PROBLEMS
makeinfo -D rawfile --no-headers --no-validate todo.texi >../TODO
makeinfo -D rawfile --no-headers --no-validate wininstall.texi >../INSTALL.windows
makeinfo -D rawfile --no-headers --no-validate faq.texi >../FAQ
cd ..
