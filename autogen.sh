#!/bin/sh
if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi

cd doc
#makeinfo -D rawfile --no-headers --no-validate readme.texi >../README
makeinfo -D rawfile --no-headers --no-validate install.texi >../INSTALL
#makeinfo -D rawfile --no-headers --no-validate problems.texi >../PROBLEMS
#makeinfo -D rawfile --no-headers --no-validate todo.texi >../TODO
makeinfo -D rawfile --no-headers --no-validate wininstall.texi >../INSTALL.windows
#makeinfo -D rawfile --no-headers --no-validate --number-sections faq.texi >../FAQ
cd ..


