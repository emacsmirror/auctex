#!/bin/sh
if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi

cd doc
makeinfo -D rawfile --no-headers --no-validate readme.texi --output ../README
makeinfo -D rawfile --no-headers --no-validate install.texi --output ../INSTALL
makeinfo -D rawfile --no-headers --no-validate problems.texi --output ../PROBLEMS
makeinfo -D rawfile --no-headers --no-validate todo.texi --output ../TODO
makeinfo -D rawfile --no-headers --no-validate wininstall.texi --output ../INSTALL.windows
makeinfo -D rawfile --no-headers --no-validate --number-sections faq.texi --output ../FAQ
cd ..
