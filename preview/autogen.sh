#!/bin/sh
rm -f configure

if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi

echo "Patching around autoconf issues now."
# Two fixes: Save LANG and LC_CTYPE before configure munges it
# even across recursive call(s) of ./configure
# Account for difference between AC_INIT in 2.13 and 2.52
sed '2i \
'"`sed -n 's/^#autogen.sh: //p' configure.in | sed 's/\$/ \\\\/'`"'

s/ac_unique_file=preview-latex/ac_unique_file=preview.el/' \
  configure >configure.sed && cat configure.sed >configure && rm configure.sed

cd doc
makeinfo -D rawfile --no-headers --no-validate readme.texi >../README
makeinfo -D rawfile --no-headers --no-validate install.texi >../INSTALL
makeinfo -D rawfile --no-headers --no-validate problems.texi >../PROBLEMS
makeinfo -D rawfile --no-headers --no-validate todo.texi >../TODO
makeinfo -D rawfile --no-headers --no-validate wininstall.texi >../INSTALL.windows
makeinfo -D rawfile --no-headers --no-validate --number-sections faq.texi >../FAQ
cd ..
