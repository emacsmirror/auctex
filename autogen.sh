#!/bin/sh
if test -z "$AUTOCONF"
then autoconf
else $AUTOCONF
fi

echo "Patching around autoconf issues now."
# Account for difference between AC_INIT in 2.13 and 2.52
sed 's/ac_unique_file=auc-tex/ac_unique_file=auctex.spec/' \
  configure >>configure.sed
cat configure.sed >configure && rm configure.sed

