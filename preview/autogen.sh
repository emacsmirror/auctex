#!/bin/sh
autoconf
grep -q '^MY_LANG=' configure || {
  echo "Trying to deal with outdated autoconf"
# Two fixes: Save LANG and LC_CTYPE before configure munges it
# Account for difference between AC_INIT in 2.13 and 2.52
  ed -s configure <<\EOF
/^[^#]/i
MY_LANG=$LANG
MY_LC_CTYPE=$LC_CTYPE
.
,s/ac_unique_file=preview-latex/ac_unique_file=preview-latex.el/
w
q
EOF
}
makeinfo -D rawfile --no-headers --no-validate doc/readme.texi >README
makeinfo -D rawfile --no-headers --no-validate doc/install.texi >INSTALL
makeinfo -D rawfile --no-headers --no-validate doc/problems.texi >PROBLEMS
