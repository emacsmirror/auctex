#!/bin/sh
autoconf
grep -q '^MY_LANG=' configure || {
  echo "Trying to deal with outdated autoconf"
  ed -s configure <<\EOF
/^[^#]/i
MY_LANG=$LANG
MY_LC_CTYPE=$LC_CTYPE
.
w
q
EOF
}
makeinfo -D rawfile --no-headers --no-validate doc/readme.texi >README
makeinfo -D rawfile --no-headers --no-validate doc/install.texi >INSTALL
makeinfo -D rawfile --no-headers --no-validate doc/problems.texi >PROBLEMS
