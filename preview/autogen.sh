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
makeinfo --no-headers --no-validate doc/readme.texi >README
makeinfo --no-headers --no-validate doc/install.texi >INSTALL
makeinfo --no-headers --no-validate doc/problems.texi >PROBLEMS
(cd doc;makeinfo preview-latex.texi)
