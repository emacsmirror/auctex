#!/bin/sh
autoconf
grep '^MY_LANG=' configure >/dev/null 2>/dev/null || {
  echo "Using an old autoconf, are we?"
  echo "Fixing the two known issues now."
# Two fixes: Save LANG and LC_CTYPE before configure munges it
# even across recursive call(s) of ./configure
# Account for difference between AC_INIT in 2.13 and 2.52
  ed -s configure <<\EOF
/^[^#]/i
if test -z "$MY_LANG$MY_LC_CTYPE$MY_LANGUAGE$MY_LC_ALL"; then 
  MY_LANG=$LANG; export MY_LANG
  MY_LC_CTYPE=$LC_CTYPE; export MY_LC_CTYPE
  MY_LANGUAGE=$LANGUAGE; export MY_LANGUAGE
  MY_LC_ALL=$LC_ALL; export MY_LC_ALL
fi
.
,s/ac_unique_file=preview-latex/ac_unique_file=preview.el/
w
q
EOF
}
cd doc
makeinfo -D rawfile --no-headers --no-validate readme.texi >../README
makeinfo -D rawfile --no-headers --no-validate install.texi >../INSTALL
makeinfo -D rawfile --no-headers --no-validate problems.texi >../PROBLEMS
makeinfo -D rawfile --no-headers --no-validate todo.texi >../TODO
makeinfo -D rawfile --no-headers --no-validate wininstall.texi >../INSTALL.windows
makeinfo -D rawfile --no-headers --no-validate --number-sections faq.texi >../FAQ
cd ..
