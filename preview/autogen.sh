:
autoconf
makeinfo --no-headers --no-validate doc/readme.texi >README
makeinfo --no-headers --no-validate doc/install.texi >INSTALL
makeinfo --no-headers --no-validate doc/problems.texi >PROBLEMS
(cd doc;makeinfo preview-latex.texi)
