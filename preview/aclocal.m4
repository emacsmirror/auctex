# serial 1

AC_DEFUN(AC_PATH_TEXMFDIR,
 [
# First check for docstrip.cfg information

if test -z "$previewtexmfdir" ; then
    AC_MSG_CHECKING([for docstrip directory configuration])
    cat > testdocstrip.tex <<EOF
\input docstrip
\ifx\basedir\undefined\else
   \message{^^J--preview-tex-dir=\showdirectory{tex/latex/preview}^^J%
               --texmf-prefix=\basedir^^J}
\fi
\endbatchfile
EOF
    $LATEX '\nonstopmode \input testdocstrip' >&5 2>&1
    texmfdir=`sed -n -e 's+/* *$++' -e '/^--texmf-prefix=/s///p' testdocstrip.log 2>&5`
    previewtexmfdir=`sed -n -e '/UNDEFINED/d' -e 's+/* *$++' -e '/^--preview-tex-dir=/s///p' testdocstrip.log 2>&5 `
    if test -z "$previewtexmfdir"  ; then
	if test ! -z "$texmfdir"  ; then
	    previewtexmfdir=\$\(texmfdir\)
	    previewdocdir=\$\(texmfdir\)
	    
	fi
    else
	previewdocdir=$texmfdir/doc/latex/styles
    fi
# Next 
# kpsepath -n latex tex
# and then go for the following in its output:
# a) first absolute path component ending in tex/latex// (strip trailing
# // and leading !!):  "Searching for TDS-compliant directory."  Install
# in preview subdirectory.
# b) first absolute path component ending in // "Searching for directory
# hierarchy"  Install in preview subdirectory.
# c) anything absolute.  Install both files directly there.

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
AC_MSG_CHECKING([for TDS-compliant directory])
for x in `kpsepath -n latex tex | tr ':' '\n' | sed -e 's/^!!//' | \
 		grep '^/.*/tex/latex//$' `
do
  x=`echo $x | sed -e 's+//+/+g' -e 's+/$++' `
  if test -d "$x"  ; then
     texmfdir=`echo $x | sed -e 's+/tex/latex++'`
     previewdocdir=\$\(texmfdir\)/doc/latex/styles
     previewtexmfdir=\$\(texmfdir\)/tex/latex/preview
     break
  fi
done
fi

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
AC_MSG_CHECKING([for TeX directory hierarchy])
for x in `kpsepath -n latex tex | tr ':' '\n' | sed -e 's/^!!//' | \
 		grep '^/.*//$'`
do
  if test -d "$x"  ; then
     texmfdir=$x
     previewtexmfdir=\$\(texmfdir\)/preview
     previewdocdir=\$\(texmfdir\)/preview
     break
  fi
done
fi

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
AC_MSG_CHECKING([for TeX input directory])
for x in `kpsepath -n latex tex | tr ':' '\n' | sed -e 's/^!!//' | \
 		grep '^/'`
do
  if test -d "$x"  ; then
     texmfdir=$x
     previewdocdir=\$\(texmfdir\)
     break
  fi
done
fi

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
	AC_MSG_ERROR([Cannot find the texmf directory!  Please use --with-texmf=dir to specify where the preview tex files go])
fi
     AC_MSG_RESULT($texmfdir)
fi

echo Preview will be placed in $previewtexmfdir
echo Preview docs will be placed in $previewdocdir
AC_SUBST(texmfdir)
AC_SUBST(previewtexmfdir)
AC_SUBST(previewdocdir)])





dnl AC_EMACS_LIST AC_XEMACS_P AC_PATH_LISPDIR and AC_EMACS_CHECK_LIB
dnl are stolen from w3.
dnl AC_PATH_LISPDIR obsoletes AM_PATH_LISPDIR.

AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil \"${OUTPUT}\"))" >& AC_FD_CC 2>&1  
	${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x 'ignore) (prin1-to-string x)) nil \"${OUTPUT}\"nil 5))" >& AC_FD_CC 2>&1
	retval=`cat ${OUTPUT}`
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1=$retval
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_XEMACS_P, [
  AC_MSG_CHECKING([if $EMACS is really XEmacs])
  AC_EMACS_LISP(xemacsp,(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\") ,"noecho")
  XEMACS=${EMACS_cv_SYS_xemacsp}
  EMACS_FLAVOR=emacs
  if test "$XEMACS" = "yes"; then
     EMACS_FLAVOR=xemacs
  fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)
])

AC_DEFUN(AC_PATH_LISPDIR, [
  AC_XEMACS_P
  if test "$prefix" = "NONE"; then
	AC_MSG_CHECKING([prefix for your Emacs])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(lispdir,[  --with-lispdir=DIR      Where to install lisp files], lispdir=${withval})
  AC_MSG_CHECKING([where .elc files should go])
  if test -z "$lispdir"; then
    dnl Set default value
    theprefix=$prefix
    if test "x$theprefix" = "xNONE"; then
	theprefix=$ac_default_prefix
    fi
    lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp"
	   break
	fi
    done
  fi
  lispdir="$lispdir/preview"
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)
])

AC_DEFUN(AC_CHECK_PROG_REQUIRED, [
AC_CHECK_PROG($1, $2, NONE)
if test "${$1}"x = NONEx ; then
   AC_MSG_ERROR([$3])
fi
])

AC_DEFUN(AC_CHECK_PROGS_REQUIRED, [
AC_CHECK_PROGS($1, $2, NONE)
if test "${$1}"x = NONEx ; then
   AC_MSG_ERROR([$3])
fi
])


AC_DEFUN(AC_PATH_PROG_REQUIRED, [
AC_PATH_PROG($1, $2, NONE)
if test "${$1}"x = NONEx ; then
   AC_MSG_ERROR([$3])
fi
])

dnl
dnl Check whether a function exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(progn (fmakunbound '$2) (condition-case nil (progn (require '$library) (fboundp '$2)) (error (prog1 nil (message \"$library not found\"))))),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
if test "${EMACS_cv_SYS_$1}" = "t"; then
	EMACS_cv_SYS_$1=yes
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
if test -z "$3"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Check whether a function exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_REQUIRE, [
if test -z "$2"; then
	AC_MSG_CHECKING(for $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,[(condition-case nil (require '$library ) (error (prog1 nil (message \"$library not found\"))))],"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
if test "${EMACS_cv_SYS_$1}" = "$library"; then
	EMACS_cv_SYS_$1=yes
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
if test -z "$2"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Perform sanity checking and try to locate the W3 package
dnl
AC_DEFUN(AC_CHECK_AUCTEX, [
AC_MSG_CHECKING(for acceptable AUC-TeX version)
AC_ARG_WITH(tex-site,[  --with-tex-site=DIR       Location of AUC-TeX's tex-site.el, if not standard], 
 [ AUCTEXDIR=${withval} ; 
   if test ! -d $AUCTEXDIR  ; then
      AC_MSG_ERROR([--with-tex-site=$AUCTEXDIR: Directory does not exist])
   fi
])
if test -z "$AUCTEXDIR" ; then
  AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_AUCTEX,[
  AC_EMACS_CHECK_REQUIRE(tex_site)
  if test "${HAVE_tex_site}" = "yes"; then
  	EMACS_cv_ACCEPTABLE_AUCTEX=yes
  else
	EMACS_cv_ACCEPTABLE_AUCTEX=no
  fi

  if test "${EMACS_cv_ACCEPTABLE_AUCTEX}" = "yes"; then
	AC_EMACS_LISP(auctex_dir, [(file-name-directory (locate-library \"tex-site\"))] ,"noecho")
	EMACS_cv_ACCEPTABLE_AUCTEX=$EMACS_cv_SYS_auctex_dir
  else
	AC_MSG_ERROR([Can't find AUC-TeX!  Please install it!  Check the PROBLEMS file for details.])
  fi
  ])
   AUCTEXDIR=${EMACS_cv_ACCEPTABLE_AUCTEX}
fi

   AC_SUBST(AUCTEXDIR)
   AC_MSG_RESULT("${AUCTEXDIR}")
])
