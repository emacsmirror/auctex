# serial 1

dnl mostly stolen from emacs-w3m, credit to Katsumi Yamaoka
dnl <yamaoka@jpl.org>

AC_DEFUN(AC_EXAMINE_PACKAGEDIR,
 [dnl Examine packagedir.
  tmpprefix="${prefix}"
  AC_FULL_EXPAND(tmpprefix)
  AC_EMACS_LISP(packagedir,
    [(let* (\
           (putative-existing-lisp-dir (locate-library \"$1\"))\
           (package-dir\
           (and putative-existing-lisp-dir\
	        (setq putative-existing-lisp-dir\
		      (file-name-directory putative-existing-lisp-dir))\
                (string-match \"[\\\\/]\\\\(lisp[\\\\/]\\\\)?\\\\($1[\\\\/]\\\\)?\$\"\
                               putative-existing-lisp-dir)\
                (replace-match \"\" t t putative-existing-lisp-dir 0))))\
      (if (and (boundp (quote early-packages))\
               (not package-dir))\
	  (let ((dirs (append (if early-package-load-path early-packages)\
			      (if late-package-load-path late-packages)\
			      (if last-package-load-path last-packages))))\
	    (while (and dirs (not package-dir))\
	      (if (file-directory-p (car dirs))\
		  (setq package-dir (car dirs))\
		  (setq	dirs (cdr dirs))))))\
      (if package-dir\
	  (progn\
	    (if (string-match \"[\\\\/]\$\" package-dir)\
		(setq package-dir (substring package-dir 0\
					     (match-beginning 0))))\
	    (if (and prefix\
		     (progn\
		       (setq prefix (file-name-as-directory prefix))\
		       (eq 0 (string-match (regexp-quote prefix)\
					   package-dir))))\
		(replace-match (file-name-as-directory \"\$(prefix)\") t t package-dir)\
	      package-dir))\
	\"NONE\"))],
    [noecho],,[prefix],["${tmpprefix}"])])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for packagedir.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([for XEmacs package directory])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "${withval}" = yes -o -z "${withval}"; then
	AC_EXAMINE_PACKAGEDIR($1)
      else
	packagedir="`echo ${withval} | sed 's/~\//${HOME}\//'`"
      fi],
      [AC_EXAMINE_PACKAGEDIR($1)])
    if test -z "${packagedir}"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT(${packagedir})
    fi
  else
    packagedir=
  fi
  AC_SUBST(packagedir)])


AC_DEFUN(AC_PATH_TEXMFDIR,
 [
AC_ARG_WITH(texmf-dir,[  --with-texmf-dir=DIR    TEXMF tree to install into],
 [ texmfdir="${withval}" ;
   AC_FULL_EXPAND(withval) 
   if test ! -d "$withval"  ; then
      AC_MSG_ERROR([--with-texmf-dir="$texmfdir": Directory does not exist])
   fi
   previewtexmfdir='$(texmfdir)/tex/latex/preview'
   previewdocdir='$(texmfdir)/doc/latex/styles'
   ])

AC_ARG_WITH(tex-dir,
 [  --with-tex-dir=DIR      Location to install preview TeX sources],
 [ previewtexmfdir="${withval}" ; 
   AC_FULL_EXPAND(withval)
   if test ! -d "$withval"  ; then
      AC_MSG_ERROR([--with-tex-dir="$previewtexmfdir": Directory does not exist])
   fi
   ])

AC_ARG_WITH(doc-dir,
  [  --with-doc-dir=DIR      Location to install preview.dvi],
  [ previewdocdir="${withval}" ; 
    AC_FULL_EXPAND(withval)
   if test ! -d "$withval"  ; then
      AC_MSG_ERROR([--with-doc-dir="$previewdocdir": Directory does not exist])
   fi
   ])

# First check for docstrip.cfg information

if test -z "$previewtexmfdir" ; then
    AC_MSG_CHECKING([for docstrip directory configuration])
    cat > testdocstrip.tex <<\EOF
\input docstrip
\ifx\basedir\undefined\else
   \message{^^J--preview-tex-dir=\showdirectory{tex/latex/preview}^^J%
               --texmf-prefix=\basedir^^J}
\fi
\endbatchfile
EOF
    "$LATEX" '\nonstopmode \input testdocstrip' >&5 2>&1
    texmfdir=`sed -n -e 's+/* *$++' -e '/^--texmf-prefix=/s///p' testdocstrip.log 2>&5`
    previewtexmfdir=`sed -n -e '/UNDEFINED/d' -e 's+/* *$++' -e '/^--preview-tex-dir=/s///p' testdocstrip.log 2>&5 `
    if test -z "$previewtexmfdir"  ; then
	if test ! -z "$texmfdir"  ; then
	    previewtexmfdir='$(texmfdir)'
	    previewdocdir='$(texmfdir)'
	    
	fi
    else
	previewdocdir='$(texmfdir)/doc/latex/styles'
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
for x in `kpsepath -n latex tex | tr ':' '\\n' | sed -e 's/^!!//' | \
 		grep '^/.*/tex/latex//$' `
do
  x="`echo $x | sed -e 's+//+/+g' -e 's+/\$++' `"
  if test -d "$x"  ; then
     texmfdir="`echo $x | sed -e 's+/tex/latex++'`"
     previewdocdir='$(texmfdir)/doc/latex/styles'
     previewtexmfdir='$(texmfdir)/tex/latex/preview'
     break
  fi
done
fi

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
AC_MSG_CHECKING([for TeX directory hierarchy])
for x in `kpsepath -n latex tex | tr ':' '\\n' | sed -e 's/^!!//' | \
 		grep '^/.*//$'`
do
  if test -d "$x"  ; then
     texmfdir="$x"
     previewtexmfdir='$(texmfdir)/preview'
     previewdocdir='$(texmfdir)/preview'
     break
  fi
done
fi

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
AC_MSG_CHECKING([for TeX input directory])
for x in `kpsepath -n latex tex | tr ':' '\\n' | sed -e 's/^!!//' | \
 		grep '^/'`
do
  if test -d "$x"  ; then
     texmfdir="$x"
     previewdocdir='$(texmfdir)'
     break
  fi
done
fi

if test -z "$previewtexmfdir"  ; then
AC_MSG_RESULT([no])
	AC_MSG_ERROR([Cannot find the texmf directory!  
Please use --with-texmf-dir=dir to specify where the preview tex files go])
fi
     AC_MSG_RESULT($texmfdir)
fi

echo Preview will be placed in $previewtexmfdir
echo Preview docs will be placed in $previewdocdir
AC_SUBST(texmfdir)
AC_SUBST(previewtexmfdir)
AC_SUBST(previewdocdir)])


AC_DEFUN(AC_FULL_EXPAND,
[ while :;do case "[$]$1" in *\[$]*) eval "$1=\"`sed 's/[[\\\`"]]/\\\\&/' <<EOF
[$]$1
EOF
`\"" ;; *) break ;; esac; done ])




dnl AC_EMACS_LISP AC_XEMACS_P AC_PATH_LISPDIR and AC_EMACS_CHECK_LIB
dnl are stolen from w3.
dnl AC_PATH_LISPDIR obsoletes AM_PATH_LISPDIR.

AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS} -batch -no-site-file $4 -eval "(let* (patsubst([$5], [\w+], [(\&(pop command-line-args-left))])(x ${elisp})) (write-region (if (stringp x) (princ x 'ignore) (prin1-to-string x)) nil \"${OUTPUT}\"))" $6 >& AC_FD_CC 2>&1
	${EMACS} -batch $4 -eval "(let* (patsubst([$5], [\w+], [(\&(pop command-line-args-left))])(x ${elisp})) (write-region (if (stringp x) (princ x 'ignore) (prin1-to-string x)) nil \"${OUTPUT}\"))" $6 >& AC_FD_CC 2>&1
	retval="`cat ${OUTPUT}`"
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1=$retval
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])


AC_DEFUN(AC_PROG_EMACS, [
# Check for (x)emacs, report its' path and flavour

# Apparently, if you run a shell window in Emacs, it sets the EMACS
# environment variable to 't'.  Lets undo the damage.
if test "${EMACS}" = "t"; then
   EMACS=""
fi
AC_ARG_WITH(emacs,
  [  --with-emacs@<:@=PATH@:>@     Use Emacs to build (on PATH if given)],
  [if test "${withval}" = "yes"; then EMACS=emacs
   else if test "${withval}" = "no"; then EMACS=xemacs
   else EMACS="${withval}"; fi ; fi])
AC_ARG_WITH(xemacs,
  [  --with-xemacs@<:@=PATH@:>@    Use XEmacs to build (on PATH if given)], 
  [if test "${withval}" = "yes"; then EMACS=xemacs; 
   else if test "${withval}" = "no"; then EMACS=emacs
   else EMACS="${withval}"; fi ; fi])

AC_PATH_PROGS(EMACS, $EMACS emacs xemacs)
if test -z "$EMACS"; then
  AC_MSG_ERROR([(X)Emacs not found!  Aborting!])
fi

AC_MSG_CHECKING([if $EMACS is XEmacs])
AC_EMACS_LISP(XEMACS,
	[(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\")],quiet)
if test "$XEMACS" = "yes"; then
  EMACS_FLAVOR=xemacs
else
  EMACS_FLAVOR=emacs
fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)
])


AC_DEFUN(AC_PATH_LISPDIR, [
  if test "$prefix" = "NONE"; then
     AC_MSG_CHECKING([prefix for your Emacs])
     AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),
		   "noecho")
     prefix=${EMACS_cv_SYS_prefix}
     AC_MSG_RESULT($prefix)
  fi
  AC_MSG_CHECKING([where lisp files go])
  AC_ARG_WITH(lispdir,
     [  --with-lispdir=DIR      Where to install lisp files], 
     [lispdir="${withval}"],
[    dnl Set default value
     theprefix="$prefix"
     if test "x$theprefix" = "xNONE"; then
        theprefix="$ac_default_prefix"
     fi
     lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp"
     for thedir in share lib; do
	AC_FULL_EXPAND(withval)
	if test -d "${withval}"; then
           lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp"
           break
        fi
     done
     if test -n "$2"; then 
        lispdir="$lispdir/$2"
     fi])
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

AC_DEFUN(AC_PATH_PROGS_REQUIRED, [
AC_PATH_PROGS($1, $2, NONE)
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
dnl Check whether a library is require'able
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_REQUIRE, [
if test -z "$2"; then
	AC_MSG_CHECKING(for $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1, 
	[(condition-case nil (require '$library ) \
	(error (prog1 nil (message \"$library not found\"))))],"noecho")
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
AC_MSG_CHECKING(for the location of AUC TeX's tex-site.el)
AC_ARG_WITH(tex-site,[  --with-tex-site=DIR     Location of AUC TeX's tex-site.el, if not standard], 
 [ auctexdir="${withval}" ; 
   AC_FULL_EXPAND(withval)
   if test ! -d "$withval"  ; then
      AC_MSG_ERROR([--with-tex-site=$auctexdir: Directory does not exist])
   fi
])
if test -z "$auctexdir" ; then
  AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_AUCTEX,[
  AC_EMACS_CHECK_REQUIRE(tex_site,silent)
  if test "${HAVE_tex_site}" = "yes"; then
  	EMACS_cv_ACCEPTABLE_AUCTEX=yes
  else
	EMACS_cv_ACCEPTABLE_AUCTEX=no
  fi

  if test "${EMACS_cv_ACCEPTABLE_AUCTEX}" = "yes"; then
	AC_EMACS_LISP(auctex_dir, 
		[(let ((aucdir (file-name-directory\
                         (locate-library \"tex-site\"))))\
                         (if (string-match \"[\\\\/]\$\" aucdir)\
                             (replace-match \"\" t t aucdir 0)\
			   aucdir))], 
		"noecho")
	EMACS_cv_ACCEPTABLE_AUCTEX=$EMACS_cv_SYS_auctex_dir
  else
	AC_MSG_ERROR([Can't find AUC-TeX!  Please install it!  
Check the PROBLEMS file for details.])
  fi
  ])
   auctexdir=${EMACS_cv_ACCEPTABLE_AUCTEX}
fi

   AC_MSG_RESULT(${auctexdir})
   #echo ${auctexdir}
   #  [echo ${auctexdir} | sed 's/\\/&&/g']
	#   auctexdir=`echo ${auctexdir} | sed 's/\\\\/&&/g'`
   AC_SUBST(auctexdir)
])

dnl
dnl AC_CHECK_MACRO_MAKEINFO( MACRO, [ACTION-IF-FOUND 
dnl					[, ACTION-IF-NOT-FOUND]])
dnl
AC_DEFUN(AC_CHECK_MACRO_MAKEINFO,
[if test -n "$MAKEINFO" -a "$makeinfo" != ":"; then
  AC_MSG_CHECKING([if $MAKEINFO understands @$1{}])
  echo \\\\input texinfo >test.texi
  echo @$1{test} >>test.texi
  if $MAKEINFO test.texi > /dev/null 2> /dev/null; then
    AC_MSG_RESULT(yes)	
    ifelse([$2], , :, [$2])
  else  
    AC_MSG_RESULT(no)	
    ifelse([$3], , :, [$3])
  fi
  rm -f test.texi test.info
fi
])

dnl
dnl AC_CHECK_MACROS_MAKEINFO( MACRO ... [, ACTION-IF-FOUND 
dnl					[, ACTION-IF-NOT-FOUND]])
dnl
AC_DEFUN(AC_CHECK_MACROS_MAKEINFO,
[for ac_macro in $1; do
    AC_CHECK_MACRO_MAKEINFO($ac_macro, $2, 
	[MAKEINFO_MACROS="-D no-$ac_macro $MAKEINFO_MACROS"
	$3])dnl
  done
AC_SUBST(MAKEINFO_MACROS)
])

AC_DEFUN(AC_SHELL_QUOTIFY,
[$1=["`sed 's/[^-0-9a-zA-Z_./:$]/\\\\&/g;s/[$]\\\\[{(]\\([^)}]*\\)\\\\[})]/${\\1}/g' <<EOF]
[$]$1
EOF
`"])
