# serial 1

dnl mostly stolen from emacs-w3m, credit to Katsumi Yamaoka
dnl <yamaoka@jpl.org>

AC_DEFUN(EMACS_EXAMINE_PACKAGEDIR,
 [dnl Examine packagedir.
  tmpprefix="${prefix}"
  AC_FULL_EXPAND(tmpprefix)
  EMACS_LISP(packagedir,
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

AC_DEFUN(EMACS_PATH_PACKAGEDIR,
 [dnl Check for packagedir.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([for XEmacs package directory])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "${withval}" = yes -o -z "${withval}"; then
	EMACS_EXAMINE_PACKAGEDIR($1)
      else
	packagedir="`echo ${withval} | sed 's/~\//${HOME}\//'`"
      fi],
      [EMACS_EXAMINE_PACKAGEDIR($1)])
    if test -z "${packagedir}"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT(${packagedir})
    fi
  else
    packagedir=
  fi
  AC_SUBST(packagedir)])


AC_DEFUN(TEX_PATH_TEXMFDIR,
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




dnl EMACS_LISP EMACS_PROG_EMACS EMACS_PATH_LISPDIR and EMACS_CHECK_LIB
dnl adapted from w3.

AC_DEFUN(EMACS_LISP, [
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


AC_DEFUN(EMACS_PROG_EMACS, [
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
EMACS_LISP(XEMACS,
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


AC_DEFUN(EMACS_PATH_LISPDIR, [
  AC_MSG_CHECKING([where lisp files go])
  AC_ARG_WITH(lispdir,
     [  --with-lispdir=DIR      Where to install lisp files], 
     lispdir="${withval}",
     [EMACS_PREFIX="${prefix}"	
     if test "$EMACS_PREFIX" = "NONE"; then
        EMACS_LISP(EMACS_PREFIX,
                   (expand-file-name \"..\" invocation-directory), silent)
     fi
     EMACS_DATADIR="${EMACS_PREFIX}/\\\\(share\\\\|lib\\\\)"
     if test $EMACS_FLAVOR = emacs; then
        if test "$datadir" != '${prefix}/share'; then
           EMACS_DATADIR="$datadir"
        fi
	EMACS_LISPDIR="${EMACS_DATADIR}/emacs/site-lisp"
     fi
     if test $EMACS_FLAVOR = xemacs; then
        if test "$libdir" != '${exec_prefix}/lib'; then
	   EMACS_DATADIR="$libdir"
        fi
	EMACS_LISPDIR="${EMACS_DATADIR}/xemacs/site-packages/lisp"
        if test -n "$packagedir"; then
  	   EMACS_LISPDIR="$packagedir/lisp"
        fi
     fi
     EMACS_LISP(lispdir,
        [(let ((load-path load-path))
            (while (and load-path (not (string-match 
                  		\"^${EMACS_LISPDIR}/?\$\" (car load-path))))
                   (setq load-path (cdr load-path))) 
            (if load-path 
                (substring (car load-path) 0 
			   (string-match \"/\$\" (car load-path))) 
		\"\"))], silent)
     if test -z "$lispdir"; then
	AC_MSG_ERROR([Cannot locate lisp directory

Use  --with-lispdir, --with-packagedir (xemacs), --datadir (emacs), 
--libdir (xemacs), or possibly --prefix to rectify this])
     fi
     if test -n "$1"; then 
        lispdir="$lispdir/$1"
     fi])
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)]
)

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
AC_DEFUN(EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
EMACS_LISP($1,(progn (fmakunbound '$2) (condition-case nil (progn (require '$library) (fboundp '$2)) (error (prog1 nil (message \"$library not found\"))))),"noecho")
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
AC_DEFUN(EMACS_CHECK_REQUIRE, [
if test -z "$2"; then
	AC_MSG_CHECKING(for $1)
fi
library=`echo $1 | tr _ -`
EMACS_LISP($1, 
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
AC_DEFUN(EMACS_CHECK_AUCTEX, [
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
  EMACS_CHECK_REQUIRE(tex_site,silent)
  if test "${HAVE_tex_site}" = "yes"; then
  	EMACS_cv_ACCEPTABLE_AUCTEX=yes
  else
	EMACS_cv_ACCEPTABLE_AUCTEX=no
  fi

  if test "${EMACS_cv_ACCEPTABLE_AUCTEX}" = "yes"; then
	EMACS_LISP(auctex_dir, 
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
dnl MAKEINFO_CHECK_MACRO( MACRO, [ACTION-IF-FOUND 
dnl					[, ACTION-IF-NOT-FOUND]])
dnl
AC_DEFUN(MAKEINFO_CHECK_MACRO,
[if test -n "$MAKEINFO" -a "$makeinfo" != ":"; then
  AC_MSG_CHECKING([if $MAKEINFO understands @$1{}])
  echo \\\\input texinfo > conftest.texi
  echo @$1{test} >> conftest.texi
  if $MAKEINFO conftest.texi > /dev/null 2> /dev/null; then
    AC_MSG_RESULT(yes)	
    ifelse([$2], , :, [$2])
  else  
    AC_MSG_RESULT(no)	
    ifelse([$3], , :, [$3])
  fi
  rm -f conftest.texi conftest.info
fi
])

dnl
dnl MAKEINFO_CHECK_MACROS( MACRO ... [, ACTION-IF-FOUND 
dnl					[, ACTION-IF-NOT-FOUND]])
dnl
AC_DEFUN(MAKEINFO_CHECK_MACROS,
[for ac_macro in $1; do
    MAKEINFO_CHECK_MACRO($ac_macro, $2, 
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
