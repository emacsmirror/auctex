# serial 1

dnl mostly stolen from emacs-w3m, credit to Katsumi Yamaoka
dnl <yamaoka@jpl.org>

AC_DEFUN(EMACS_EXAMINE_PACKAGEDIR,
 [dnl Examine packagedir.
  dnl $2 here is only for correcting old (CVS) mistakes
  tmpprefix="${prefix}"
  AC_FULL_EXPAND(tmpprefix)
  EMACS_LISP(packagedir,
    [(let* (\
	   (putative-existing-lisp-dir (locate-library \"$1\"))\
	   (package-dir\
	   (and putative-existing-lisp-dir\
		(setq putative-existing-lisp-dir\
		      (file-name-directory putative-existing-lisp-dir))\
		(string-match \"[[\\\\/]]lisp[[\\\\/]]\\\\($2[[\\\\/]]\\\\)?\$\"\
			       putative-existing-lisp-dir)\
		(replace-match \"\" t t putative-existing-lisp-dir))))\
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
	    (if (string-match \"[[\\\\/]]\$\" package-dir)\
		(setq package-dir (substring package-dir 0\
					     (match-beginning 0))))\
	    (if (and prefix\
		     (progn\
		       (setq prefix (file-name-as-directory prefix))\
		       (eq 0 (string-match (regexp-quote prefix)\
					   package-dir))))\
		(replace-match (file-name-as-directory \"\${prefix}\") t t package-dir)\
	      package-dir))\
	\"NONE\"))],
    [noecho],,[prefix],["${tmpprefix}"])])

AC_DEFUN(EMACS_PATH_PACKAGEDIR,
 [dnl Check for packagedir.
  dnl $2 here is only for correcting old (CVS) mistakes
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([for XEmacs package directory])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "${withval}" = yes -o -z "${withval}"; then
	EMACS_EXAMINE_PACKAGEDIR($1,$2)
      else
	packagedir="`echo ${withval} | sed 's/^~\//${HOME}\//;s/[[\/\\]]$//'`"
      fi],
      [EMACS_EXAMINE_PACKAGEDIR($1,$2)])
    if test -z "${packagedir}"; then
      AC_MSG_ERROR([not found, exiting!])
    fi
    AC_MSG_RESULT(${packagedir})
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
   previewtexmfdir='${texmfdir}/tex/latex/preview'
   previewdocdir='${texmfdir}/doc/latex/styles'
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
	    previewtexmfdir='${texmfdir}'
	    previewdocdir='${texmfdir}'
	fi
    else
	previewdocdir='${texmfdir}/doc/latex/styles'
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
     previewdocdir='${texmfdir}/doc/latex/styles'
     previewtexmfdir='${texmfdir}/tex/latex/preview'
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
     previewtexmfdir='${texmfdir}/preview'
     previewdocdir='${texmfdir}/preview'
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
     previewdocdir='${texmfdir}'
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
[ while :;do case "[$]$1" in *\[$]*) __ac_tmp__='s/[[\`"]]/\\&/g'
eval "$1=`sed ${__ac_tmp__} <<EOF
[$]$1
EOF
`";; *) break ;; esac; done ])
dnl "



dnl EMACS_LISP EMACS_PROG_EMACS EMACS_PATH_LISPDIR and EMACS_CHECK_LIB
dnl adapted from w3.

dnl EMACS_LISP takes 6 arguments.  $1 is the name of the shell
dnl variable to assign a value, $2 is a Lisp expression placed into
dnl shell double quotes (which has consequences for quoting and
dnl variable expansion).  $3 is ignored; it is there for historical
dnl reasons.  $4 is a list of Emacs options evaluated before the
dnl expression itself, $5 is a list of Elisp variables that is
dnl assigned from the command line arguments from $6.

AC_DEFUN(EMACS_LISP, [
  elisp="$2"
  OUTPUT=./conftest-$$
  echo "${EMACS}" -batch $4 -eval "(let* (patsubst([$5], [\w+], [(\&(pop command-line-args-left))])(x ${elisp})) (write-region (if (stringp x) x (prin1-to-string x)) nil \"${OUTPUT}\"))" $6 >& AC_FD_CC 2>&1
  "${EMACS}" -batch $4 -eval "(let* (patsubst([$5], [\w+], [(\&(pop command-line-args-left))])(x ${elisp})) (write-region (if (stringp x) x (prin1-to-string x)) nil \"${OUTPUT}\"))" $6 >& AC_FD_CC 2>&1
  $1="`cat ${OUTPUT}`"
  echo "=> ${1}" >& AC_FD_CC 2>&1
  rm -f ${OUTPUT}
])


AC_DEFUN(EMACS_PROG_EMACS, [
# Check for (X)Emacs, report its path, flavor and version

# Apparently, if you run a shell window in Emacs, it sets the EMACS
# environment variable to 't'.  Let's undo the damage.
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

# "${prefix}/bin" is for Windows users
AC_PATH_PROGS(EMACS, $EMACS emacs xemacs, "", $PATH "${prefix}/bin" )
if test -z "$EMACS"; then
  AC_MSG_ERROR([(X)Emacs not found!  Aborting!])
fi

AC_MSG_CHECKING([if $EMACS is XEmacs])
EMACS_LISP(XEMACS,
	[(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\")])
if test "$XEMACS" = "yes"; then
  EMACS_FLAVOR=xemacs
else
  if test "$XEMACS" = "no"; then
    EMACS_FLAVOR=emacs
  else
    AC_MSG_ERROR([Unable to run $EMACS!  Aborting!])
  fi
fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)

AC_MSG_CHECKING([if (X)Emacs is recent enough])
EMACS_LISP(EMACS_MAJOR_VERSION,[emacs-major-version])
if (( $EMACS_MAJOR_VERSION < 21 )); then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([AUCTeX requires at least (X)Emacs version 21.  Aborting!])
else
  AC_MSG_RESULT([yes])
fi
  AC_SUBST(EMACS_MAJOR_VERSION)
])



dnl "\${packagedir}/lisp"

AC_DEFUN(EMACS_TEST_LISPDIR, [
  for i in "\${datadir}/${EMACS_FLAVOR}/site-lisp" \
	   "\${libdir}/${EMACS_FLAVOR}/site-lisp" \
	   "\${libdir}/${EMACS_FLAVOR}/site-packages/lisp" \
	   "\${datadir}/${EMACS_FLAVOR}/site-packages/lisp" \
	   "\${prefix}/site-lisp" ; do
    lispdir="$i"
    AC_FULL_EXPAND(i)
    EMACS_LISPDIR=""
    EMACS_LISP(EMACS_LISPDIR,
      [[(let ((load-path load-path)
	     (pattern (concat \"^\" (regexp-quote cmdpath) \"[/\\\\]?\$\")))
	 (while (and load-path (not (string-match pattern
						  (car load-path))))
		(setq load-path (cdr load-path)))
	 (if load-path \"yes\" \"no\"))]],,,[cmdpath],["$i"])
    if test "$EMACS_LISPDIR" = "yes"; then
      break
    fi
  done
  if test "$EMACS_LISPDIR" = "no"; then
    lispdir="NONE"
  fi
])


AC_DEFUN(EMACS_PATH_LISPDIR, [
  AC_MSG_CHECKING([where lisp files go])
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      Where to install lisp files],
    [lispdir="${withval}"
     # Store expanded path minus trailing slash, may be added to (X)Emacs load-path
     lispdir_expanded="`echo $lispdir | sed 's/[[\/\\]]$//'`"
     AC_FULL_EXPAND(lispdir_expanded)
    ],
    [
     # Save prefix
     oldprefix=${prefix}
     oldexec_prefix=${exec_prefix}
     if test "${prefix}" = "NONE"; then
       # Set prefix temporarily
       prefix="${ac_default_prefix}"
     fi
     if test "${exec_prefix}" = "NONE"; then
       # Set exec_prefix temporarily
	exec_prefix="${prefix}"
     fi
     if test "${EMACS_FLAVOR}" = 'emacs' -o "${packagedir}" = 'no'; then
       # Test paths relative to prefixes
       EMACS_TEST_LISPDIR
       if test "$lispdir" = "NONE"; then
	 # No? Test paths relative to binary
	 EMACS_LISP(prefix,[(expand-file-name \"..\" invocation-directory)])
	 exec_prefix=${prefix}
	 EMACS_TEST_LISPDIR
	 AC_FULL_EXPAND(lispdir)
       fi
       if test "$lispdir" = "NONE"; then
	 # No? notify user.
	 AC_MSG_ERROR([Cannot locate lisp directory,
use  --with-lispdir, --datadir, or possibly --prefix to rectify this])
       fi
     else
       # XEmacs
       lispdir='${packagedir}/lisp'
     fi
     # Store expanded path, may be added to (X)Emacs load-path
     lispdir_expanded="$lispdir"
     AC_FULL_EXPAND(lispdir_expanded)
     # Restore prefix
     prefix=${oldprefix}
     exec_prefix=${oldexec_prefix}
    ])
  AC_MSG_RESULT([${lispdir}, expanded to ${lispdir_expanded}])
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
AC_DEFUN(EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
EMACS_LISP(EMACS_cv_SYS_$1,(progn (fmakunbound '$2) (condition-case nil (progn (require '$library) (fboundp '$2)) (error (prog1 nil (message \"$library not found\"))))),"noecho")
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
if test "$$1" = "nil"; then
	$1=no
fi
if test "$$1" = "$library"; then
	$1=yes
fi
HAVE_$1=$$1
AC_SUBST(HAVE_$1)
if test -z "$2"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Perform sanity checking and try to locate the auctex package
dnl
AC_DEFUN(EMACS_CHECK_AUCTEX, [
AC_MSG_CHECKING(for the location of AUCTeX's tex-site.el)
AC_ARG_WITH(tex-site,[  --with-tex-site=DIR     Location of AUCTeX's tex-site.el, if not standard],
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
    EMACS_LISP(EMACS_cv_ACCEPTABLE_AUCTEX,
	[[(let ((aucdir (file-name-directory (locate-library \"tex-site\"))))\
	   (if (string-match \"[\\\\/]\$\" aucdir)\
	       (replace-match \"\" t t aucdir)\
	       aucdir))]], "noecho")
  else
	AC_MSG_ERROR([Can't find AUCTeX!  Please install it!
Check the PROBLEMS file for details.])
  fi
  ])
  auctexdir=${EMACS_cv_ACCEPTABLE_AUCTEX}
fi
AC_MSG_RESULT(${auctexdir})
AC_SUBST(auctexdir)
])

dnl
dnl Check (X)Emacs supports Mule.
dnl
AC_DEFUN(EMACS_CHECK_MULE, [
AC_MSG_CHECKING(for mule support)
EMACS_CHECK_REQUIRE(mule,silent)
if test "${HAVE_mule}" = "yes"; then
  COMPILE_MULE="tex-jp.el"
  CONTRIB_MULEELC="tex-jp.elc"
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
AC_SUBST(COMPILE_MULE)
AC_SUBST(CONTRIB_MULEELC)
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

dnl
dnl Check if build directory is valid.
dnl The directory should not be part of `load-path'
dnl
AC_DEFUN(VALID_BUILD_DIR, [
  AC_MSG_CHECKING([if build directory is valid])
  EMACS_LISP(valid_build_dir,
    [[(if (or (member (directory-file-name default-directory) load-path)\
	      (member (file-name-as-directory default-directory) load-path))\
	 \"no\" \"yes\")]])
  if test "$valid_build_dir" = "no"; then
    AC_MSG_ERROR([Build directory inside load-path!  Aborting!])
  else
    AC_MSG_RESULT([yes])
  fi
])

dnl
dnl Determine directories which hold TeX input files.
dnl
AC_DEFUN(TEX_INPUT_DIRS,
 [
AC_ARG_WITH(tex-input-dirs,[  --with-tex-input-dirs="DIRS"
			  semicolon-separated DIRS for TeX file searches],
 [ texinputdirs="${withval}" ;
   AC_FULL_EXPAND(withval)
   texinputdirsout=""
   OLDIFS="$IFS"
   IFS=";"
   for x in ${texinputdirs} ; do
     IFS="$OLDIFS"
     if test ! -d "$x" ; then
       AC_MSG_ERROR([--with-tex-input-dirs="$x": Directory does not exist])
     fi
     if test "${texinputdirsout}" != "" ; then
       texinputdirsout="${texinputdirsout} "
     fi
     texinputdirsout="${texinputdirsout}\"$x\""
     IFS=";"
   done
   IFS="$OLDIFS"
   texinputdirs="${texinputdirsout}"
   ])

if test -z "$texinputdirs" ; then
  AC_MSG_CHECKING([for directories holding TeX input files])
  texinputdirs=""
  temp=`kpsewhich --progname latex --expand-braces \\$SYSTEXMF 2> /dev/null`
  if test $? -ne 0 ; then
    temp=""
    temp1=`kpsewhich --progname latex --expand-braces \\$TEXMFLOCAL 2> /dev/null`
    if test $? -ne 0 ; then
      temp1=`kpsewhich --progname latex --expand-path \\$TEXMFLOCAL`
    fi
    temp2=`kpsewhich --progname latex --expand-braces \\$TEXMFMAIN 2> /dev/null`
    if test $? -ne 0 ; then
      temp2=`kpsewhich --progname latex --expand-path \\$TEXMFMAIN`
    fi
    temp3=`kpsewhich --progname latex --expand-braces \\$TEXMFDIST 2> /dev/null`
    if test $? -ne 0 ; then
      temp3=""
    fi
    for i in $temp1 $temp2 $temp3 ; do
      if ! echo $i | grep '^[[A-Za-z]]:' > /dev/null && \
	 ! echo $i | grep ';' > /dev/null ; then
	i=`echo $i | tr ':' ';'`
      fi
      if ! test -z "$i" ; then
	if ! test -z "$temp" ; then
	  temp=$temp";"
	fi
	temp="$temp""$i"
      fi
    done
  else
    if ! echo $temp | grep '^[[A-Za-z]]:' > /dev/null && \
       ! echo $temp | grep ';' > /dev/null ; then
      temp=`echo $temp | tr ':' ';'`
    fi
  fi
  for x in `echo $temp | tr ';' '\\n' | sed -e 's/^!!//'` ; do
    # We assume that we are dealing with a TDS-compliant TeX system.
    for y in "/tex/" "/bibtex/bst/" ; do
      tempy="$x""$y"
      if test -e "$tempy" ; then
	if test "${texinputdirs}" != "" ; then
	  texinputdirs="${texinputdirs} "
	fi
	texinputdirs="${texinputdirs}\"${tempy}\""
      fi
    done
  done
  if test -z "$texinputdirs" ; then
    texinputdirs="\"/usr/share/texmf/tex/\" \"/usr/share/texmf/bibtex/bst/\""
  fi
  AC_MSG_RESULT((${texinputdirs}))
fi
AC_SUBST(texinputdirs)
])

# AUCTEX_AUTO_DIR
# ---------------
# Set the directory containing AUCTeX automatically generated global style
# hooks.
AC_DEFUN(AUCTEX_AUTO_DIR,
[AC_MSG_CHECKING([where automatically generated global style hooks go])
 AC_ARG_WITH(auto-dir,
	     [  --with-auto-dir=DIR     directory containing AUCTeX automatically generated
			  global style hooks],
	     [autodir="${withval}"
	      autodir_expanded="${autodir}"
	      AC_FULL_EXPAND(autodir_expanded)],
	     [autodir='${localstatedir}/auctex'
	      oldprefix="${prefix}" # save prefix
	      if test "${prefix}" = "NONE"
		then prefix="${ac_default_prefix}" # temporarily set it
	      fi
	      autodir_expanded="${autodir}"
	      AC_FULL_EXPAND(autodir_expanded)
	      prefix="${oldprefix}" # restore prefix])
 AC_MSG_RESULT([${autodir}, expanded to ${autodir_expanded}])
 AC_SUBST(autodir)
 AC_SUBST(autodir_expanded)
])
