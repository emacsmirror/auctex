# serial 1

dnl mostly stolen from emacs-w3m, credit to Katsumi Yamaoka
dnl <yamaoka@jpl.org>

AC_DEFUN(AC_EXAMINE_PACKAGEDIR,
 [dnl Examine packagedir.
  AC_EMACS_LISP(packagedir,
    (let* ((prefix \"${prefix}\")\
           (putative-existing-lisp-dir (locate-library \"auctex\"))\
           (putative-existing-package-dir\
           (and putative-existing-lisp-dir\
                (string-match \"lisp/auctex/tex-buf\.elc?\$\"\
                              putative-existing-lisp-dir)\
                 (replace-in-string putative-existing-lisp-dir\
                                    \"lisp/auctex/tex-buf\.elc?\$\" \"\")))\
           package-dir)\
      (if (and (boundp (quote early-packages))\
               (not putative-existing-package-dir))\
	  (let ((dirs (append (if putative-existing-package-dir\
                                  (list putative-existing-package-dir))\
                              (if early-package-load-path early-packages)\
			      (if late-package-load-path late-packages)\
			      (if last-package-load-path last-packages))))\
	    (while (and dirs (not package-dir))\
	      (if (file-directory-p (car dirs))\
		  (setq package-dir (car dirs)\
			dirs (cdr dirs))))))\
      (if putative-existing-package-dir\
          (setq package-dir putative-existing-package-dir))\
      (if package-dir\
	  (progn\
	    (if (string-match \"/\$\" package-dir)\
		(setq package-dir (substring package-dir 0\
					     (match-beginning 0))))\
	    (if (and prefix\
		     (progn\
		       (setq prefix (file-name-as-directory prefix))\
		       (eq 0 (string-match (regexp-quote prefix)\
					   package-dir))))\
		(replace-match \"\$(prefix)/\" nil nil package-dir)\
	      package-dir))\
	\"NONE\")),
    noecho)])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for packagedir.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([where the XEmacs package is])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "${withval}" = yes -o -z "${withval}"; then
	AC_EXAMINE_PACKAGEDIR
      else
	packagedir="`echo ${withval} | sed 's/~\//${HOME}\//'`"
      fi],
      AC_EXAMINE_PACKAGEDIR)
    if test -z "${packagedir}"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT(${packagedir})
    fi
  else
    packagedir=NONE
  fi
  AC_SUBST(packagedir)])


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
  lispdir="$lispdir/auctex"
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)
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
  AC_EMACS_CHECK_REQUIRE(tex_site,silent)
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

   AC_MSG_RESULT(in ${AUCTEXDIR})
echo ${AUCTEXDIR}
  [echo ${AUCTEXDIR} | sed 's/\\/&&/g']
	#   AUCTEXDIR=`echo ${AUCTEXDIR} | sed 's/\\\\/&&/g'`
   AC_SUBST(AUCTEXDIR)
])


dnl AC_CHECK_MACRO_MAKEINFO(MACRO, [ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(AC_CHECK_MACRO_MAKEINFO,
[AC_MSG_CHECKING([if $MAKEINFO understands @$1{}])
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
])

dnl AC_CHECK_MACROS_MAKEINFO(FUNCTION... [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
AC_DEFUN(AC_CHECK_MACROS_MAKEINFO,
[if test -n "$MAKEINFO"; then
  for ac_macro in $1; do
    AC_CHECK_MACRO_MAKEINFO($ac_macro, $2, [MAKEINFO_MACROS="-D no-$ac_macro $MAKEINFO_MACROS"
    $3])dnl
  done
fi
AC_SUBST(MAKEINFO_MACROS)
])



AC_DEFUN(AC_PATH_PROGS_REQUIRED, [
AC_PATH_PROGS($1, $2, NONE)
if test "${$1}"x = NONEx ; then
   AC_MSG_ERROR([$3])
fi
])
