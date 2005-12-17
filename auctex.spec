%define FOR_SUSE    %{?suse_version:1}%{!?suse_version:0}

%if %{FOR_SUSE}
%define distri       .suse
%define commongroup  Productivity/Editors/Emacs
%define xemacspkgdir %{_datadir}/xemacs/xemacs-packages
%else
%define distri       .fedora
%define commongroup  Applications/Editors
%define xemacspkgdir %{_datadir}/xemacs/site-packages
%endif

Summary: 	Enhanced TeX modes for Emacsen
Name: 		auctex
Version: 	11.82
Release: 	0%{distri}
License: 	GPL
Group: 		%{commongroup}
URL: 		http://www.gnu.org/software/auctex/
Source0:        ftp://ftp.gnu.org/pub/gnu/auctex/%{name}-%{version}.tar.gz
BuildArchitectures: noarch
BuildRoot: 	%{_tmppath}/%{name}-root

%description
AUCTeX is an extensible package that supports writing and formatting TeX files
for most variants of Emacs.  

AUCTeX supports many different TeX macro packages, including AMS-TeX, LaTeX,
Texinfo and basic support for ConTeXt.  Documentation can be found under
/usr/share/doc, e.g. the reference card (tex-ref.pdf) and the FAQ.  The AUCTeX
manual is available in Emacs info (C-h i d m AUCTeX RET).  On the AUCTeX home
page, we provide manuals in various formats.

%package emacs
Summary: 	Enhanced TeX modes for GNU Emacs
Group:          %{commongroup}
Requires: 	emacs >= 21
Obsoletes:      ge_auc emacs-auctex auctex preview-latex-common preview-latex-emacs
Conflicts:      emacspeak < 18
Provides:       auctex

%description emacs
AUCTeX is an extensible package that supports writing and formatting TeX files
for most variants of Emacs.  

AUCTeX supports many different TeX macro packages, including AMS-TeX, LaTeX,
Texinfo and basic support for ConTeXt.  Documentation can be found under
/usr/share/doc, e.g. the reference card (tex-ref.pdf) and the FAQ.  The AUCTeX
manual is available in Emacs info (C-h i d m AUCTeX RET).  On the AUCTeX home
page, we provide manuals in various formats.

This package is for GNU Emacs.  XEmacs users should use the package system for
installation.

The package enables AUCTeX modes system-wide.  The README file
contains information how users may override this choice.

%prep
%setup

%build
# The below will make the package build from a tar straight from CVS
# NOT RECOMMENDED, but useful for testing!
test -f ./configure || ./autogen.sh
%configure --with-emacs %{extraconfig} INSTALL_INFO=: --without-texmf-dir
make
pushd doc
make tex-ref.pdf
popd

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}{%{_datadir}/emacs/site-lisp,%{_infodir}}
%if %{FOR_SUSE}
cat <<EOFA > %{buildroot}%{_datadir}/emacs/site-lisp/suse-start-auctex.el
;; suse-start-auctex.el
;; This file enables AUCTeX globally:
(load "auctex.el" nil t t)
;; See (info "(auctex)Introduction") on how to disable AUCTeX.
EOFA
cat <<EOFP > %{buildroot}%{_datadir}/emacs/site-lisp/suse-start-preview-latex.el
;; suse-start-preview-latex.el
;; This file enables preview-latex globally:
(load "preview-latex.el" nil t t)
EOFP
%else
mkdir -p %{buildroot}%{_datadir}/emacs/site-lisp/site-start.d
%endif
%makeinstall install-docs

%post emacs
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/auctex.info
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/preview-latex.info

%preun emacs
# $1 is the number of versions of this package installed
# after this uninstallation
if [ $1 -eq 0 ]; then
  /sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/auctex.info
  /sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/preview-latex.info
fi
%clean
rm -rf %{buildroot}

%files emacs
%defattr(-,root,root)
%doc RELEASE COPYING INSTALL README TODO FAQ CHANGES
%doc doc/tex-ref.pdf
# %doc --parents preview/RELEASE preview/README preview/INSTALL preview/TODO preview/FAQ
%doc %{_infodir}/*
%exclude %{_infodir}/dir
%{_datadir}/emacs/site-lisp/%{name}
%{_datadir}/emacs/site-lisp/preview
%{_localstatedir}/%{name}
%config %{_datadir}/emacs/site-lisp/tex-site.el
%if %{FOR_SUSE}
%{_datadir}/emacs/site-lisp/auctex.el
%{_datadir}/emacs/site-lisp/preview-latex.el
%{_datadir}/emacs/site-lisp/suse-start-auctex.el
%{_datadir}/emacs/site-lisp/suse-start-preview-latex.el
%else
%{_datadir}/emacs/site-lisp/site-start.d/auctex.el
%{_datadir}/emacs/site-lisp/site-start.d/preview-latex.el
%endif


%changelog
# Shouldn't changelog include changes in the package instead of changes in the
# spec file?
* Tue May  3 2005 David Kastrup <dak@gnu.org>
- include preview-latex, so outdate other stuff.

* Fri Jan 21 2005 David Kastrup <dak@gnu.org>
- Conflict with outdated Emacspeak versions

* Fri Jan 14 2005 David Kastrup <dak@gnu.org>
- Install and remove auctex.info, not auctex

* Thu Aug 19 2004 David Kastrup <dak@gnu.org>
- Change tex-site.el to overwriting config file mode.  New naming scheme.

* Mon Aug 16 2004 David Kastrup <dak@gnu.org>
- Attempt a bit of SuSEism.  Might work if we are lucky.

* Sat Dec  7 2002 David Kastrup <David.Kastrup@t-online.de>
- Change addresses to fit move to Savannah.

* Mon Apr 15 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Adjusted TeX-macro-global and put autoactivation in preinstall
  script so that it can be chosen at install time.

* Tue Feb 19 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Added site-start.el support

* Sat Feb 16 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Prerelease 11.11
