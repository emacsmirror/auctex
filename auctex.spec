%define FOR_SUSE    %{?suse_version:1}%{!?suse_version:0}

%if %{FOR_SUSE}
%define distri       .suse
%define commongroup  Productivity/Editors/Emacs
%define xemacspkgdir %{_datadir}/xemacs/xemacs-packages
%define startupfile  %{_datadir}/emacs/site-lisp/suse-start-%{name}.el
%else
%define distri       .fedora
%define commongroup  Applications/Editors
%define xemacspkgdir %{_datadir}/xemacs/site-packages
%define startupfile  %{_datadir}/emacs/site-lisp/site-start.d/%{name}-init.el
%endif

Summary: 	Enhanced TeX modes for Emacsen
Name: 		auctex
Version: 	11.52
Release: 	2%{distri}
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
Obsoletes:      ge_auc emacs-auctex auctex
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

The package enables AUCTeX system-wide.  If you do not want this,
install/upgrade with 'rpm --nopre ...'  (the activation is done in the
preinstall script).

%prep
%setup

%build
# The below will make the package build from a tar straight from CVS
# NOT RECOMMENDED, but useful for testing!
test -f ./configure || ./autogen.sh
# --with-texmf-dir overrides local docstrip configurations.
%configure "--with-emacs" '--with-texmf-dir=%{_datadir}/texmf'
make
pushd doc
make auctex tex-ref.pdf
popd

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}{%{_datadir}/emacs/site-lisp,%{_infodir}}
%makeinstall install-contrib install-info

# Remove dir file that has been created by the makeinfo calls because this
# file will not been included in the rpm distribution (make RPM 4.1+ happy)
rm -f '%{buildroot}%{_infodir}/dir'

%pre emacs
echo "; Autoactivation of AUCTeX" > %{startupfile}
echo "; Created for %{name}-%{version}-%{release}.noarch.rpm" >> \
  %{startupfile}
echo "(require 'tex-site)" >> %{startupfile}

%post emacs
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/auctex

%preun emacs
# $1 is the number of versions of this package installed
# after this uninstallation
if [ $1 -eq 0 ]; then
  /sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/auctex
  rm -f %{startupfile}
fi
%clean
rm -rf %{buildroot}

%files emacs
%defattr(-,root,root)
%doc RELEASE COPYING INSTALL README TODO FAQ CHANGES
%doc doc/tex-ref.pdf
%doc %{_infodir}/*
%{_datadir}/emacs/site-lisp/%{name}
%{_localstatedir}/%{name}
%config %{_datadir}/emacs/site-lisp/tex-site.el

%changelog
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
