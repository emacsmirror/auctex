Summary: 	Enhanced LaTeX mode for GNU Emacs
Name: 		auctex
Version: 	11.51
Release: 	1
License: 	GPL
Group: 		Applications/Editors
URL: 		http://www.gnu.org/software/auctex/
Source0:        ftp://ftp.gnu.org/pub/gnu/auctex/%{name}-%{version}.tar.gz
Requires: 	emacs >= 21
#BuildRequires: 	emacs-X11
BuildArchitectures: noarch
BuildRoot: 	%{_tmppath}/%{name}-root

%description 
AUCTeX is a comprehensive, customizable, integrated environment for
writing, editing and processing input files for LaTeX using GNU Emacs.

For XEmacs, use XEmacs' own package system for installation.

This .rpm enables AUCTeX system-wide.  
If you do not want this, install/upgrade with 'rpm --nopre ...' 
(the activation is done in the preinstall script).

%prep
%setup

%build
# The below will make the package build from a tar straight from CVS
# NOT RECOMMENDED, but useful for testing!
test -f ./configure || ./autogen.sh
# --with-texmf-dir overrides local docstrip configurations.
%configure "--with-emacs" '--with-texmf-dir=%{_datadir}/texmf'
make 'infodir=%{_infodir}'
pushd doc
make 'infodir=%{_infodir}'
make auctex tex-ref.pdf
popd

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}{%{_datadir}/emacs/site-lisp,%{_infodir}}
%makeinstall install-contrib install-info

# Remove dir file that has been created by the makeinfo calls because this
# file will not been included in the rpm distribution (make RPM 4.1+ happy)
rm -f '%{buildroot}%{_infodir}/dir'

%pre
echo "; Autoactivation of AUCTeX" > \
  %{_datadir}/emacs/site-lisp/site-start.d/%{name}-init.el
echo "; Created for %{name}-%{version}-%{release}.noarch.rpm" >> \
  %{_datadir}/emacs/site-lisp/site-start.d/%{name}-init.el
echo "(require 'tex-site)" >> \
  %{_datadir}/emacs/site-lisp/site-start.d/%{name}-init.el

%post
/sbin/install-info --info-dir=%{_infodir} %{_infodir}/auctex

%preun
# $1 is the number of versions of this package installed
# after this uninstallation
if [ $1 -eq 0 ]; then
  /sbin/install-info --delete --info-dir=%{_infodir} %{_infodir}/auctex
  rm -f %{_datadir}/emacs/site-lisp/site-start.d/%{name}-init.el
fi
%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%doc RELEASE COPYING INSTALL README TODO FAQ CHANGES
%doc doc/tex-ref.pdf
%doc %{_infodir}/*
%{_datadir}/emacs/site-lisp/%{name}
%config(noreplace) %{_datadir}/emacs/site-lisp/tex-site.el

%changelog
* Sat Dec  7 2002 David Kastrup <David.Kastrup@t-online.de>
- Change addresses to fit move to Savannah.

* Mon Apr 15 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Adjusted TeX-macro-global and put autoactivation in preinstall
  script so that it can be chosen at install time.

* Tue Feb 19 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Added site-start.el support

* Sat Feb 16 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Prerelease 11.11


