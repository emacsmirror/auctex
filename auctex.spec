Summary: 	Enhanced LaTeX mode for GNU Emacs
Name: 		auctex
Version: 	11.13
Release: 	2
License: 	GPL
Group: 		Applications/Editors
URL: 		http://www.nongnu.org/auctex/
Source0:        http://savannah.nongnu.org/download/auctex/stable.pkg/%{version}/%{name}-%{version}.tar.gz
Requires: 	emacs >= 20
#BuildRequires: 	emacs-X11
BuildArchitectures: noarch
BuildRoot: 	%{_tmppath}/%{name}-root

%description 
AUC TeX is a comprehensive, customizable, integrated environment for
writing, editing and processing input files for LaTeX using GNU Emacs.

This .rpm enables AUC TeX system-wide.  
If you do not want this, install/upgrade with 'rpm --nopre ...' 
(the activation is done in the preinstall script).

%prep
%setup -q
cat >> tex-site.el << EOF
;; Correction: RedHat adjustments
;;
;; Variable from tex.el, with a default more fitting for RedHat
(defcustom TeX-macro-global '("/usr/share/texmf/tex/")
  "Directories containing the sites TeX macro files and style files.
The directory names *must* end with a slash."
  :group 'TeX-file
  :type '(repeat (directory :format "%v")))
EOF

%build
make
make contrib
( cd doc; make )

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}{%{_datadir}/emacs/site-lisp,%{_infodir}}
%makeinstall install-contrib install-info
sed -e "s#@AUCDIR#%{_datadir}/emacs/site-lisp/auctex/#" tex-site.el \
  > %{buildroot}%{_datadir}/emacs/site-lisp/tex-site.el

%pre
echo "; Autoactivation of AUC TeX" > \
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
%doc CHANGES COPYING PROBLEMS ChangeLog INSTALLATION README
%doc doc/*.dvi
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


