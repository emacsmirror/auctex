%define HAVE_EMACS  %(which emacs  >/dev/null 2>/dev/null && echo 1 || echo 0)
%define HAVE_XEMACS %(which xemacs >/dev/null 2>/dev/null && echo 1 || echo 0)

Summary: 	Emacs/LaTeX inline preview 
Name: 		preview-latex
Version: 	0.7
Release: 	1
BuildArchitectures: noarch
URL: 		http://preview-latex.sourceforge.org
Source0: 	http://prdownloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz
License: 	GPL
Group: 		Applications/Editors
BuildRoot: 	%{_tmppath}/%{name}-root
Prereq:		info
Requires:	auctex >= 11.0
Requires:	ghostscript >= 6.51
Requires:	tetex tetex-dvips
BuildRequires:	texinfo >= 4.0

%description
Does your neck hurt from turning between previewer windows and the
source too often? This Elisp/LaTeX package will render your displayed
LaTeX equations right into the editing window where they belong. 

%package common
Summary: 	Emacs/LaTeX inline preview (LaTeX style and docs)
Group: 		Applications/Editors

%description common
Does your neck hurt from turning between previewer windows and the
source too often? This Elisp/LaTeX package will render your displayed
LaTeX equations right into the editing window where they belong. 

This package contains the LaTeX style files and the documentation.

%package emacs
Summary:	Emacs/LaTeX inline preview (GNU Emacs lisp files)
Group: 		Applications/Editors
Requires:	%{name}-common = %{version}-%{release}
Requires:	emacs >= 21.1
Obsoletes:	preview-latex

%description emacs
Does your neck hurt from turning between previewer windows and the
source too often? This Elisp/LaTeX package will render your displayed
LaTeX equations right into the editing window where they belong. 

This package contains the lisp modules for GNU Emacs 21.1 or higher.

%package xemacs
Summary:	Emacs/LaTeX inline preview (XEmacs lisp files)
Group: 		Applications/Editors
Requires:	%{name}-common = %{version}-%{release}
Requires:	xemacs >= 21.4

%description xemacs
Does your neck hurt from turning between previewer windows and the
source too often? This Elisp/LaTeX package will render your displayed
LaTeX equations right into the editing window where they belong. 

This package contains the lisp modules for XEmacs 21.4 or higher.  

%prep
%setup -c -q

%if %{HAVE_EMACS}
  mkdir emacs
  pushd emacs
  ln -sf ../%{name}-%{version}/* .
  popd
%endif

%if %{HAVE_XEMACS}
  mkdir xemacs
  pushd xemacs
  ln -sf ../%{name}-%{version}/* .
  popd
%endif

%build
for i in *emacs; do
  pushd $i
  # The below will make the package build from a tar straight from CVS
  # NOT RECOMMENDED, but useful for testing!
  test -f ./configure || ./autogen.sh
  %configure --with-$i
  make
  popd
done

%install 
rm -rf %{buildroot}
for i in *emacs; do
  pushd $i
  %makeinstall texmfdir=%{buildroot}%{_datadir}/texmf 
  if [ $i == "emacs" ]; then 
    mkdir -p %{buildroot}%{_datadir}/emacs/site-lisp/site-start.d
    install -c -m 644 preview-latex.el \
      %{buildroot}%{_datadir}/emacs/site-lisp/site-start.d
  fi
  popd
done


%clean
rm -rf %{buildroot}

%post
install-info --info-dir=%{_infodir} %{_infodir}/preview-latex.info
texhash /usr/share/texmf

%preun
install-info --info-dir=%{_infodir} --delete \
  %{_infodir}/preview-latex.info 

%files common
%defattr(-,root,root)
%dir %{_datadir}/texmf/tex/latex/preview
%{_datadir}/texmf/tex/latex/preview/*.sty
%{_datadir}/texmf/tex/latex/preview/*.def
%config %{_datadir}/texmf/tex/latex/preview/*.cfg
%doc %{_datadir}/texmf/doc/latex/styles/preview.dvi
%doc %{_infodir}/preview-latex.info.gz
%doc %{name}-%{version}/ChangeLog
%doc %{name}-%{version}/circ.tex
%doc %{name}-%{version}/COPYING
%doc %{name}-%{version}/INSTALL
%doc %{name}-%{version}/PROBLEMS
%doc %{name}-%{version}/README
%doc %{name}-%{version}/latex/README-preview
%doc %{name}-%{version}/RELEASE
%doc %{name}-%{version}/TODO
%doc %{name}-%{version}/doc/preview-latex.dvi
%doc %{name}-%{version}/patches

%if %{HAVE_EMACS}
%files emacs
%defattr(-,root,root)
%{_datadir}/emacs/site-lisp/preview
%{_datadir}/emacs/site-lisp/site-start.d/preview-latex.el 
%endif

%if %{HAVE_XEMACS}
%files xemacs
%defattr(-,root,root)
%{_libdir}/xemacs/site-packages/lisp/preview
%{_libdir}/xemacs/site-packages/lisp/site-start.d/preview-latex.el 
%endif

%changelog
* Wed Apr 10 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Triple-rpm simplifications

* Sun Mar 31 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Prepare for 0.7, initial triple rpm attempt

* Sun Mar 10 2002 David Kastrup <David.Kastrup@t-online.de>
- Prepare for 0.6.1

* Tue Feb 19 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Added site-start.d support and prauctex.cfg config file

* Thu Feb 14 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Adjusted for 0.6

* Wed Jan 23 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Initial build.
