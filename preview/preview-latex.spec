Summary: 	Emacs/LaTeX inline preview 
Name: 		preview-latex
Version: 	0.6
Release: 	1
BuildArchitectures: noarch
URL: 		http://preview-latex.sourceforge.org
Source0: 	http://prdownloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz
License: 	GPL
Group: 		Applications/Editors
BuildRoot: 	%{_tmppath}/%{name}-root
Prereq:		info
Requires:	emacs >= 21.1
Requires:	auctex
Requires:	ghostscript >= 6.51
Requires:	tetex tetex-dvips
BuildRequires:	texinfo

%description 
Does your neck hurt from turning between previewer windows and the
source too often? This Elisp/LaTeX package will render your displayed
LaTeX equations right into the editing window where they belong. Needs
at least Emacs-21.1, XEmacs porters welcome.

%prep
%setup -q

%build
# The below will make the package build from a tar straight from CVS
# NOT RECOMMENDED, but useful for testing!
#./autogen.sh; rm -r patches/CVS # Simplifies the files section

%configure
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_infodir}
# To buildroot special paths not contained in makeinstall  
set_here () { export $1=$RPM_BUILD_ROOT/$3; }
set_here `grep ^texmfdir Makefile`
set_here `grep ^previewtexmfdir Makefile`
set_here `grep ^previewdocdir Makefile`
set_here `grep ^AUCTEXDIR Makefile`
%makeinstall texmfdir=$texmfdir \
	previewtexmfdir=$previewtexmfdir \
	previewdocdir=$previewdocdir \
	AUCTEXDIR=$AUCTEXDIR 
install -d $RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp/site-start.d
install -c -m 644 preview-latex.el \
  $RPM_BUILD_ROOT%{_datadir}/emacs/site-lisp/site-start.d

%clean
rm -rf $RPM_BUILD_ROOT

%post
install-info --info-dir=%{_infodir} %{_infodir}/preview-latex.info
texhash /usr/share/texmf

%preun
install-info --info-dir=%{_infodir} --delete \
	%{_infodir}/preview-latex.info 

%files
%defattr(-,root,root)
%dir /usr/share/texmf/tex/latex/preview
%config /usr/share/texmf/tex/latex/preview/prauctex.cfg
/usr/share/texmf/tex/latex/preview/prauctex.def
/usr/share/texmf/tex/latex/preview/preview.sty
/usr/share/texmf/doc/latex/styles/preview.dvi
/usr/share/emacs/site-lisp/preview
%{_datadir}/emacs/site-lisp/site-start.d/preview-latex.el 
%{_infodir}/preview-latex.info.gz
%doc ChangeLog circ.tex COPYING INSTALL PROBLEMS README
%doc README-preview RELEASE TODO doc/preview-latex.dvi
%doc patches

%changelog
* Tue Feb 19 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Added site-start.d support and prauctex.cfg config file

* Thu Feb 14 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Adjusted for 0.6

* Wed Jan 23 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Initial build.


