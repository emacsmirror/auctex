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
Requires:	emacs >= 21.1
Requires:	auctex >= 11.0
Requires:	ghostscript >= 6.51
Requires:	tetex tetex-dvips
BuildRequires:	texinfo >= 4.0

%description 
Does your neck hurt from turning between previewer windows and the
source too often? This Elisp/LaTeX package will render your displayed
LaTeX equations right into the editing window where they belong. This
version is for GNU Emacs 21.

%prep
%setup -q

%build
# The below will make the package build from a tar straight from CVS
# NOT RECOMMENDED, but useful for testing!
# Actually, when building, you should get your stuff via
# cvs export
# and you won't get those pesky CVS directories.
#./autogen.sh; rm -r patches/CVS # Simplifies the files section

%configure --with-texmf-dir=%{_datadir}/texmf
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_infodir}
# To buildroot special paths not contained in makeinstall  
set_here () { export $1=${RPM_BUILD_ROOT}$3; }
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
%dir %{_datadir}/texmf/tex/latex/preview
%config %{_datadir}/texmf/tex/latex/preview/prauctex.cfg
%{_datadir}/texmf/tex/latex/preview/prauctex.def
%{_datadir}/texmf/tex/latex/preview/preview.sty
%{_datadir}/texmf/doc/latex/styles/preview.dvi
%{_datadir}/emacs/site-lisp/preview
%{_datadir}/emacs/site-lisp/site-start.d/preview-latex.el 
%{_infodir}/preview-latex.info.gz
%doc ChangeLog circ.tex COPYING INSTALL PROBLEMS README
%doc README-preview RELEASE TODO doc/preview-latex.dvi
%doc patches

%changelog
* Sun Mar 10 2002 David Kastrup <David.Kastrup@t-online.de>
- Prepare for 0.6.1

* Tue Feb 19 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Added site-start.d support and prauctex.cfg config file

* Thu Feb 14 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Adjusted for 0.6

* Wed Jan 23 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Initial build.
