Summary: 	Emacs/LaTeX inline preview 
Name: 		preview-latex
Version: 	0.6
Release: 	0
URL: 		http://preview-latex.sourceforge.org
Source0: 	http://prdownloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz
License: 	GPL
Group: 		Applications/Editors
BuildRoot: 	%{_tmppath}/%{name}-root
Prereq:		/sbin/install-info
Requires:	emacs >= 21.1
Requires:	auctex >= 10.0g
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
# NOT RECOMMENDED!
test -a README || ./autogen.sh 
rm -r patches/CVS # Simplifies the files section

%configure
make
# The below will make the package build without Alan's autoconf fix
test -a doc/preview-latex.info || (cd doc; make preview-latex.info)

%install
rm -rf $RPM_BUILD_ROOT
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
# The below will make the package build without Alan's autoconf fix
test -a $RPM_BUILD_ROOT/%{_infodir}/preview-latex.info || \
	( mkdir -p $RPM_BUILD_ROOT/%{_infodir}             
	/usr/bin/install -c -m 644 doc/preview-latex.info \
		$RPM_BUILD_ROOT/%{_infodir}/preview-latex.info )

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/install-info %{_infodir}/preview-latex.info %{_infodir}/dir
texhash /usr/share/texmf

%preun
/sbin/install-info --delete %{_infodir}/preview-latex.info %{_infodir}/dir

%files
%defattr(-,root,root)
/usr/share/texmf/tex/latex/preview
/usr/share/texmf/doc/latex/styles/preview.dvi
/usr/share/emacs/site-lisp/preview
%{_infodir}/preview-latex.info.gz
%doc ChangeLog circ.tex COPYING INSTALL PROBLEMS README
%doc README-preview RELEASE TODO doc/preview-latex.dvi
%doc patches

%changelog
* Thu Feb 14 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Adjusted for 0.6

* Wed Jan 23 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Initial build.


