Summary: 	Emacs/LaTeX inline Preview 
Name: 		preview-latex
Version: 	0.5.8.2
Release: 	0
URL: 		http://preview-latex.sourceforge.org
Source0: 	http://prdownloads.sourceforge.net/%{name}/%{name}-%{version}.tar.gz
License: 	GPL
Group: 		Applications/Editors
BuildRoot: 	%{_tmppath}/%{name}-root
#Prereq:		/sbin/install-info
Requires:	emacs >= 21.1
Requires:	auctex >= 10.0g
Requires:	ghostscript >= 6.51
Requires:	tetex tetex-dvips
#BuildRequires:	texinfo

%description 
Your neck hurts from turning between previewer windows and the source
too often? This Elisp/LaTeX package will render your displayed LaTeX
equations right into the editing window where they belong. Needs at
least Emacs-21.1, XEmacs porters welcome.

%prep
%setup -q

%build
%configure
make
# Docs not included in this package. 
#pushd doc
#make
#popd

%install
rm -rf $RPM_BUILD_ROOT
# To buildroot special paths not contained in makeinstall  
set_here () { export $1=$3; }
set_here `grep ^texmfdir Makefile`
set_here `grep ^previewtexmfdir Makefile`
set_here `grep ^previewdocdir Makefile`
set_here `grep ^AUCTEXDIR Makefile`
%makeinstall texmfdir=$RPM_BUILD_ROOT/$texmfdir \
	previewtexmfdir=$RPM_BUILD_ROOT/$previewtexmfdir \
	previewdocdir=$RPM_BUILD_ROOT/$previewdocdir \
	AUCTEXDIR=$RPM_BUILD_ROOT/$AUCTEXDIR 


%clean
rm -rf $RPM_BUILD_ROOT

%post
#/sbin/install-info %{_infodir}/preview-latex 
texhash /usr/share/texmf

%preun
#/sbin/install-info --delete %{_infodir}/preview-latex

%files
%defattr(-,root,root)
/usr/share/texmf/tex/latex/preview
/usr/share/texmf/doc/latex/styles/preview.dvi
/usr/share/emacs/site-lisp/preview
%doc ChangeLog circ.tex COPYING INSTALL PROBLEMS README
%doc README-preview RELEASE TODO

%changelog
* Wed Jan 23 2002 Jan-Ake Larsson <jalar@imf.au.dk>
- Initial build.


