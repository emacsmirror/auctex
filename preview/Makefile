INSTALLDIR=/usr/share/texmf.local/tex/latex/preview

all: preview.sty prauctex.def preview.drv \
   preview-mk.ins preview.dvi

preview-mk.ins: preview.dtx bootstrap.ins
	tex -interaction nonstopmode bootstrap.ins
preview.sty prauctex.def preview.drv: preview.dtx preview-mk.ins
	tex -interaction nonstopmode preview-mk.ins

install: preview.sty prauctex.def
	-mkdir -p ${INSTALLDIR}
	cp preview.sty prauctex.def ${INSTALLDIR}


preview.dvi: preview.drv preview.dtx
	latex -interaction nonstopmode preview.drv

