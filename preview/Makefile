INSTALLDIR=/usr/share/texmf.local/tex/latex/preview

all: ${INSTALLDIR}/preview.sty ${INSTALLDIR}/prauctex.def preview.drv \
   preview.ins preview.dvi

preview.ins: preview.dtx
	tex '\nonstopmode\def\jobname{ugh.ins}\input docstrip \askforoverwritefalse\generate{\file{preview.ins}{\from{preview.dtx}{installer}}}\endbatchfile'
${INSTALLDIR}/preview.sty ${INSTALLDIR}/auctex.pde preview.drv: preview.dtx preview.ins ${INSTALLDIR}
	TEXMFOUTPUT=${INSTALLDIR} tex preview.ins

${INSTALLDIR}:
	mkdir -p $@

preview.dvi: preview.drv preview.dtx
	latex -interaction nonstopmode preview.drv

