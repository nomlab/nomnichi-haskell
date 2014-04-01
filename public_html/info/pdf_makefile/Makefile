###
### Makefile for LaTeX documents
###
### Yoshinari Nomura
###
### GNU make 3.81 will work for you.
###

#####################################################################
# Typeset commands and path settings
#####################################################################

#vpath graphics

#PLATEX    = platex-sjis -interaction=nonstopmode
PLATEX     = platex -interaction=nonstopmode
BIBTEX     = jbibtex

DVIPDFMX   = dvipdfmx -p a4     # for A4
#DVIPDFMX  = dvipdfmx -p a4  -l # for A4 Landscape

GS         = gs -q -dSAFER -dBATCH -dNOPAUSE
EBB        = ebb

TEXINPUTS := sty:$(TEXINPUTS)

#####################################################################
# Helper Macros
#####################################################################

# Extract trailed page number from FILE-NAME:
#  $(call pagenum,fig1.pdf) -> 1
pagenum	= $(shell echo $(basename $1) | sed 's/^[^0-9]*//')

# Add PAGE-NUMBER to FILE-NAME:
#   $(call addpage,fig.pdf,1) -> fig1.pdf
# if SUFFIX is set as the third param, it replaces the original suffix:
#   $(call addpage,fig.pdf,1,bb) -> fig1.bb
#
addpage = $(basename $(1))$(2)$(if $(3),.$(3),$(suffix $(1)))

#####################################################################
# Suffix rules
#####################################################################

.SUFFIXES:
.SUFFIXES: .pdf .dvi .tex .eps .obj .png .gif .gnuplot

.dvi.pdf:
	${DVIPDFMX} $<

.tex.dvi:
	${PLATEX} $< 2>&1 > /dev/null
#	${BIBTEX} $*
	${PLATEX} $<

.pdf.bb:
	@echo "Making $@ from $<"
	@${GS} -sDEVICE=bbox -r100 $< 2> $@

#####################################################################
# Files in the project
#####################################################################

# if the main TeX file is ``myproject.tex'', set:
#   PROJECT = myproject
PROJECT		= myproject

# All figure files are extracted from the single PDF file: FIG_MASTER_FILE.
# FIG_PAGES indicates which pages are extracted and used in the main TeX file.
#
# For example:
#   FIG_MASTER_FILE = graphics/fig.pdf
#   FIG_PAGES     = 1 2 3 4 5
# makes:
#   graphics/fig1.pdf ... graphics/fig5.pdf from graphics/fig.pdf
#
FIG_MASTER_FILE = graphics/fig.pdf
FIG_PAGES       = 1 2 3 4 5

TEX_FILES      = ${PROJECT}.tex
FIG_PDF_FILES := $(foreach n, $(FIG_PAGES), $(call addpage,$(FIG_MASTER_FILE),$(n)))
FIG_EBB_FILES := $(foreach n, $(FIG_PAGES), $(call addpage,$(FIG_MASTER_FILE),$(n),bb))

#####################################################################
# Common rules
#####################################################################

preview: ${PROJECT}.pdf
	open $<

all:	${PROJECT}.pdf

${FIG_PDF_FILES} : ${FIG_MASTER_FILE}
	@echo 'Making $@ from $< page: $(call pagenum,$*)'
	@${GS}  -sDEVICE=pdfwrite \
		-dFirstPage=$(call pagenum,$*)  \
		-dLastPage=$(call pagenum,$*)   \
		-sOutputFile=$@ ${FIG_MASTER_FILE} 2>/dev/null

${FIG_EBB_FILES} : %.bb : %.pdf
	@echo "Making $@ from $<"
	@${GS} -sDEVICE=bbox -r100 $< 2> $@

${PROJECT}.dvi: ${TEX_FILES} ${FIG_PDF_FILES} ${FIG_EBB_FILES}

${PROJECT}.pdf: ${PROJECT}.dvi

distclean: clean figclean pdfclean

clean:
	rm -f *.log *.aux *~ *.toc *.idx *.ilg *.ind *.dvi
figclean:
	rm -f ${FIG_PDF_FILES} ${FIG_EBB_FILES} 
pdfclean:
	rm -f ${PROJECT}.pdf

