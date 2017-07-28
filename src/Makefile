# R noweb files.
SRC_RNW = $(wildcard *.Rnw)
DST_RNW = $(patsubst %.Rnw,%.tex,$(SRC_RNW))

# All noweb files (hand-written and generated).
SRC_TEX = $(wildcard *.tex) $(DST_RNW)
DST_PDF = $(patsubst %.tex,%.pdf,$(SRC_TEX))

# All outputs.
DST_ALL = $(DST_PDF)

# Default action is to show what commands are available.
all : commands

## preview: build documents.
preview : $(DST_ALL)

# Pattern to build pdf files.
%.pdf: %.tex
	latexmk -pdf $@ $<

# Pattern to convert R noweb to tex.
%.tex: %.Rnw
	Rscript -e "knitr::knit('$$(basename $<)', output = '$$(basename $@)')"

## commands: display available commands.
commands : Makefile
	@sed -n 's/^##//p' $<

## settings: show variables and settings.
settings :
	@echo 'SRC_RNW:' $(SRC_RNW)
	@echo 'DST_RNW:' $(DST_RNW)
	@echo 'SRC_TEX:' $(SRC_TEX)
	@echo 'DST_PDF:' $(DST_PDF)

## clean: clean up temporary and intermediate files.
clean :
	@rm -rf $$(find . -name '*~' -print) \
	*.log *.aux *.out *.blg *.bbl *.nav *.snm *.toc *.lof *.brf *.lot

# deepclean: remove generated pdf.
deepclean :
	@rm -f $(DST_PDF)

## clearcache: remove cache directory
clearcache : 
	@rm -rf cache
