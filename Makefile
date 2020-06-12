## This is plague_growth

current: target
-include target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

Sources += README.md
Ignore += .gitignore

-include makestuff/perl.def

Ignore += *tikzDictionary

######################################################################

## Check setup and catch missing packages 
Sources += library.R texstuff.tex

######################################################################

## Documents

Sources += ms.tex nlipreamble.tex plague.bib ms_text.tex

## make from scratch; use dotdir for testing
Sources += dottarget.mk
Ignore += fullpaper.pdf
checkpaper: paper fullpaper.pdf
fullpaper.pdf: ms.pdf supp.pdf
	$(pdfcat) || (echo "STOPPING\nSTOPPING: $^ made but couldn't merge\nSTOPPING" && false)
	$(MAKE) $@.go || (echo "STOPPING\nSTOPPING: $@ merged but couldn't display\nSTOPPING" && false)

## paper: supp.Rnw ms.tex
paper: texstuff.alltex library.Rout supp.alltex ms.alltex
	@echo
	@echo Done?
	$(MAKE) ms.pdf
	$(MAKE) supp.pdf

######################################################################

## Crib

$(Sources):
	cp ../plague/$@ .

######################################################################

### Makestuff
msrepo = https://github.com/dushoff

Sources += Makefile
Ignore += makestuff

Makefile: makestuff/Makefile
makestuff/Makefile:
clonestuff:
	git clone $(msrepo)/makestuff
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/wrapR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
