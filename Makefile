## This is plague_growth

current: target
-include target.mk

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

## ms.pdf: ms.tex

######################################################################

## Supp and crossrefs

Sources += supp.Rnw
Ignore += supp.tex
Ignore += $(wildcard *.cpt)

## supp.pdf: supp.Rnw
supp.tex: supp.Rnw analysis/fits/epochsum.Rout

supp.tex: supp.Rnw
	Rscript -e "library(knitr); knit('$<')"

## Crossrefs 2020 Jun 11 (Thu) 

Ignore += supp_crossrefs.tex
supp_crossrefs.tex: supp.tex
	$(MAKE) supp.ltx
	grep newlabel supp.aux > $@

######################################################################

### Analysis subdirectories
analysis/plots/makestuff analysis/fits/makestuff analysis/tables/makestuff:
	$(makethere)

## fits ##
Sources += $(wildcard analysis/fits/*.R)
Sources += analysis/fits/Makefile analysis/fits/window_defs.csv
Ignore += analysis/fits/*.tex analysis/fits/*.pdf
analysis/fits/%.tex analysis/fits/%.Rout: $(wildcard analysis/fits/*.R)
	$(makethere)

## plots ##
Sources += $(wildcard analysis/plots/*.R)
Sources += analysis/plots/Makefile
Ignore += analysis/plots/*.tex analysis/plots/*.pdf
Ignore += analysis/plots/*.png
analysis/plots/%.pdf: $(wildcard analysis/plots/*.R)
	$(makethere)

## tables ##
Sources += $(wildcard analysis/tables/*.R)
Sources += analysis/tables/Makefile
Ignore += analysis/tables/*.tex
analysis/tables/%.tex: $(wildcard analysis/tables/*.R)
	$(makethere)

######################################################################

## Miscellaneous

Sources += pnas-new.bst $(wildcard images/*.*)

Ignore += plague plague.zip
plague: plague.zip

plague.zip:
	wget -O $@ "https://github.com/davidearn/plague/archive/master.zip"

######################################################################

## Crib
$(Sources):
	$(MAKE) plague
	cp plague/$@ .

plague:
	git clone https://github.com/davidearn/plague.git

images/%:
	cp ../plague/$@ $@

######################################################################

### Makestuff
msrepo = https://github.com/dushoff

Sources += Makefile
Ignore += makestuff

Makefile: makestuff/Makefile
makestuff/Makefile:
	git clone $(msrepo)/makestuff
	@echo "*** MESSAGE: makestuff is now set up; you may need to try your make command again"
	ls makestuff/Makefile

-include makestuff/os.mk

-include makestuff/wrapR.mk
-include makestuff/texdeps.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk
