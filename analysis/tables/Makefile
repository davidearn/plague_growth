### This is the tables subdirectory of the plague project

### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

params.tex: params.Rout ;
params.Rout: ../fits/nlminb_major.do.Rout ../fits/epidemics.Rout ../fits/permtest.Rout table.Rout params.R
	$(run-R)

### fits subdirectory
../fits/%.Rout ../fits/%.rds: $(wildcard ../fits/*.R)
	$(makethere)

######################################################################

## Crib

.PRECIOUS: %.R
%.R:
	/bin/cp ../../../plague/analysis/tables/$@ .

##################################################################

Makefile: makestuff
makestuff:
	/bin/ln -s ../../makestuff

-include makestuff/os.mk

-include makestuff/git.mk
-include makestuff/visual.mk

-include makestuff/wrapR.mk
-include makestuff/texdeps.mk

