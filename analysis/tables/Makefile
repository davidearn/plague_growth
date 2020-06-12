### This is the tables subdirectory of the plague project

### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

params.tex: params.Rout ;
params.Rout: ../fits/nlminb_major.do.Rout ../fits/epidemics.Rout ../fits/permtest.Rout table.Rout params.R
	$(run-R)

wills_delay_tab.tex: wills_delay_tab.Rout ;
wills_delay_tab.Rout: ../fits/wills_delay.rds wills_delay_tab.R

epochsum.tex: epochsum.Rout ;
epochsum.Rout: ../fits/permtest.Rout epochsum.R
	$(run-R)

minorparams.tex: minorparams.Rout ;
minorparams.Rout: ../fits/minors.Rout ../fits/epidemics.Rout table.Rout minorparams.R
	$(run-R)

suppwindows.tex: suppwindows.Rout ;
suppwindows.Rout: ../fits/nlminb_major.do.Rout suppwindows.R

suppwindowsminor.tex: suppwindowsminor.Rout ;
suppwindowsminor.Rout: ../fits/minors.Rout suppwindowsminor.R

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

