### This is the plots subdirectory of the plague project

### Hooks for the editor to set the default target

current: target
-include target.mk

######################################################################

## plot_all.pdf: plot_all.R
plot_all.tex: plot_all.Rout ;
plot_all.Rout: ../fits/epidemics.Rout setcolours.Rout plot_all.R

Ignore += timeseries.tex timeseries.pdf
timeseries.pdf: timeseries.tex
timeseries.tex: timeseries.Rout ;
timeseries.Rout: plot.Rout setcolours.Rout ../fits/epidemics.Rout ../fits/comb.Rout timeseries.R

## combdata.pdf: combdata.R
combdata.tex: combdata.Rout ;
combdata.Rout: ../fits/comb.Rout ../fits/permtest.Rout
combdata.Rout: ../fits/epidemics.Rout setcolours.Rout 3axes.Rout combdata.R

### Analysis subdirectory
Sources += $(wildcard analysis/fits/*.R)
.PRECIOUS: ../fits/%.Rout
../fits/%.Rout: $(wildcard ../fits/*.R)
	$(makethere)

######################################################################

## Crib

.PRECIOUS: %.R
%.R:
	/bin/cp ../../../plague/analysis/plots/$@ .

##################################################################

Makefile: makestuff
makestuff:
	/bin/ln -s ../../makestuff

-include makestuff/os.mk

-include makestuff/git.mk
-include makestuff/visual.mk

-include makestuff/wrapR.mk
-include makestuff/texdeps.mk

