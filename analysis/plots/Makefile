### This is the plots subdirectory of the plague project

### Hooks for the editor to set the default target

current: target
-include target.mk

######################################################################

## plot_allsources.pdf: plot_allsources.R
crown.tex plot_allsources.tex: plot_allsources.Rout ;
plot_allsources.Rout: ../fits/epidemics.Rout setcolours.Rout plot_allsources.R

Ignore += timeseries.tex timeseries.pdf
timeseries.pdf: timeseries.tex
timeseries.tex: timeseries.Rout ;
timeseries.Rout: plot.Rout setcolours.Rout ../fits/epidemics.Rout ../fits/comb.Rout timeseries.R

## combdata.pdf: combdata.R
combdata.tex: combdata.Rout ;
combdata.Rout: ../fits/comb.Rout ../fits/permtest.Rout
combdata.Rout: ../fits/epidemics.Rout setcolours.Rout 3axes.Rout combdata.R

probate_dates.tex: probate_dates.Rout ;
probate_dates.Rout: ../fits/epidemics.Rout ../fits/comb.Rout plot.setup.Rout probate_dates.R

## This is not part of the main pipeline because willsr is not public
probate.Rds: probate.Rout ;
probate.Rout: ../fits/epidemics.Rout ../fits/comb.Rout probate.R
../../data/probate.rds:
	$(MAKE) probate.Rds
	$(CP) probate.Rds $@
willsr:
	git clone https://github.com/davidearn/willsr.git
willsr.install:
	$(MAKE) willsr
	R CMD INSTALL willsr


## plot3sources.pdf: plot3sources.R
plot3sources.tex: plot3sources.Rout ;
plot3sources.Rout: setcolours.Rout plot3sources.R
	$(run-R)

## Panel blowups (show details for big epidemics)
## plot3sourcesmajors.pdf: 
plot3sourcesmajors.tex: plot3sourcesmajors.Rout ;
plot3sourcesmajors.Rout: setcolours.Rout plot3sourcesmajors.R
	$(run-R)

# seasonalgrowth.pdf: seasonalgrowth.R
seasonalgrowth.tex: seasonalgrowth.Rout ;
seasonalgrowth.Rout: ../fits/nlminb_logit_major.do.Rout.envir ../fits/minors.Rout.envir setcolours.Rout ../fits/epidemics.Rout plot.Rout seasonalgrowth.R
	$(run-R)

# plotallscaled.crop.pdf: plotallscaled.R
plotallscaled.tex: plotallscaled.Rout ;
plotallscaled.Rout: ../fits/data.Rout plot.setup.Rout plotallscaled.R

# plotall.crop.pdf: plotall.R
plotall.tex: plotall.Rout ;
plotall.Rout: ../fits/data.Rout plot.setup.Rout plotall.R

## plotwills14c.pdf: plotwills14c.R
plotwills14c.tex: plotwills14c.Rout ;
plotwills14c.Rout: setcolours.Rout plotwills14c.R

## KvsR.pdf: KvsR.R
KvsR.tex: KvsR.Rout ;
KvsR.Rout: KvsR.R ../fits/epochsum.Rout

## population only (would log scale be better?)
## london_pop.pdf: london_pop.R
london_pop.tex: london_pop.Rout ;
london_pop.Rout: london_pop.R

######################################################################

## Figure cropping
%_crop.pdf: %.pdf
	pdfcrop $< $@

### Analysis subdirectory
Sources += $(wildcard analysis/fits/*.R)
.PRECIOUS: ../fits/%.Rout
../fits/%.Rout: $(wildcard ../fits/*.R)
	$(makestuffthere)

.PRECIOUS: ../fits/%.envir
../fits/%.envir: $(wildcard ../fits/*.R)
	$(makestuffthere)

######################################################################

Makefile: makestuff
makestuff:
	/bin/ln -s ../../makestuff

-include makestuff/os.mk

-include makestuff/git.mk
-include makestuff/visual.mk

-include makestuff/wrapR.mk
-include makestuff/texdeps.mk

