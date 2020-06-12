### This is the fits subdirectory of the plague project

### Hooks for the editor to set the default target

current: target
-include target.mk

##################################################################

Sources += $(wildcard *.R)

## predicted values and summary statistics (r, R0, etc. in both epochs)
epochsum.Rout: permtest.Rout epochsum.R
	$(run-R)

## mixed-model fit and permutation test of epoch summaries and between-epoch differences
permtest.Rout: nlminb_logit_major.do.Rout simfuns.R epidemics.R permtest.R
	$(run-R)

all_majors = nlminbrep_major.do.Rout.envir nlminb_major.do.Rout.envir NM_major.do.Rout.envir combo_major.do.Rout.envir nlminb_logit_major.do.Rout.envir combo_logit_major.do.Rout.envir NM_logit_major.do.Rout.envir
## nmk_major.do.Rout.envir nmk_logit_major.do.Rout.envir nmk_twostage_major.do.Rout.envir

## 'iwanthue' attempts to set a dichromat-friendly palette (still not great)
compare_majors.Rout:  $(all_majors) iwanthue.Rout epidemics.Rout compare_majors.R
	$(run-R)

## fit models to major epidemics with different optimizers
## ls set_*.R
.PRECIOUS: %.do.Rout
%.do.Rout: set_%.Rout epidemics.Rout doFits.R
	$(run-R)

## Combine predictions, estimates, CIs, etc. for use in plots (see ../plots/Makefile)
comb.Rout: epidemics.Rout nlminb_major.do.Rout comb.R

######################################################################

data.Rout: epidemics.R data.R
	$(run-R)

## fit minor epidemics (supp only)
minors.Rout: set_minors.Rout epidemics.R doFits.R
	$(run-R)

Ignore += wills_delay.rds
wills_delay.rds: wills_delay.Rout ;
wills_delay.Rout: comb.Rout wills_delay.R

wills_delay_defs.Rout: wills_delay.rds wills_delay_defs.R
	$(run-R)

######################################################################

## Settings

Sources += window_defs.csv

######################################################################

## Crib

.PRECIOUS: %.R
%.R:
	/bin/cp ../../../plague/analysis/fits/$@ .

######################################################################

Makefile: makestuff

makestuff:
	/bin/ln -s ../../makestuff

-include makestuff/os.mk

-include makestuff/git.mk
-include makestuff/visual.mk

-include makestuff/wrapR.mk
-include makestuff/texdeps.mk

