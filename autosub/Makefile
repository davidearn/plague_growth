
## PNAS style
resources = plague.bib pnas-new.bst pnas-new.cls pnasresearcharticle.sty pnassupportinginfo.sty pnas-logo.png

## main text
figs = timeseries.pdf combdata.pdf plot_allsources.pdf
pix = WillExample1644_page86_1.pdf bill1665adjust.pdf
images = cripplegate_wellcome_parish.pdf 

## supp
figs += london_pop.pdf probate_dates.pdf plotallscaled_crop.pdf plotall_crop.pdf plotwills14c.pdf plot3sourcesmajors.pdf KvsR.pdf seasonalgrowth.pdf

## Cross references
resources += supp_crossrefs.tex ms.aux

## INKSCAPE = inkscape $< --export-eps=$@
## INKSCAPE = inkscape $< --export-type=eps -o $@

Earn%.pdf: Earn%.tex $(figs) $(pix) $(images) $(resources)
	pdflatex Earn$*
	bibtex Earn$*
	pdflatex Earn$*
	pdflatex Earn$*

.PRECIOUS: $(resources)
$(resources): %: ../%
	$(linkexisting)

.PRECIOUS: $(figs)
$(figs): %.pdf: ../analysis/plots/%.pdf
	$(copy)

.PRECIOUS: $(pix)
$(pix): %.pdf: ../images/%.pdf
	$(copy)

.PRECIOUS: $(images)
$(images): %.pdf: ../images/%.pdf
	$(copy)

-include ../makestuff/unix.mk
