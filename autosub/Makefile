figs = timeseries.eps combdata.eps plot_allsources.eps
pix = WillExample1644_page86_1.eps bill1665adjust.eps
images = cripplegate_wellcome_parish.eps 

bibs = plague.bib pnas-new.bst

## INKSCAPE = inkscape $< --export-eps=$@
INKSCAPE = inkscape $< --export-type=eps -o $@

Earn_etal_MS.pdf: %.pdf: Earn_etal_MS.tex $(figs) $(pix) $(images) $(bibs)
	latex $*
	bibtex $*
	latex $*
	latex $*
	dvipdf $*.dvi $@

.PRECIOUS: $(bibs)
$(bibs): %: ../%
	$(linkexisting)

.PRECIOUS: $(figs)
$(figs): %.eps: ../analysis/plots/%.pdf
	$(INKSCAPE)

.PRECIOUS: $(pix)
$(pix): %.eps: ../images/%.pdf
	$(INKSCAPE)

.PRECIOUS: $(images)
$(images): %.eps: ../images/%.jpg
	gm convert $< $@

-include ../makestuff/unix.mk
