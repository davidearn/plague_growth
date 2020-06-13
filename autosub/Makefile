figs = plot_all.eps timeseries.eps combdata.eps 
pix = WillExample1644_page86_1.eps bill1665adjust.eps
images = cripplegate_wellcome_parish.eps 

bibs = plague.bib pnas-new.bst

Earn_etal_MS.pdf: %.pdf: %.tex $(figs) $(pix) $(images) $(bibs)
	latex $*
	bibtex $*
	latex $*
	latex $*

.PRECIOUS: $(bibs)
$(bibs): %: ../%
	$(linkexisting)

$(figs): %.eps: ../analysis/plots/%.pdf
	inkscape $< --export-eps=$@

$(pix): %.eps: ../images/%.pdf
	inkscape $< --export-eps=$@

$(images): %.eps: ../images/%.jpg
	gm convert $< $@

-include ../makestuff/unix.mk
