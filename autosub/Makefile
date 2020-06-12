figs = plot_all.eps timeseries.eps combdata.eps 
pix = WillExample1644_page86_1.eps bill1665adjust.eps
images = cripplegate_wellcome_parish.eps 

bibs = plague.bib pnas-new.bst

Earn_etal_MS.pdf: Earn_etal_MS.tex $(figs) $(pix) $(images) $(bibs)
	texi2dvi --pdf $<

.PRECIOUS: $(bibs)
$(bibs): %: ../%
	$(linkexisting)

$(figs): %.eps: ../analysis/plots/%.pdf

$(pix): %.eps: ../images/%.pdf

$(images): %.eps: ../images/%.jpg

%.eps:
	inkscape $< --export-eps=$@

-include ../makestuff/unix.mk
