.PHONY : all

all : pme_dists.pdf scales_diagram.pdf

include ../rules.mk

pme_dists.pdf : make_figs.R
	Rscript $<
