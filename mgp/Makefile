default: ms.pdf ms.R reply1.pdf

.INTERMEDIATE: ms.tex

ms.pdf reply1.pdf: header.tex defs.tex

diff.tex: TPB1.tex ms.tex diff.ed
	latexdiff TPB1.tex ms.tex > diff.tex
	(cat diff.ed && echo w) | ed - diff.tex

include rules.mk

clean: .clean

fresh: .fresh
	$(RM) ms.tex
