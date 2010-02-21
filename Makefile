.PHONY:	ghci pubs

# problem: bibtex refuses to generate empty bbl files
# thus you must have at least one entry per publication type
TYPES := conference journal popular program \
	reviewedconference reviewedjournal techreport thesis

ghci:
	ghci -Wall -i:src src/Publications.hs

pubs:	tex/publications.pdf

%.pdf:	%.tex %-cite.tex $(patsubst %, tex/%.bbl, $(TYPES))
	(cd $(dir $<); pdflatex $(notdir $<); pdflatex $(notdir $<))

%-cite.tex:	%.bib
	ghc -e main src/Publications.hs < $< >$@

%.bbl:	%.aux tex/publications.bib
	(cd $(dir $<); bibtex $(notdir $*))

tex/%.aux: tex/publications.tex
	(cd $(dir $<); pdflatex $(notdir $<))
