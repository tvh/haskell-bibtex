.PHONY:	ghci pubs hackbib remotehackbib

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
	./dist/build/publication-overview/publication-overview < $< >$@
#	ghc -e main src/Publications.hs < $< >$@

%.bbl:	%.aux tex/publications.bib
	(cd $(dir $<); bibtex $(notdir $*))

tex/%.aux: tex/publications.tex
	(cd $(dir $<); pdflatex $(notdir $<))


hackbib:	hackage.bib

hackage.bib:	$(HOME)/.cabal/packages/hackage.haskell.org/00-index.tar.gz src/Hackage.hs
	gunzip --stdout $< | ./dist/build/hackage-bibtex/hackage-bibtex >$@
#	gunzip --stdout $< | ghc -e main src/Hackage.hs >$@

remotehackbib:
	wget -O - http://hackage.haskell.org/packages/archive/00-index.tar.gz \
	   | gunzip \
	   | ./dist/build/hackage-bibtex/hackage-bibtex \
	   | ssh code.haskell.org tee /home/thielema/public_html/bibtex/hackage.bib \
	   > /dev/null
