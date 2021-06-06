NAME = cp2021t
tex:  $(NAME).lhs
	lhs2TeX $^ > $(NAME).tex
	pdflatex $(NAME).tex
	bibtex $(NAME).aux
	makeindex $(NAME).idx
