NAME=reaction

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) atom.ml molecule.ml alkane.ml reaction.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
