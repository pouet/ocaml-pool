NAME=atom

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) atom.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
