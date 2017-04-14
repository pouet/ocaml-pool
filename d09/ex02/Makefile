NAME=monoid

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) calc.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
