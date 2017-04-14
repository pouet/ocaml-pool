NAME=try

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) try.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
