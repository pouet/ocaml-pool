NAME=set

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) set.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
