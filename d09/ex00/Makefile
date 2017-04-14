NAME=hour

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) watchtower.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
