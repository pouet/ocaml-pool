NAME=app

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) app.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
