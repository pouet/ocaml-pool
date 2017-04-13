NAME=galifrey

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) people.ml doctor.ml dalek.ml army.ml galifrey.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
