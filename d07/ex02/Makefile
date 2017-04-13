NAME=dalek

all: $(NAME)

$(NAME):
	ocamlopt -o $(NAME) people.ml doctor.ml dalek.ml main.ml

clean:
	$(RM) *.cmi *.cmx *.o

fclean: clean
	$(RM) $(NAME)

re: fclean all
