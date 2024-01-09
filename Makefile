##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Makefile
##

NAME = funEvalExpr

SRC = app/Main.hs

all: $(NAME)

$(NAME): $(SRC)
		stack build
		cp $(shell stack path --local-install-root)/bin/$(NAME)-exe ./$(NAME)

clean:
		stack clean

fclean: clean
		rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re