##
## EPITECH PROJECT, 2025
## Wolfram
## File description:
## Makefile
##

CC	=	ghc

SRC	=	app/Main.hs	\

OBJ	=	$(SRC:*.hs=*.o)

EXEC	=	wolfram

all:	$(EXEC)

$(EXEC):	$(OBJ)
	stack build
	$(CC) -o $(EXEC) $(OBJ)
	@mkdir -p bin
	@mv app/*.o bin
	@mv app/*.hi bin

clean:
	rm -rf bin

fclean:	clean
	rm -f $(EXEC)

re:	fclean all

tests_run:
	stack build && stack test

.PHONY:	all clean fclean re tests_run
