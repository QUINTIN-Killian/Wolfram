##
## EPITECH PROJECT, 2025
## Wolfram
## File description:
## Makefile
##

CC	=	ghc

SRC	=	app/Main.hs	\
		src/Lib.hs	\

OBJ	=	$(SRC:src/*.hs=bin/*.o)

EXEC	=	wolfram

all:	$(EXEC)

$(EXEC):	$(OBJ)
	stack build
	$(CC) -o $(EXEC) $(OBJ)
	@mkdir -p bin
	@mv src/*.o bin
	@mv app/*.o bin
	@mv src/*.hi bin
	@mv app/*.hi bin

clean:
	rm -rf bin

fclean:	clean
	rm -f $(EXEC)

re:	fclean all

tests_run:
	stack build && stack test

.PHONY:	all clean fclean re tests_run
