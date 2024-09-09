GHC ?= ghc
RM ?= rm
SRC := BabyGlados.hs
BIN := $(basename $(SRC))

all: $(BIN)

$(BIN):
	$(GHC) $(SRC)

clean:
	$(RM) $(addsuffix .hi, $(BIN))
	$(RM) $(addsuffix .o, $(BIN))

fclean: clean
	$(RM) $(BIN)

re: fclean all

.PHONY: fclean re clean all
