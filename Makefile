SOURCES = intro.hs
TARGETS = intro

HC = ghc

.PHONY: all clean mrproper

all: $(TARGETS)

intro: intro.o
	$(HC) $< -o $@

%.o: %.hs
	$(HC) -c $< -o $@

clean:
	@rm -f *.hi *.o

mrproper: clean
	@rm -f $(TARGETS)
