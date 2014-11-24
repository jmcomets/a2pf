SOURCES = intro.hs countdown.hs
TARGETS = intro countdown

HC = ghc

.PHONY: all clean mrproper

all: $(TARGETS)

intro: intro.o
	$(HC) $< -o $@

countdown: countdown.o
	$(HC) $< -o $@

%.o: %.hs
	$(HC) -c $< -o $@

clean:
	@rm -f *.hi *.o

mrproper: clean
	@rm -f $(TARGETS)
