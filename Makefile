all: $(addprefix bin/,$(basename $(wildcard *.hs)))

bin/%: %.hs
	@ghc --make $< -O3 -Wall -Wextra -Werror -o $@

test-%: %.hs
	@doctest --preserve-it $<

run-%: bin/%
	@$<

clean:
	rm -f *.hi *.o

.PHONY: all test-% clean
