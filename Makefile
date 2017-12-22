all: $(addprefix bin/,$(basename $(wildcard *.hs)))

bin/%: %.hs
	@ghc --make $< -O3 -Wall -Wextra -Werror -o $@

test-%: %.hs
	@doctest --preserve-it $<

run-%: bin/%
	@$<

.PHONY: all test-%
