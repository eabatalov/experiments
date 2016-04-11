all: bin/test bin/test2 bin/misc bin/bplustree

CPP_FLAGS=-Wall -Werror -ggdb -std=c++11

bin/test: src/test.cpp
	g++ $(CPP_FLAGS) $< -o $@

bin/test2: src/test2.cpp
	g++ $(CPP_FLAGS) $< -o $@

bin/misc: src/misc.cpp
	g++ $(CPP_FLAGS) $< -o $@

bin/bplustree: src/bplustree.cpp
	g++ $(CPP_FLAGS) $< -o $@

.PHONY: clean all

clean:
	rm -f bin/*
