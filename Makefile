PACKAGE-NAME=affix
EXCL=example
DEPS-FLAGS=--check-pkg-deps --unused-pkg-deps

all: setup

install:
	raco pkg install --deps search-auto

remove:
	raco pkg remove $(PACKAGE-NAME)

setup:
	raco setup --tidy $(DEPS-FLAGS) --pkgs $(PACKAGE-NAME)

clean:
	raco setup --fast-clean --pkgs $(PACKAGE-NAME)

check-deps:
	raco setup --no-docs $(DEPS-FLAGS) $(PACKAGE-NAME)

test:
	raco test -x -p $(PACKAGE-NAME)
