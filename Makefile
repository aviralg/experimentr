R := R
SOURCEDIR := src
INCLUDEDIR := inst/include

SOURCES := $(shell find $(SOURCEDIR) -name '*.cpp')
INCLUDES := $(shell find $(INCLUDEDIR) -name '*.hpp') $(shell find $(SOURCEDIR) -name '*.h')

CPPCHECK := cppcheck
SCAN_BUILD := scan-build

export R_ENABLE_JIT=0
export R_DISABLE_BYTECODE=1

.PHONY: all build check document test

all: clean document build check install

build: document
	$(R) CMD build .

check: build
	$(R) CMD check experimentr*tar.gz

clean:
	-rm -f experimentr*tar.gz
	-rm -fr experimentr.Rcheck
	-rm -rf src/*.o src/*.so

install: clean
	$(R) CMD INSTALL .

uninstall:
	$(R) --slave -e "remove.packages('experimentr')"

document: install-devtools
	$(R) --slave -e "devtools::document()"

website: document
	$(R) --slave -e "pkgdown::build_site()"

test: install-devtools
	$(R) --slave -e "devtools::test()"

lintr: install-lintr
	$(R) --slave -e "quit(status = length(print(lintr::lint_package())) != 0)"

install-devtools:
	$(R) --slave -e "if (!require('devtools')) install.packages('devtools')"

install-lintr:
	$(R) --slave -e "if (!require('lintr')) install.packages('lintr')"

clang-tidy: clean
	$(SCAN_BUILD) make build

cppcheck:
	$(CPPCHECK) --version
	@$(CPPCHECK) --force                                                  \
	             --enable=all                                             \
	             --language=c++                                           \
	             --inconclusive                                           \
	             --std=c++11                                              \
	             --std=c++14                                              \
	             --inline-suppr                                           \
	             --error-exitcode=1                                       \
	             --quiet                                                  \
	             --suppress=missingIncludeSystem                          \
	             --suppress=unusedStructMember                            \
	              -I inst/include/                                        \
	             $(SOURCES)                                               \
	             $(INCLUDES)
