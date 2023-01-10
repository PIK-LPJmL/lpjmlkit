.PHONY: help build check test lint format
.DEFAULT_GOAL = help

# extracts the help text and formats it nicely
HELP_PARSING = 'm <- readLines("Makefile");\
				m <- grep("\#\#", m, value=TRUE);\
				command <- sub("^([^ ]*) *\#\#(.*)", "\\1", m);\
				help <- sub("^([^ ]*) *\#\#(.*)", "\\2", m);\
				cat(sprintf("%-8s%s", command, help), sep="\n")'

help:           ## Show this help.
	@Rscript -e $(HELP_PARSING)

build:          ## Build the package using lucode2::buildLibrary()
	Rscript -e 'lucode2::buildLibrary()'

check:          ## Build documentation and vignettes, run testthat tests,
                ## and check if code etiquette is followed using lucode2::check()
	Rscript -e 'lucode2::check()'

test:           ## Run testthat tests
	Rscript -e 'devtools::test(show_report = TRUE)'

lint:           ## Check if code etiquette is followed using lucode2::lint()
	Rscript -e 'lucode2::lint()'

format:         ## Apply auto-formatting to changed files and lint afterwards
	Rscript -e 'lucode2::autoFormat()'
