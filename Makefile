.PHONY: all build check install clean docs test lint style stats

R = /Library/Frameworks/R.framework/Resources/bin/R
Rscript = /Library/Frameworks/R.framework/Resources/bin/Rscript
PACKAGE_STAR = ../packages/qboost_*.tar.gz

all: build install clean

build: clean
	@${R} CMD BUILD .
	@if [ ! -e $$(dirname ${PACKAGE_STAR}) ]; then \
		mkdir -v $$(dirname ${PACKAGE_STAR}); \
	fi
	@mv $$(basename ${PACKAGE_STAR}) $$(dirname ${PACKAGE_STAR})

check:
	@${R} CMD check .

install:
	@Rscript -e 'print(.libPaths())'
	@${R} CMD INSTALL \
		--library=$$(Rscript -e 'cat(.libPaths()[1])') \
		${PACKAGE_STAR}

clean:
	@if [ -e ${PACKAGE_STAR} ]; then \
		rm -v ${PACKAGE_STAR}; \
	fi

uninstall:
	@${Rscript} -e 'remove.packages("qboost")'

docs:
	@${Rscript} -e 'devtools::document()'

tests: test

test:
	@${Rscript} -e "devtools::test()"

lint:
	@${R} --quiet --vanilla --slave -e "\
		if (!requireNamespace('lintr', quietly = TRUE)) \
			install.packages('lintr'); \
		library(lintr); \
		lint_package(path = '.', linters = NULL)"

style:
	@${R} --quiet --vanilla --slave -e "\
		if (!requireNamespace('styler', quietly = TRUE)) \
			install.packages('styler'); \
		library(styler); \
		style_pkg()"

stats:
	@echo "Current lines: "
	@dirs=$$(for d in R dev tests rd; do [ -d "$$d" ] && echo "$$d"; done); \
	if [ -n "$$dirs" ]; then \
		find $$dirs -name '*.R' -exec cat {} + | wc -l; \
	else \
		echo "0"; \
	fi

	@changes_so_far=$$(git log --format=%H | \
		xargs -I {} \
		git show --format= --numstat {} | \
		awk '{add+=$$1; subs+=$$2} END {print add+subs}') && \
	data_lines=0 && \
	for d in data data-raw; do \
		if [ -d "$$d" ] && [ -n "$$(ls -A $$d 2>/dev/null)" ]; then \
			data_lines=$$(($$data_lines + $$(cat $$d/* | wc -l | sed 's/^ *//'))); \
		fi; \
	done && \
	total=$$(($$changes_so_far - $$data_lines)) && \
	echo "\nChanges (without data directories): " && \
	echo "   $$total"
