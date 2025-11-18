.PHONY: all build check install clean docs test lint style stats

R = /Library/Frameworks/R.framework/Resources/bin/R
Rscript = /Library/Frameworks/R.framework/Resources/bin/Rscript
PACKAGE_NAME = qboost
PACKAGE_TARBALL = ${PACKAGE_NAME}_*.tar.gz
PACKAGE_DIR = builds
PACKAGE_STAR = ${PACKAGE_DIR}/${PACKAGE_TARBALL}
PACKAGE_LIB = ${PACKAGE_DIR}/library

all: build install clean

build: clean
	@${R} CMD build .
	@pkg=$$(ls ${PACKAGE_TARBALL} | tail -n 1) && \
		dir=${PACKAGE_DIR} && \
		mkdir -p $$dir && \
		mv $$pkg $$dir/

check:
	@${R} CMD check .

install: build
	@Rscript -e 'print(.libPaths())'
	@mkdir -p ${PACKAGE_LIB}
	${R} CMD INSTALL \
		--library=${PACKAGE_LIB} \
		$$(ls ${PACKAGE_STAR})

clean:
	@rm -f ${PACKAGE_TARBALL} ${PACKAGE_STAR}

uninstall:
	@${Rscript} -e "tryCatch(remove.packages(\"${PACKAGE_NAME}\"), error = function(e) message('Package not installed'))"

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
		lint_dir('.')"

style:
	@${R} --quiet --vanilla --slave -e "\
		if (!requireNamespace('lintr', quietly = TRUE)) \
			install.packages('lintr'); \
		library(styler); \
		style_dir('.')"

stats:
	@echo "Current lines: "
	@find R dev tests -name '*.R' -exec cat {} + | wc -l

	@changes_so_far=$$(git log --format=%H | \
		xargs -I {} \
		git show --format= --numstat {} | \
		awk '{add+=$$1; subs+=$$2} END {print add+subs}') && \
	data_lines=$$(cat data{,-raw}/* | wc -l | sed 's/^ *//') && \
	total=$$(($$changes_so_far - $$data_lines)) && \
	echo "\nChanges (without data directories): " && \
	echo "   $$total"
