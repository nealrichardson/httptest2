VERSION = $(shell grep ^Version DESCRIPTION | sed s/Version:\ //)

doc:
	R -s -e 'library(roxygen2); roxygenise()'
	-git add --all man/*.Rd

test:
	R CMD INSTALL --install-tests --no-test-load --no-docs --no-help --no-byte-compile .
	export NOT_CRAN=true && R -s -e 'library(testthat); setwd(file.path(.libPaths()[1], "httptest2", "tests")); system.time(test_check("httptest2", filter="${file}", reporter=ifelse(nchar("${r}"), "${r}", "summary")))'

deps:
	R -s -e 'options(repos="http://cran.at.r-project.org"); install.packages("pak"); pak::local_install(".", dependencies = TRUE)'

build: doc
	R CMD build .

check: build
	-export _R_CHECK_CRAN_INCOMING_REMOTE_=FALSE && R CMD check --as-cran httptest2_$(VERSION).tar.gz
	rm -rf httptest2.Rcheck/

release: build
	-R CMD check --as-cran httptest2_$(VERSION).tar.gz
	rm -rf httptest2.Rcheck/

man: doc
	R CMD Rd2pdf man/ --force

md:
	R CMD INSTALL --install-tests .
	mkdir -p inst/doc
	R -e 'setwd("vignettes"); lapply(dir(pattern="Rmd"), knitr::knit, envir=globalenv())'
	mv vignettes/*.md inst/doc/
	-cd inst/doc && ls | grep .md | xargs -n 1 egrep "^.. Error"

build-vignettes: md
	R -e 'setwd("inst/doc"); lapply(dir(pattern="md"), function(x) markdown::markdownToHTML(x, output=sub("\\\\.md", ".html", x)))'
	cd inst/doc && ls | grep .html | xargs -n 1 sed -i '' 's/.md)/.html)/g'

covr:
	export NOT_CRAN=true && R -s -e 'library(covr); cv <- package_coverage(); df <- covr:::to_shiny_data(cv)[["file_stats"]]; cat("Line coverage:", round(100*sum(df[["Covered"]])/sum(df[["Relevant"]]), 1), "percent\\n"); report(cv)'

style:
	R -s -e 'if (requireNamespace("styler")) styler::style_file(system("git diff --name-only | grep .*R$$", intern = TRUE))'

style-all:
	R -s -e 'styler::style_file(dir(pattern = "R$$", recursive = TRUE))'
