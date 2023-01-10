
## How to download a package, which is not found at cran repository.

### update Rtools
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")

### Download at direct url

url <- "http://cran.r-project.org/src/contrib/Archive/ElemStatLearn/ElemStatLearn_2015.6.26.tar.gz"
pkgFile <- "ElemStatLearn_2015.6.26.tar.gz"
download.file(url = url, destfile = pkgFile)

install.packages(pkgs = pkgFile, type = "source", repos = NULL)

unlink(pkgFile)
