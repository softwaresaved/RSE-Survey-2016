##### Script to init the packrat poject
##### The packages needed by the project
##### Are installed in the ../library folder
##### And are local to the project
##### Information about the package manager:
##### https://rstudio.github.io/packrat/


###### Set directory #####

setwd('~/git/ssi/RSE-Survey-2016/')

####### Load libraries ########
package <- 'packrat'

if (!require(package, character.only=T, quietly=T)) {
		install.packages(package)
		library(package, character.only=T)
}

packrat::init("../RSE-Survey-2016")
