install.packages("devtools")
#You'll also need roxygen2 for documenting your functions (see below).
devtools::install_github("klutometis/roxygen")

library(roxygen2)


require(devtools)
require(roxygen2)
require(usethis)

#install.packages("roxygen2")
#The first thing you want to do is create the framework for your R package. We can do this using devtools:


#devtools::use_data(APE)

APE=data(apes)

BONE1=data(boneData)
SCP=data(scallopPLY)
DIGIT3=data(digit3.dat)
usethis::use_data(APE)
usethis::use_data(BONE1)
usethis::use_data(SCP)
usethis::use_data(DIGIT3)

#install("NBpagm")

devtools::document()

N
# now function to install your new package directly from the GitHub page.
#Test for Installation
#https://r-pkgs.org/git.html

## https://github.com/petrkeil/bPCA/blob/master/R/bPCA_functions.r

## https://tinyheero.github.io/jekyll/update/2015/07/26/making-your-first-R-package.html



devtools::load_all()
devtools::use_vignette("introduction")

usethat::use_vignette("introduction")
#install_github('cats','debashischatterjee111')

#This automatically adds in the .Rd files to the man directory, and adds a NAMESPACE file to the main directory


require(devtools)
install_github("debashischatterjee111/BPviGM1")



