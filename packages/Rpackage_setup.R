##########################################
##### Build 'indexanalysis' package
##### Notes on how to build a package
##########################################

# 1. Load libraries
library(devtools)
library(roxygen2)
go_inside_package <- file.path("indexanalysis")
# 2. Setup environment
getwd()
setwd(file.path("~", "GitHub_Projects", "hiring-index-analyst-polivier", "p1_packages"))

devtools::create(go_inside_package) # create directory
setwd(file.path(go_inside_package)) # move inside the package
getwd()
devtools::document() # build documentation
## Add dependencies to "DESCRIPTION" file under one of the categories: "Imports:", "Suggests:"...
usethis::use_vignette("ind-wrangling", "Introduction to Package 'indexwrangling'") # Generate a blank vignette
devtools::build_rmd(file.path("vignettes", "ind-wrangling.Rmd")) # Build the HTML from the Rmd
usethis::use_package(package = c("quantmod"), type = "imports") # Add dependencies to Description > 'Imports:'
usethis::use_package(package = c("zoo"), type = "imports") # Add dependencies to Description > 'Imports:'
usethis::use_package(package = c("tidyr"), type = "imports") # Add dependencies to Description > 'Imports:'
usethis::use_package(package = c("lubridate", "purrr"), type = "imports") # Add dependencies to Description > 'Imports:'

usethis::use_pipe() # Add the magrittr pipe as dependencies
usethis::use_mit_license("Pierre Olivier")
devtools::document() # build documentation


usethis::use_tidy_description() # Reorder the DESCRIPTION file according to standards
load_all() # load functions interactively to memory
library("indexanalysis") # load and attach package
search() # view the search path available to access variables and functions from packages

setwd("..")
devtools::install(go_inside_package) # if the path directory is at the package directory, will install the package
# To add new function, add them to the .R file or a new .R file in 'R' folder
# Go inside the folder of the package
# Run:
setwd(file.path(go_inside_package)) # move inside the package
devtools::check() # Check that all the parts are available and can be installed
devtools::document() # build documentation
# Rerun install()
# 
# If Error: rdb corrupted, restart session

# usethis::use_r("compute_corr") # Create an .R file with this name in the R folder that hosts functions
