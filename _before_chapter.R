# Script to run before each chapters

## Clean environment
rm(list = ls(all = TRUE))

## Setup path to Box modules
old_opts <- options() # preserve current preferences for after we exist the function
options(box.path = c("./scripts", 
                     "../scripts",
                     "./scripts/box",
                     "../scripts/box") )