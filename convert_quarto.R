# Rename Rmd files
library(fs)
library(stringr)
rmd_names <- dir_ls(path = ".", glob = "*.Rmd")
qmd_names <- str_replace(string = rmd_names,
                         pattern = "Rmd",
                         replacement = "qmd")
file_move(path = rmd_names,
          new_path = qmd_names)

# Convert the YAML files
file_move(path = "_bookdown.yml",
          new_path = "_quarto.yml")

# Rename and Reindex the chapters
library(readr)
library(stringr)
quarto_yaml_rmd <- read_lines("_quarto.yml")

quarto_yaml_qmd <- str_replace_all(string = quarto_yaml_rmd,
                                   pattern = "Rmd",
                                   replacement = "qmd")

write_lines(
    x = quarto_yaml_qmd,
    file = "_quarto.yml"
)
