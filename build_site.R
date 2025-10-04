#!/usr/bin/env Rscript
if(!require("quarto")){
  install.packages("quarto")
}
library(quarto)
if(!quarto::quarto_binary_sitrep()){
  stop("Something is wrong with your quarto installation.")
}
quarto::quarto_render(".")
system("git add docs/*")

if(file.exists("index.qmd")){
  system("git add index.qmd")
}

if(file.exists("miniproject01.qmd")){
  system("git add miniproject01.qmd")
}

# if(!any(grepl("rstudio", search()))){q("no")}



