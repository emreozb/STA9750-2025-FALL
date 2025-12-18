#!/usr/bin/env Rscript
if(!require("quarto")){
  install.packages("quarto")
}
library(quarto)
if(!quarto::quarto_binary_sitrep()){
  stop("Something is wrong with your quarto installation.")
}
quarto::quarto_render(".")
system("git add -A docs")

files <- c("index.qmd","mp01.qmd","mp02.qmd","mp03.qmd","mp04.qmd","summary_report.qmd")
for (f in files) if (file.exists(f)) system(paste("git add", shQuote(f)))
