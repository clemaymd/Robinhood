# Load R packages

#options(warn = -1)

list_pkg <- c("here",
              "xts",
              "data.table",
              "chron",
              "dplyr",
              "ggplot2",
              "gridExtra",
              "ggpubr",
              "xtable",
              "sandwich",
              "lmtest",
              "plm",
              "car",
              "rugarch")

for (i in seq(along = list_pkg)) {
    suppressMessages(suppressWarnings(library(list_pkg[i], character.only = TRUE)))
}