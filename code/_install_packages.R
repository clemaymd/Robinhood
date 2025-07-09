# Install R package

library_to_install <- c("here",
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

new_libraries <- library_to_install[!(library_to_install %in% installed.packages()[,"Package"])]

# Install latest packages from CRAN
install.packages(new_libraries, repos = "http://cran.us.r-project.org")


