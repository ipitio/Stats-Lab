# set this file's dir as working dir
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
library(tidyverse)
this_dir <-  function() {
    this_file <- commandArgs() %>%
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col = value, into = c("key", "value"),
                    sep = "=", fill = "right") %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
    if (length(this_file) == 0) {
      this_file <- rstudioapi::getSourceEditorContext()$path
    }
    return(dirname(this_file))
}
setwd(this_dir())

# Q1

height <- c(1.55, 1.92, 1.60, 1.75, 1.58, 1.67,
            1.63, 1.82, 1.76, 1.77, 1.72, 1.85)
cat("Q1:\n\tmean:\t\t", mean(height), "\n\tsd:\t\t\t", sd(height),
    "\n\tlen:\t\t", length(height), "\n\tnum < 1.65:\t", sum(height < 1.65),
    "\n\tis 1.6 < val < 1.75:\n\t\t\t\t", file = "1.txt")
write.table(data.frame(height, c(height > 1.6 & height < 1.75)), "1.txt",
            TRUE, sep = " \t", row.names = FALSE, eol = "\n\t\t\t\t",
            col.names = FALSE)
cat("\n", file = "1.txt", append = TRUE)