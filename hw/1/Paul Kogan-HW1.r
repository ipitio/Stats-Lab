if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ISwR, random)
options(warn = -1)
library(tidyverse)
this_dir <-  function() {
    this_dir <- commandArgs() %>%
      tibble::enframe(name = NULL) %>%
      tidyr::separate(fill = "right", col = value, sep = "=",
                      into = c("key", "value")) %>%
      dplyr::filter(key == "--file") %>%
      dplyr::pull(value)
    if (length(this_dir) == 0)
      this_dir <- rstudioapi::getSourceEditorContext()$path
    return(dirname(this_dir))
}
setwd(this_dir()) # output will be saved where this file is located
output <- "Paul Kogan-HW1.txt" # name of output file

# Q1

height <- c(1.55, 1.92, 1.60, 1.75, 1.58, 1.67,
            1.63, 1.82, 1.76, 1.77, 1.72, 1.85)
cat("# For best results, set tab to 4 spaces #\n\nQ1:\t1)\t",
    mean(height), "\n\t2)\t", sd(height), "\n\t3)\t", length(height),
    "\n\t4)\t", sum(height < 1.65), "\n\t5)\t ", file = output)
write.table(data.frame(height, c(height > 1.6 & height < 1.75)), output,
            TRUE, sep = " \t", row.names = FALSE, eol = "\n\t\t ",
            col.names = FALSE)
cat("\n", file = output, append = TRUE)

# Q2

tmp <- matrix(rnorm(12), 3, 4)
cat("Q2:\t1)\t", sum(rowSums(tmp)[-1]), "\n\t2)\t",
    prod(colSums(tmp)[c(2, 4)]), "\n\t3)\t", dim(tmp), "\n\t4)\t",
    c((less <- tmp[2, ])[less < 0.2]), "\n\n", file = output, append = TRUE)

# Q3

library(ISwR)
cat("Q3:\t1)\t\t", file = output, append = TRUE)
write.table(subset(thuesen, blood.glucose > 10 & short.velocity > 1.5),
            col.names = c("glucose", "velocity"), file = output, append = TRUE,
            sep = "  \t", eol = "\n\t\t ")
cat("\n", file = output, append = TRUE)

# Q4

library(random)
cat("Q4:\t1)\t", randomNumbers(15, 0, 81),
    "\n\n", file = output, append = TRUE)

# Q5

rnd <- randomNumbers(1, 1, 10^6, 1)
prb <- c(0.2, 0.3, 0.5)
len <- length(prb)
size <- as.integer(rnd / len)
cat("Q5:\t1)\t", sample(sample(rnd, len, ifelse(rnd == 1, TRUE, FALSE)), size,
    TRUE, prb), "\n\t2)\t", rmultinom(ifelse(rnd != 1, size, 1), rnd, prb),
    file = output, append = TRUE)

writeLines(readLines(output)) # output printed to console for convenience
