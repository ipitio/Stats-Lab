options(warn = -1)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
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
setwd(this_dir())

rjct <- function(lvl) paste("<= alpha =", lvl,
    "so reject the null\n\t\t\thypothesis that the")
conc <- function(test, hyp, lvl = 1 - attr(test$conf.int, "conf.level"),
                 f = T) {
    lvl <- ifelse(length(lvl) == 0, 0.05, lvl)
    val <- test$p.value
    str <- paste("conclusion: p-value =", format(round(val, 4), nsmall = 4),
           ifelse(val <= lvl, rjct(lvl), rjct(lvl) %>%
           str_replace_all(c("<=" = ">", "so" = "so fail to"))), hyp)
    if (f) cat(str)
    invisible(str)
}

# Q1

x <- c(2.4, 1.6, 2.0, 2.6, 1.4, 1.6, 2.0, 2.2)
y <- c(225, 184, 220, 240, 180, 184, 186, 215)
fit <- summary.lm(lm(y~x))
m <- fit$coefficients[[2, 1]]
b <- fit$coefficients[[1, 1]]
cat("\na)\t", cor(x, y),
    "\nb)\t y(x) =", paste0(m, "x +"), b,
    "\nc)\t", fit$r.squared, "\nd)")
test <- cor.test(x, y, conf.level = 0.99)
test
conc(test, "correlation between x and y is zero")
cat(paste0("\ne)\t $", format(round(1000 * (m * 1.8 + b), 2), nsmall = 2)))
