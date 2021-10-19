if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ISwR, MASS)
library(tidyverse)
this_dir <- \() {
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

rjct <- \(lvl) paste("<= alpha =", lvl,
    "so reject the null\n\t\t\thypothesis that the")
conc <- \(test, hyp, lvl, cat = T) {
    lvl <- try(1 - attr(test$conf.int, "conf.level"))
    val <- try(test$p.value)
    lvl <- ifelse(length(lvl) == 0, .05, lvl)
    val <- ifelse(length(val) == 0, test[, 5], val)
    str <- paste("conclusion: p-value =", format(round(val, 4), nsmall = 4),
           ifelse(val <= lvl, rjct(lvl), rjct(lvl) %>%
           str_replace_all(c("<=" = ">", "so" = "so fail to"))), hyp)
    if (cat) cat(str)
    invisible(str)
}

# Q1

