options(warn = -1)
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

data <- read.table("d_logret_6stocks.txt", header = T)
limo <- lm(Pfizer ~ Exxon, data)
vana <- anova(limo)
sset <- data[, c("Pfizer", "Exxon", "Citigroup")] %>% gather(tic, ret, 1:2)
grup <- anova(lm(ret ~ tic, sset))
citi <- data$Citigroup
test <- binom.test(length(citi[which(citi > 0)]), length(citi))
cat("\na)\t Intercept:\t", limo$coefficients[[1]],
    "\n  \t Exxon:\t\t", limo$coefficients[[2]], "\nb)\n\t")
vana
conc(vana, "regression effects are not significant")
cat("\nc)\n\t")
grup
conc(grup, "means of the groups are equal")
cat("\nd)\t")
test
conc(test, "proportion of positive returns is 0.5")