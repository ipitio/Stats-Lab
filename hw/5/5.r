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
sset <- gather(data[, c("Pfizer", "Exxon", "Citigroup")], tic, ret, 1:2)
citi <- data$Citigroup
limo <- lm(Pfizer ~ Exxon, data)
cat("\na)\t Intercept:\t", limo$coefficients[[1]],
    "\n  \t Exxon:\t\t", limo$coefficients[[2]], "\nb)\n\t")
(vaan <- anova(limo))
conc(vaan, "regression effects are not significant")
cat("\nc)\n\t")
(grup <- anova(lm(ret ~ tic, sset)))
conc(grup, "means of the groups are equal")
cat("\nd)\t")
(test <- binom.test(length(citi[citi > 0]), length(citi)))
conc(test, "proportion of positive returns is 0.5")

# Q2

library(ISwR)
data <- na.exclude(data.frame(igf1 = juul$igf1, tanner = factor(juul$tanner,
        labels = c("I", "II", "III", "IV", "V"))))
cat("\na)\n\t")
(vaan <- anova(lm(igf1 ~ tanner, data)))
conc(vaan, "igf1 means of each tanner level are equal")
cat("\nb)\n")
print.data.frame(plyr::ddply(data, ~tanner, summarise, mean = mean(igf1)))
cat("\nc)")
(test <- pairwise.t.test(data$igf1, data$tanner, p.adj = "bonf", pool.sd = F))
pval <- test$p.value
cat("Tanner level pairs that appear to have a difference:\n\t")
for (i in 1:nrow(pval)) {
  for (j in 1:ncol(pval)) {
    e <- pval[i, j]
    if (!is.na(e) && .5 > e) {
      pair <- paste(rownames(pval)[i], "and", colnames(pval)[j])
      cat(ifelse(!(i == nrow(pval) && j == ncol(pval)),
        paste0(pair, ", "), paste("and", pair)))
    }
  }
}

# Q3

library(MASS)
(test <- fisher.test(survey$Smoke, survey$Exer))
conc(test, "students smoking habit is independent of their exercise level")