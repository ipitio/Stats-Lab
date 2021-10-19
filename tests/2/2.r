if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, leaps, MASS)
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
attach(data)
(fit1 <- lm(Pfizer ~  AmerExp))
cat("\na)\t Intercept:\t", fit1$coefficients[[1]],
    "\n  \t AmerExp:\t\t", fit1$coefficients[[2]], "\nb)\n\t")
(table <- anova(fit1))
conc(table, "effects are not significant")
test <- cor.test(Pfizer, AmerExp)
cat("\nc)\n\tCorrelation:\t", cor(Pfizer, AmerExp),
    "\n\tCor Test:")
test
conc(test, "correlation is zero")
cat("\nd)")

cat("\ne)")
Group1 <- Exxon
Group2 <- AmerExp
Group3 <- Pfizer
y <- c(Group1, Group2, Group3)
x <- c(rep(1, 64), rep(2, 64), rep(3, 64))
fit2 <- lm(y ~ factor(x))
(test2 <- anova(fit2))
conc(test2, "means are the same")

# Q2

library(MASS)
model <- lm(y ~ 0 + x1 + x2 + x3 + x4, data = cement)
best <- coef(leaps::regsubsets(y ~ ., data = cement, nvmax = 2), 1:2)[[2]]
cat("\na) Coefficients:\t", model$coefficients,
    "\nb) Adj. R Squared:\t", summary(model)$adj.r.squared,
    "\nc) Best:\ty = ", best[1], " + ", best[2], "x1 + ", best[3], "x2")

# Q3

tredecula <- c(0.09, 0.17, 0.19, 0.16, 0.27, 0.15, 0.25, 0.16, 0.14)
tredecassini <- c(0.28, 0.15, 0.22, 0.11, 0.20, 0.17, 0.23, 0.18, 0.23)
tredecim <- c(0.28, 0.29, 0.14, 0.18, 0.32, 0.39, 0.33, 0.26, 0.12)
t_tredecula <- shapiro.test(tredecula)
t_tredecassini <- shapiro.test(tredecassini)
t_tredecim <- shapiro.test(tredecim)
list <- list(b1 = tredecula, b2 = tredecassini, b3 = tredecim)
stack <- stack(list)
bart <- bartlett.test(list)
cat("\na)")
t_tredecula
conc(t_tredecula, "tredecula weights are normal", 0.1)
t_tredecassini
conc(t_tredecassini, "tredecassini weights are normal", 0.1)
t_tredecim
conc(t_tredecim, "tredecim weights are normal", 0.1)
cat("\nb)")
bart
conc(bart, "variances are the same", 0.1)
cat("\nc")
stripchart(values ~ ind, data = stack, method = "jitter")
cat("\nd")
(test <- anova(lm(values ~ ind, data = stack)))
conc(test, "the weights are the same")


# Q4

cat("\na)")
(obs <- t(matrix(c(4, 6, 10, 18, 14, 20, 5, 10), nrow = 2)))
test <- chisq.test(obs)
pval <- test$p.value
cat("\nb)")
test
conc(test, "effects are independent")
cat("\nc)")
test$expected