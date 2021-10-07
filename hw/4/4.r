options(warn = -1)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ISwR)
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
test <- cor.test(x, y, conf.level = 0.99)
cat("\na)\t", cor(x, y),
    "\nb)\t y(x) =", paste0(m, "x +"), b,
    "\nc)\t", fit$r.squared, "\nd)")
test
conc(test, "correlation between x and y is zero")
cat(paste0("\ne)\t $", format(round(1000 * (m * 1.8 + b), 2), nsmall = 2)))

# Q2

data <- read.table("d_logret_6stocks.txt", header = T)
con <- lm(Intel ~ Citigroup, data)
test <- cor.test(data$Citigroup, data$Intel)
cat("\na)\t Intercept:", con$coefficients[[1]],
         "; Citigroup:", con$coefficients[[2]],
    "\nb)\t Citigroup:", lm(Intel ~ 0 + Citigroup, data)$coefficients,
    "\nc)\t Correlation: ", test$estimate)
test
conc(test, "correlation between Intel and Citigroup is zero")

# Q3

library(ISwR)
attach(rmr)
#plot(body.weight, metabolic.rate, main = "Metabolic Rate vs Body Weight")
fit <- lm(metabolic.rate ~ body.weight)$coefficients
cat("The predicted metabolic rate is", fit[[2]] * 80 + fit[[1]])