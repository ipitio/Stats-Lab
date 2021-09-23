options(warn = -1)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ISwR, random)
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
    str <- paste("conclusion: p-value =", val,
           ifelse(val <= lvl, rjct(lvl), rjct(lvl) %>%
           str_replace_all(c("<=" = ">", "so" = "so fail to"))), hyp)
    if (f) cat(str)
    invisible(str)
}

# Q1

rand.vec <- rchisq(70, 5)
rand.mat <- matrix(rand.vec, 10, 7, T)
cat("Q1:\tVector:\n")
rand.vec
cat("\n\tMatrix:\n")
rand.mat
cat("\n\tProduct:\t", prod(rand.mat[, 5]^2), "\n\n")

# Q2

prb <- c(.1, 0.3, 0.35, 0.25)
len <- length(prb)
rnd <- random::randomNumbers(1, len, len^len, 1)
amt <- as.integer(rnd / len)
cat("Q2:\t1)\n", sample(sample(rnd, len), rnd, T, prb),
    "\n\t2)\n", rmultinom(amt, rnd, prb))

# Q3

cat("Q3:\t1)\t", pchisq(11, 9),
    "\n\t2)\t", dpois(7, 2),
    "\n\t3)\t", pnorm(14, 12, 6, F),
    "\n\t4)\t", pexp(5, .2) - pexp(2, .2),
    "\n\t5)\t", dbinom(5, 12, .6), "\n\n")

# Q4

volume <- ISwR::lung$volume
s_vol <- shapiro.test(volume)
t_vol <- t.test(volume, mu = 3.5, alternative = "less")
cat("Q4:\t1)\n")
s_vol
conc(s_vol, "data are normal\n\n")
cat("\t2)\n")
t_vol
conc(t_vol, "mean of 'volume' is less than 3.5\n\n")

# Q5

lvl <- .1
after <- c(49, 60, 65, 52, 58, 67)
before <- c(75, 82, 79, 65, 77, 83)
t_bp <- t.test(before, after, paired = T, var.equal =
               var.test(before, after)$p.value > lvl)
cat("Q5:\n")
t_bp
conc(t_bp, "new drug can decrease the blood pressure of patients")