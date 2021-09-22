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
    str <- paste("conclusion: p-value =", val,
           ifelse(val <= lvl, rjct(lvl), rjct(lvl) %>%
           str_replace_all(c("<=" = ">", "so" = "so fail to"))), hyp)
    if (f) cat(str)
    invisible(str)
}

# Q1

stock <- read.table("d_logret_6stocks.txt", T)
intel <- stock$Intel
pfizer <- stock$Pfizer
ip_var <- var.test(intel, pfizer)
t_i <- t.test(intel)
w_i <- wilcox.test(intel, conf.int = T)
t_ip <- t.test(intel, pfizer, var.equal = ip_var$p.value > 0.05)
w_ip <- wilcox.test(intel, pfizer, conf.int = T)

cat("a:\n")
t_i
conc(t_i, "mean of intel return is zero\n\n")

cat("b:\n")
w_i
conc(w_i, "mean of intel return is zero\n\n")

cat("c:\n")
t_ip
conc(t_ip, "mean returns of pfizer and intel are the same\n\n")

cat("d:\n")
w_ip
conc(w_ip, "mean returns of pfizer and intel are the same\n\n")

cat("e:\n")
ip_var
conc(ip_var, "variances of returns of pfizer and intel are the same\n\n")

# Q2

bp26 <- c(152, 157, 179, 185, 178, 149)
bp5 <- c(384, 369, 354, 367, 375, 423)
t_bp <- t.test(bp26, bp5, "greater",
               var.equal = var.test(bp26, bp5)$p.value > 0.05)
t_bp
conc(t_bp, "mean blood pressures are the same\n\n")

# Q3

lvl <- .1
affected <- c(488, 478, 480, 426, 440, 410, 458, 460)
not_a <- c(484, 478, 492, 444, 436, 398, 464, 476)
s_aff <- shapiro.test(affected)
s_not <- shapiro.test(not_a)
s_affc <- conc(s_aff, "data are normal\n", lvl, F)
s_notc <- conc(s_not, "data are normal\n", lvl, F)
v_a <- var.test(affected, not_a)
t_aff <- t.test(affected, not_a, conf.level = 1 - lvl,
                var.equal = v_a$p.value > lvl)
assumptions <- c(paste0("data in each group are ",
    ifelse(grepl("<", s_affc, F, F, T) ||
        grepl("<", s_notc, F, F, T), "not ", ""), "normal"),
    paste0("variances of the groups are ",
        ifelse(!v_a$p.value > lvl, "not ", ""), "equal"))

cat("a:\n\tAffected")
s_aff
cat(s_affc)
cat("\n\tNot Affected")
s_not
cat(s_notc)
v_a
conc(v_a, "variances are equal\n\nassumptions checked:\t")
for (assumption in assumptions) cat(assumption, "\n\t\t\t\t\t\t")
t_aff
conc(t_aff, "corneal thickness is equal in affected v unaffected eyes\n\n")
cat("b:\t", t_aff$conf.int[1:2], "\n\n")

# Q4

mean <- 25
time <- c(28, 25, 27, 31, 10, 26, 30, 15, 55, 12, 24, 32, 28, 42, 38)
s_time <- shapiro.test(time)
t_time <- t.test(time, alternative = "greater", mu = mean, conf.level = .95)

cat("a:\n")
s_time
conc(s_time, "data are normal\n\n")

cat("b:\n")
t_time
conc(t_time, paste("mean time for a warehouse to fill a buyers order is", mean,
     "minutes\n\n"))