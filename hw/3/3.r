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

rjct <- function(lvl) paste("<", lvl,
                            "so reject the null hypothesis that the\n")
fail <- function(lvl) rjct(lvl) %>%
    str_replace_all(c("<" = ">", "so" = "so fail to"))
conc <- function(test, lvl = 1 - attr(test$conf.int, "conf.level")) {
  lvl <- ifelse(length(lvl) == 0, 0.05, lvl)
  ifelse(test$p.value > lvl, fail(lvl), rjct(lvl))
}

# Q1

stock <- read.table("d_logret_6stocks.txt", T)
intel <- stock$Intel
pfizer <- stock$Pfizer
ip_var <- var.test(intel, pfizer)
t_i <- t.test(intel)
w_i <- wilcox.test(intel, pfizer, conf.int = T)
t_ip <- t.test(intel, pfizer, var.equal = ip_var$p.value > 0.05)
w_ip <- wilcox.test(intel, pfizer, conf.int = T)

cat("a:\n")
t_i
cat("conclusion: p-value =", t_i$p.value, conc(t_i),
    "mean of intel return is zero\n\n")

cat("b:\n")
w_i
cat("conclusion: p-value = ", w_i$p.value, conc(w_i),
    "mean of intel return is zero\n\n")

cat("c:\n")
t_ip
cat("conclusion: p-value =", t_ip$p.value, conc(t_ip),
    "mean returns of pfizer and intel are the same\n\n")

cat("d:\n")
w_ip
cat("conclusion: p-value =", w_ip$p.value, conc(w_ip),
    "mean returns of pfizer and intel are the same\n\n")

cat("e:\n")
ip_var
cat("conclusion: p-value =", ip_var$p.value, conc(ip_var),
    "variances of returns of pfizer and intel are the same\n\n")

# Q2

bp26 <- c(152, 157, 179, 185, 178, 149)
bp5 <- c(384, 369, 354, 367, 375, 423)
t_bp <- t.test(bp26, bp5, "greater",
               var.equal = var.test(bp26, bp5)$p.value > 0.05)
t_bp
cat("conclusion: p-value =", t_bp$p.value, conc(t_bp),
    "mean blood pressures are the same\n\n")

# Q3

lvl <- .1
affected <- c(488, 478, 480, 426, 440, 410, 458, 460)
not_a <- c(484, 478, 492, 444, 436, 398, 464, 476)
s_aff <- shapiro.test(affected)
s_not <- shapiro.test(not_a)
s_affc <- conc(s_aff, lvl)
s_notc <- conc(s_not, lvl)
v_a <- var.test(affected, not_a)
t_aff <- t.test(affected, not_a, conf.level = 1 - lvl,
                var.equal = v_a$p.value > lvl)
assumptions <- c(paste("data in each group are ", sep = "",
ifelse(grepl("<", s_affc, F, F, T) || grepl("<", s_notc, F, F, T), "not ", ""),
"normal"), paste("variances of the groups are ",
ifelse(!v_a$p.value > lvl, "not ", ""), "equal", sep = ""))

cat("a:\n\tAffected")
s_aff
cat("conclusion: p-value =", s_aff$p.value, s_affc,
    "data are normal\n\n\tNot Affected")
s_not
cat("conclusion: p-value =", s_not$p.value, s_notc,
    "data are normal\n")
v_a
cat("conclusion: p-value =", s_not$p.value, s_notc,
    "variances are equal\n\nassumptions checked:\t")
for (assumption in assumptions) cat(assumption, "\n\t\t\t\t\t\t")
t_aff
cat("conclusion: p-value =", t_aff$p.value, conc(t_aff),
    "corneal thickness is equal for affected versus unaffected eyes\n\n")
cat("b:\t", t_aff$conf.int[1:2], "\n\n")

# Q4

mean <- 25
time <- c(28, 25, 27, 31, 10, 26, 30, 15, 55, 12, 24, 32, 28, 42, 38)
s_time <- shapiro.test(time)
t_time <- t.test(time, alternative = "greater", mu = mean, conf.level = .95)

cat("a:\n")
s_time
cat("conclusion: p-value =", s_time$p.value, conc(s_time),
    "data are normal\n\n")

cat("b:\n")
t_time
cat("conclusion: p-value =", t_time$p.value, conc(t_time),
    "mean time for a warehouse to fill a buyers order is", mean, "minutes\n\n")