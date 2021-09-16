if (!require("pacman")) install.packages("pacman")
pacman::p_load(evir, ISwR, MASS)
options(warn = -1)

cat("a)\t", pnorm(16, 15, 3), "\nb)\t", pchisq(8, 10), "\nc)\t",
    dbinom(5, 10, 0.4), "\nd)\t", dpois(5, 3))

table <- as.table(array(c(25, 18, 13, 6, 121, 92, 130, 87), dim = c(4, 2),
                dimnames = list(c("18-20", "21-23", "24-25", ">25"),
                c("Yes", "No"))))
names(attributes(table)$dimnames) <- c("Age", "Accidents")
table

evir::emplot(rexp(100, 2))

x <- rnorm(50)
boxplot(x, log(abs(x)), main = "x vs log(abs(x))",
        names = c("x", "log(abs(x))"))

hist(ISwR::react, breaks = 18)

MASS::truehist(ISwR::react)