pbinom(3, 20, 0.5)
dbinom(0:3, 20, 0.5)
?dbinom
?pbinom
binom.test(3, 20)
install.packages("DescTools")
library(DescTools)
BinomCI(3, 20, method = c("agresti-coull"))
binom.test(335, 636, 0.5)
BinomCI(335, 636, method = c("agresti-coull"))
install.packages("exact2x2")
library(exact2x2)
performance <- matrix(c(794, 86, 50, 570), nrow =2, dimnames = list("1st survey" = c("Approve",  "Disapprove"), "2nd survey" = c("Approve", "Disapprove")))
performance
mcnemar.test(performance, correct = T)
mcnemar.test(performance, correct = F)
mytable <- matrix(c(20, 12, 2, 16), nrow = 2, dimnames = list("row" = c("+", "-"), "column" = c("+", "-")))
mytable
mcnemar.test(mytable)
depersonalization <- matrix(c(14, 2, 5, 2), nrow = 2, dimnames = list("Without depersonalization" = c("Cured", "Not Cured"), "With depersonalization" = c("Cured", "Not Cured")))
depersonalization
mcnemar.test(depersonalization, correct = T)
