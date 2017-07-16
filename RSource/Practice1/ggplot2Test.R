library(ggplot2)
library(plotROC)
set.seed(2529)
D.ex <- rbinom(200, size = 1, prob = .5)
M1 <- rnorm(200, mean = D.ex, sd = .65)
M2 <- rnorm(200, mean = D.ex, sd = 1.5)

test <- data.frame(D = D.ex,  D.str = c("qq", "ww")[D.ex + 1],
                   M1 = M1, M2 = M2, stringsAsFactors = FALSE)
longtest<-melt_roc(test,"D",c("M1","M2"))
basicplot <- ggplot(longtest, aes(d = D, m = M,color =name)) + geom_roc()
basicplot
seq(0,1,by=0.1)
#seq_along()  2121