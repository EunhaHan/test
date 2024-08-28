setwd("C:/Users/Owner/OneDrive - Monash University/KOU/2024-2 Biostatistics/R practice")

pop <- (-4):4
sample(pop, size=4, replace=TRUE)

set.seed(1000)
sam <- sample(pop, size=4, replace=TRUE)
mean(sam)

m <- 10
xbar.vec <- rep(NA,m)
for(i in 1:m){
  set.seed(1000+i)
  xx <- sample(pop, size=4, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=10", xlab=bquote(bar(x)))
mean(xbar.vec)
sd(xbar.vec)

m <- 100
xbar.vec <- rep(NA,m)
for(i in 1:m){
  set.seed(1000+i)
  xx <- sample(pop, size=4, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=100", xlab=bquote(bar(x)))
mean(xbar.vec)
sd(xbar.vec)


m <- 1000
xbar.vec <- rep(NA,m)
for(i in 1:m){
  set.seed(1000+i)
  xx <- sample(pop, size=4, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=1000", xlab=bquote(bar(x)))
mean(xbar.vec)
sd(xbar.vec)

m <- 10000
xbar.vec <- rep(NA,m)
for(i in 1:m){
  set.seed(1000+i)
  xx <- sample(pop, size=4, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=4, m=10000", xlab=bquote(bar(x)))
mean(xbar.vec)
sd(xbar.vec)


m <- 10000
xbar.vec <- rep(NA,m)
for(i in 1:m){
  set.seed(1000+i)
  xx <- sample(pop, size=40, replace=TRUE)
  xbar.vec[i] <- mean(xx)
}
table(xbar.vec)
hist(xbar.vec, main="n=40, m=10000", xlab=bquote(bar(x)), xlim=c(-4,4))
mean(xbar.vec)
sd(xbar.vec)





dat0 <- read.csv("biostat_ex_data.csv")
library(dplyr)
dat1 <- dat0 %>% mutate_at(vars(sex, Recur, stage, smoking, obesity,
                                Recur_1y, post.CA19.9.binary,
                                post.CA19.9.3grp), as.factor)

mean(dat1$weight)
t.test(dat1$weight)

t.test(dat1$weight, conf.level=0.99)$conf.int