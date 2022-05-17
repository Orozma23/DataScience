# 1)
x <- seq(-3, 3, 0.01)
y <- dnorm(x)
plot(y ~ x, type="l", ylim=c(0,0.5), ylab="density")

# 2)
p <- (seq(1,500)-0.5)/500
z <- qnorm(p, mean=0, sd=1)
plot(z ~ p, type="l")
abline(c(0,0), lty="dotted")

# 3)
darwin <- c(49, -67, 8, 16, 6, 23, 28, 41, 14, 29, 56, 24, 75, 60, -48)
qqnorm(darwin)
p <- (1:length(darwin)-0.5)/length(darwin)
z <- qnorm(p)
plot(sort(darwin) ~ z, ylim=c(-75,75), xlim=c(-2,2), main="Darwin")
x11(); qqnorm(darwin, ylim=c(-75,75), xlim=c(-2,2))

# 4)
hist(rpois(1000,1), breaks=seq(-0.5,10.5,1))
hist(rpois(1000,2), breaks=seq(-0.5,10.5,1))

# 5)
par(mfrow=c(1,2))
qqnorm(rnorm(40,100,15))
qqnorm(rnorm(40,100,15))

# 6)
qqnorm(c(rnorm(20,70,15), rnorm(20,130,15)))
qqnorm(c(rnorm(20,70,15), rnorm(20,130,15)))

# 7)
qqnorm(c(25,175,rnorm(38,100,15)))
qqnorm(c(25,175,rnorm(38,100,15)))

# 8)
qqnorm(runif(40,80,120))
qqnorm(runif(40,80,120))

# 9)
qqnorm(c(rexp(20,1), -rexp(20,1)))
qqnorm(c(rexp(20,1), -rexp(20,1)))

# 10)
qqnorm(exp(rnorm(40,5,1)))
qqnorm(exp(rnorm(40,5,1)))

# 11)
qqnorm(1500 - exp(rnorm(40,5,1)))
qqnorm(1500 - exp(rnorm(40,5,1)))

# 12)
leukemia <- scan()
1 1 2 2 3 4 4 5 5 8 8 8 8 11 11 12 12 15 17 22 23
n <- length(leukemia)
p <- seq(1:n)/n - 0.5/n
x <- -log(1-p)
y <- leukemia
plot(y ~ x, main="Q-Q plot for exponential dist")

# 13)
library(lattice)
qqmath(~leukemia, distribution = function(p) qexp(p,1))

# 14)
qqmath(~leukemia, distribution = function(p) qweibull(p,1.5,1))

# 15)
x <- rbeta(80,2,3)*100
y <- rbeta(120,3,2)*100
x.quantile <- round(quantile(x),1)
y.quantile <- round(quantile(y),1)
par(mfrow=c(1,2))
qqplot(x, y, xlim=c(0,100), type="l")
x.quantile[1] <- 0
x.quantile[5] <- 100
y.quantile[1] <- 0
y.quantile[5] <- 100
plot(y.quantile ~ x.quantile, xlim=c(0,100), type="l")
