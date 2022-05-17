t <- seq(1:60)
s <- sin(2*pi*t/12) + sin(2*pi*t/30)
a <- rnorm(60, 0, 1)
x <- s + a

plot(x, type="l", ylab="", ylim=c(-3,3), lty="dotted)
x11()
plot(s, type="l", ylim=c(-3,3), col="blue")
par(new=T)
plot(x, type="l", ylab="", ylim=c(-3,3), lty="dotted")

smooth.f <- smooth(x)
x11()
plot(smooth.f, type="l", ylab="", ylim=c(-3,3), col="blue")
par(new=T)
plot(x, type="l", ylim=c(-3,3), lty="dotted")

Telemetric <- read.table("Telemetric.txt", header=T)
plot(Telemetric$temperature, type="l", lty="dotted", ylim=c(45, 95))

x11()
plot(Telemetric$temperature, type="l", lty="dotted", ylim=c(45, 95))
par(new=T)
plot(smooth(Telemetric$temperature, twiceit=T), type="l", col="blue",
     ylim=c(45, 95), ylab="")

Export <- read.table("Export_1988.txt", header=T)
Series <- ts(Export$Series/1000, start=c(1988,1), frequency=12)
plot(Series)
x11()
Log.Series <- log(Series)
plot(Log.Series, ylim=c(8.5, 10.0))

decomp.out <- decompose(Log.Series)
round(decomp.out$season[1:12], 4)

x11(); plot(decomp.out$trend)

Adjusted <- exp(Log.Series - decomp.out$seasonal)
Adj.Series <- ts(Adjusted, start=c(1988,1), frequency=12)
x11(); plot(Adj.Series, ylim=c(5000,25000))

library(MASS)
geyser
str(geyser)
attach(geyser)
acf(waiting)
x11(); acf(duration)
plot(duration ~ waiting)

round(acf(waiting)$acf[1:5],3)
round(acf(duration)$acf[1:5],3)

ccf(waiting, duration)
attach(ccf(waiting, duration))
round(cbind(ccf(waiting, duration)$lag, ccf(waiting, duration)$acf),3)

plot(duration ~ waiting)

waiting.1 <- c(waiting[2:299], NA)
x11(); plot(waiting.1 ~ duration)
duration.1 <- c(duration[2:299], NA)
x11(); plot(duration.1 ~ waiting)

