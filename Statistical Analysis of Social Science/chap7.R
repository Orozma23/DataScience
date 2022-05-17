#예제 7-2(p.221)
lamb <- 1:5; x <- 0:150/50
dcol <- c("red", "blue", "green2", "purple", "orange2")

win.graph(9, 5); par(mfrow=c(1,2))
plot(x, dexp(x, lamb[1]), type="l", main="지수분포의 확률밀도함수",
     lwd=2, col=dcol[1], ylab="f(x)", xlab="(a)", ylim=c(0,5))
grid(col=3)
for (i in 2:5) lines(x, dexp(x, lamb[i]), lwd=2, col=dcol[i])
text(0, lamb, labels=expression(lambda), pos=4)
text(0.08, lamb, labels=paste0("=", lamb), pos=4)

plot(x, pexp(x, lamb[1]), type="l", main="지수분포의 누적분포함수",
     lwd=2, col=dcol[1], ylim=c(0,1), ylab="F(x)", xlab="(b)")
grid(col=3)
for (i in 2:5) lines(x, pexp(x, lamb[i]), lwd=2, col=dcol[i])
text(0.5, pexp(0.5, lamb), labels=expression(lambda))
text(0.5, pexp(0.5, lamb), labels=paste0("=", lamb), pos=4)


#예제 7-3(p.222)
Lam <- 1/10
pexp(8, Lam, lower.tail=F)/pexp(5, Lam, lower.tail=F)


#예제 7-4(p.223)
-10000*log(0.9)
Lam <- 1/10000
x <- 0:10000000/1000
x[abs(pexp(x, Lam, lower.tail=F)-0.9) < 0.0000001]


#예제 7-6(p.225)
th <- 1; alp <- c(0.5, 1, 2, 3)
x <- 0:150/30
dcol <- c("red", "blue", "green2", "purple")

win.graph(9, 5); par(mfrow=c(1,2))
plot(x, dgamma(x, shape=alp[1], scale=1), type="l", lwd=2, col=dcol[1],
     main="감마분포의 확률밀도함수", ylim=c(0, 1.2), ylab="f(x)", xlab="(a)")
grid(col=3)
for (i in 2:4) lines(x, dgamma(x, shape=alp[i], scale=1), lwd=2, col=dcol[i])
text(0.2, c(1.2, 1, 0.3, 0.1), labels=expression(alpha))
text(0.18, c(1.2, 1, 0.3, 0.1), labels=paste0("=", alp), pos=4)

plot(x, pgamma(x, shape=alp[1], scale=1), type="l", lwd=2, col=dcol[1],
     main="감마분포의 누적분포함수", ylab="F(x)", xlab="(b)")
grid(col=3)
for (i in 2:4) lines(x, pgamma(x, shape=alp[i], scale=1), lwd=2, col=dcol[i])
text(1.5, pgamma(1.5, alp, 1), labels=expression(alpha))
text(1.48, pgamma(1.5, alp, 1), labels=paste0("=", alp), pos=4)
