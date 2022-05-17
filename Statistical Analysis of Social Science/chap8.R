#예제 8-1(p.245)
x <- (-140:140)/20
fx <- matrix(c(dnorm(x,0,1), dnorm(x,0,2), dnorm(x,2,1), 
               dnorm(x,2,2)), nrow=4, byrow=T)
par(mfrow=c(2,2))
plot(x, fx[1,], type="l", lwd=2, col=2, ylab="f(x)", main="N(0,1)")
segments(0,0,0,max(fx[1,]), lty=2, col=4)
plot(x, fx[2,], type="l", lwd=2, col=2, ylab="f(x)", main="N(0,4)")
segments(0,0,0,max(fx[2,]), lty=2, col=4)
plot(x, fx[3,], type="l", lwd=2, col=2, ylab="f(x)", main="N(2,1)")
segments(2,0,2,max(fx[3,]), lty=2, col=4)
plot(x, fx[4,], type="l", lwd=2, col=2, ylab="f(x)", main="N(2,4)")
segments(2,0,2,max(fx[4,]), lty=2, col=4)


#예제 8-2(p.250)
pnorm(4, 2, 2) - pnorm(-1, 2, 2); pnorm(1)-pnorm(-1.5)


#예제 8-3(p.252)
z <- -4000:4000/1000
par(mfrow=c(1,1))
plot(z, pnorm(z,0,1), type="l", col=2, ylab="F(z)",
     main="표준정규분포누적확률")
grid(col=3)
xv <- -4:4/2
yv <- pnorm(xv)
segments(xv, 0, xv, yv, lty=2)
segments(xv, yv, -4, yv, lty=2)
text(xv, 0, labels=xv)
text(-4, yv, labels=round(yv,3))


#예제 8-4(p.254)
pv <- c(0.005, 0.01, 0.025, 0.05, 1:9/10, 0.95, 0.975, 0.99, 0.995)
zv <- qnorm(pv, 0, 1)

par(mfrow=c(1,1))
p <- 0:1000/1000
plot(p, qnorm(p, 0, 1), type="l", lwd=2, col=2, ylab="z",
     main="표준정규분포 분위수", xlim=c(-0.1, 1))
grid(col=3)
segments(pv, -3, pv, zv, lty=2)
segments(0, zv, pv, zv, lty=2)
text(pv[c(1,5:13,17)], -3, labels=pv[c(1,5:13,17)])
text(-0.05, zv, labels=round(zv,2))


# 예제 8-5(p.256)
pnorm(185, 175, 8) - pnorm(180, 175, 8)


#예제 8-6(p.256)
qnorm(0.05, 10, 1.5); 10+qnorm(0.05)*1.5


#예제 8-7(p.257)
pnorm(0,21.5-20,sqrt(0.3^2+0.4^2))


#예제 8-8(p.258)
pnorm(60,50,4) - pnorm(40,50,4)


#예제 8-9(p.259)
pbinom(4, 25, 0.2); pnorm(4.5, 5, 2)


#예제 8-10(p.260)
pbinom(45, 100, 0.5) - pbinom(39, 100, 0.5)


#예제 8-11(p.262)
p <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99, 0.995)
df <- c(1:40, 50 ,100)
q <- matrix(NA, nrow=length(df), ncol=length(p))
for(i in 1:length(df)) {
  q[i,] <- qchisq(p,df[i])
}
colnames(q) <- p
rownames(q) <- df
round(q, 3)


#예제 8-12(p.263)
df <- c(5, 10, 30, 100)

par(mfrow=c(2,2))
for (i in 1:4) {
  plot(0:20000/100, dchisq(0:20000/100, df[i]), type="l", 
       main=paste0("Chisq(", df[i], ")"), 
       xlab="x", ylab="f(x)", col=2, lwd=2)
}


#예제 8-13(p.265)
nu <- c(5, 10 ,30 , 100)
p <- 0.95

qchisq(p, nu)


#예제 8-14(p.265)
p <- 0:1000/1000
df <- c(5, 10, 30, 100)

par(mfrow=c(2,2))
for (i in 1:4) {
  plot(p, qchisq(p,df[i])/df[i], type="l", 
       main=paste0("chisq(", df[i], ")"),
       xlab="p", ylab="x(p)", col=2, lwd=2, ylim=c(0,3))
  abline(h=1)
}


#예제 8-15(p.267)
df <- c(1,5,10,30)
x <- -500:500/100

par(mfrow=c(2,2))
for (i in 1:4) {
  plot(x, dt(x,df[i]), type="l", main=paste0("t(",df[i],")"),
       xlab="x", ylab="f(x)", col=2, lwd=2, ylim=c(0,0.4))
  lines(x,dnorm(x,0,1), lty=2)
}
pt(1,df)-pt(-1,df)
pt(2,df)-pt(-2,df)
pt(3,df)-pt(-3,df)


#예제 8-16(p.269)
p <- c(5:9/10, 0.95, 0.975, 0.99, 0.995, 0.999, 0.9995)
nc <- length(p)

df <- c(1:40, 50, 100, Inf); nr <- length(df)

qv <- array(0, dim=c(nr, nc))
colnames(qv) <- p; rownames(qv) <- df
for (i in 1:nc) qv[,i] <- qt(p[i], df)
print(round(qv,3))


#예제 8-17(p.270)
x <- 100:300/100
df <- c(1:5,10,30)

par(mfrow=c(1,1))
plot(x, pnorm(x)-pnorm(-x), type="l", lwd=2, main="P(-x < X < x)",
     ylab="P(-x < X < x)", ylim=c(0.5,1))

for (i in 1:6) {
  lines(x, pt(x,df[i])-pt(-x,df[i]), col=i+1, lwd=2)
}
text(3, 1, labels="N(0,1)")
text(3, pt(3,1)-pt(-3,1), labels="t(1)")
text(3, pt(3,2)-pt(-3,2), labels="t(2)")
text(3, pt(3,3)-pt(-3,3), labels="t(3)")
text(3, pt(3,5)-pt(-3,5), labels="t(5)")


#예제 8-18(p.272)
n <- 10000
csq1 <- rchisq(n,8)
csq2 <- rchisq(n,5)
f <- (csq1/8)/(csq2/5)

par(mfrow=c(1,1))
hist(f, breaks=seq(0, max(f), length.out=250), prob=T, xlim=c(0,20),
     main="카이제곱 비율 통계량의 분포 F(8,5)")
lines(seq(0,20,0.1), df(seq(0,20,0.1),8,5), col=2, lwd=2)


#예제 8-19(p.273)
p <- c(0.9, 0.95, 0.975, 0.99)

df1 <- c(1:10, 11:15, 20, 30, 40 ,50, Inf); nc <- length(df1)
df2 <- c(1:40, 50, 100, Inf); nr <- length(df2)

pL <- vector("list", 4)
for (i in 1:4) {
  pL[[i]] <- matrix(qf(p[i], rep(df1, each=nr), df2), nrow=nr)
  colnames(pL[[i]]) <- df1; rownames(pL[[i]]) <- df2}
for (i in 1:4) {
  cat("p=", p[i], "\n")
  print(round(pL[[i]][,1:10], 3))
  print(round(pL[[i]][,11:nc], 3))
}


#연습문제 R1(p.277)
x1 <- (-140:140)/20
fx1 <- matrix(c(dnorm(x1,12,2), dnorm(x1,12,3), dnorm(x1,12,4), 
               dnorm(x1,15,3)), nrow=4, byrow=T)

par(mfrow=c(2,2))
plot(x1, fx1[1,], type="l", lwd=2, col=2, ylab="f(x)", main="N(12,2)")
segments(0,0,0,max(fx1[1,]), lty=2, col=4)
plot(x1, fx1[2,], type="l", lwd=2, col=2, ylab="f(x)", main="N(12,3)")
segments(0,0,0,max(fx1[2,]), lty=2, col=4)
plot(x1, fx1[3,], type="l", lwd=2, col=2, ylab="f(x)", main="N(12,4)")
segments(2,0,2,max(fx1[3,]), lty=2, col=4)
plot(x1, fx1[4,], type="l", lwd=2, col=2, ylab="f(x)", main="N(15,3)")
segments(2,0,2,max(fx1[4,]), lty=2, col=4)


#연습문제 R2(p.277)
p2 <- c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)
df2 <- 5
q2 <- matrix(NA, nrow=length(df2), ncol=length(p2))
for(i in 1:length(df2)) {
  q2[i,] <- qchisq(p2,df2[i])
}
colnames(q2) <- p2
rownames(q2) <- df2
round(q2, 3)


#연습문제 R3(p.277)



#연습문제 R4(p.277)
p4 <- 0.95
df4 <- 2:100
q4 <- matrix(NA, nrow=length(df4), ncol=length(p4))
for(i in 1:length(df4)) {
  q4[i,] <- qchisq(p4,df4[i])
}
colnames(q4) <- p4
rownames(q4) <- df4
round(q4, 3)


#연습문제 R5(p.278)
x5 <- 100:300/100
df5 <- c(1,3,5,10,30)
par(mfrow=c(1,1))
plot(x5, pnorm(x5)-pnorm(-x5), type="l", lwd=2, main="P(-x < X < x)",
     ylab="P(-x < X < x)", ylim=c(0.5,1))

for (i in 1:6) {
  lines(x5, pt(x5,df5[i])-pt(-x5,df5[i]), col=i+1, lwd=2)
}
text(3, 1, labels="N(0,1)")
text(3, pt(3,1)-pt(-3,1), labels="t(1)")
text(3, pt(3,2)-pt(-3,2), labels="t(2)")
text(3, pt(3,3)-pt(-3,3), labels="t(3)")
text(3, pt(3,5)-pt(-3,5), labels="t(5)")


#연습문제 R6(p.278)
par(mfrow=c(2,2))
#F(2,10)
n6 <- 10000
csq6_1 <- rchisq(n6,2)
csq6_2 <- rchisq(n6,10)
f6_1 <- (csq6_1/2)/(csq6_2/10)
hist(f6_1, breaks=seq(0, max(f6_1), length.out=250), prob=T, xlim=c(0,20),
     main="카이제곱 비율 통계량의 분포 F(2,10)")
lines(seq(0,20,0.1), df(seq(0,20,0.1),8,5), col=2, lwd=2)
#F(10,2)
n6 <- 10000
csq6_3 <- rchisq(n6,10)
csq6_4 <- rchisq(n6,2)
f6_2 <- (csq6_3/10)/(csq6_4/2)
hist(f6_2, breaks=seq(0, max(f6_2), length.out=250), prob=T, xlim=c(0,20),
     main="카이제곱 비율 통계량의 분포 F(10,2)")
lines(seq(0,20,0.1), df(seq(0,20,0.1),8,5), col=2, lwd=2)
#F(2,2)
n6 <- 10000
csq6_5 <- rchisq(n6,2)
csq6_6 <- rchisq(n6,2)
f6_3 <- (csq6_5/2)/(csq6_6/2)
hist(f6_3, breaks=seq(0, max(f6_3), length.out=250), prob=T, xlim=c(0,20),
     main="카이제곱 비율 통계량의 분포 F(2,2)")
lines(seq(0,20,0.1), df(seq(0,20,0.1),8,5), col=2, lwd=2)
#F(10,10)
n6 <- 10000
csq6_7 <- rchisq(n6,10)
csq6_8 <- rchisq(n6,10)
f6_4 <- (csq6_7/10)/(csq6_8/10)
hist(f6_4, breaks=seq(0, max(f6_4), length.out=250), prob=T, xlim=c(0,20),
     main="카이제곱 비율 통계량의 분포 F(10,10)")
lines(seq(0,20,0.1), df(seq(0,20,0.1),8,5), col=2, lwd=2)


#연습문제 R7(p.278)


