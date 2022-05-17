install.packages("scatterplot3d")
library(prob)
library(scatterplot3d)

#예제 6-2(p.179)
n <- 10; p <- c(0.2, 0.5, 0.8); x <- 0:n

fx1 <- array(NA, dim=c(11,3))
for(i in 1:3) fx1[, i] <- dbinom(x, n, p[i])
colnames(fx1) <- p; rownames(fx1) <- x
round(t(fx1), 4)

apply(fx1, 2, sum)

win.graph(9,3); par(mfrow=c(1,3))
plot(x, fx1[,1], type="h", main=paste0("B(10", p[1], ")"), lwd=4, col=2)
plot(x, fx1[,2], type="h", main=paste0("B(10", p[2], ")"), lwd=4, col=2)
plot(x, fx1[,3], type="h", main=paste0("B(10", p[3], ")"), lwd=4, col=2)


#예제 6-3(p.180)
dbinom(0:2, 20, 0.03)
1-sum(dbinom(0:2, 20, 0.03)); pbinom(2, 20, 0.03, lower=F)

#예제 6-4(p.184)
N <- 50; S <- c(10, 25, 40); n <- 10; x <- 0:n

fx2 <- array(NA, dim=c(11,3))
for(i in 1:3) fx2[, i] <- dhyper(x, S[i], N-S[i], n)
colnames(fx2) <- p; rownames(fx2) <- x; round(t(fx2), 4)

apply(fx2, 2, sum)

win.graph(9,3); par(mfrow=c(1,3))
plot(x, fx2[,1], type="h", 
     main=paste0("HG(10, 50,", S[1],")"), lwd=4, col=2)
plot(x, fx2[,2], type="h", 
     main=paste0("HG(10, 50,", S[2],")"), lwd=4, col=2)
plot(x, fx2[,3], type="h", 
     main=paste0("HG(10, 50,", S[3],")"), lwd=4, col=2)

win.graph(9,3); par(mfrow=c(1,3)); d <- 0.1
plot(x-d, fx2[,1], type="h", main="HG(50) : Binom (n=10, p=0.2)",
     ylab=NA, xlab=NA, lwd=3, col=4)
lines(x+d, fx1[,1], type="h", lwd=3, col=2)
plot(x-d, fx2[,2], type="h", main="HG(50) : Binom (n=10, p=0.5)",
     ylab=NA, xlab=NA, lwd=3, col=4)
lines(x+d, fx1[,2], type="h", lwd=3, col=2)
plot(x-d, fx2[,3], type="h", main="HG(50) : Binom (n=10, p=0.8)",
     ylab=NA, xlab=NA, lwd=3, col=4)
lines(x+d, fx1[,3], type="h", lwd=3, col=2)


#예제 6-5(p.186)
dhyper(0:3, 50, 950, 30)
sum(dhyper(0:3, 50, 950, 30)); phyper(3, 50, 950, 30)


#예제 6-6(p.189)
L <- c(2, 5, 8); xr <- 0:20

fx3 <- array(NA, dim=c(21,3))
for(i in 1:3) fx3[, i] <- dpois(xr, L[i])
colnames(fx3) <- L; rownames(fx3) <- xr; round(t(fx3), 4)

apply(fx3, 2, sum)

win.graph(9,3); par(mfrow=c(1,3))
plot(xr, fx3[,1], type="h", 
     main=paste0("Poisson(", L[1], ")"), lwd=4, col=2)
plot(xr, fx3[,2], type="h", 
     main=paste0("Poisson(", L[2], ")"), lwd=4, col=2)
plot(xr, fx3[,3], type="h", 
     main=paste0("Poisson(", L[3], ")"), lwd=4, col=2)

win.graph(9,3); par(mfrow=c(1,3)); d <- 0.2
plot(x-d, fx2[,1], type="h", main="HG(50) : Binom : Pois (2)",
     ylab=NA, xlab=NA, lwd=3, col=4)
lines(x, fx1[,1], type="h", lwd=3, col=2)
lines(x+d, fx3[1:11, 1], type="h", lwd=3, col=3)
plot(x-d, fx2[,2], type="h", main="HG(50) : Binom : Pois (5)",
     ylab=NA, xlab=NA, lwd=3, col=4)
lines(x, fx1[,2], type="h", lwd=3, col=2)
lines(x+d, fx3[1:11, 2], type="h", lwd=3, col=3)
plot(x-d, fx2[,3], type="h", main="HG(50) : Binom : Pois (8)",
     ylab=NA, xlab=NA, lwd=3, col=4)
lines(x, fx1[,3], type="h", lwd=3, col=2)
lines(x+d, fx3[1:11, 3], type="h", lwd=3, col=3)


#예제 6-7(p.191)
dpois(0:2, 1.5)
1-sum(dpois(0:2, 1.5)); ppois(2, 1.5, lower=F)


#예제 6-8(p.194)
p <- c(0.1, 0.2, 0.3, 0.5); xr <- 1:30

fx4 <- array(NA, dim=c(30, 4))
for (i in 1:4) fx4[,i] <- dgeom(xr-1, p[i])
colnames(fx4) <- p; rownames(fx4) <- xr; round(t(fx4), 4)

apply(fx4, 2, sum)

par(mfrow=c(2,2))
plot(xr, fx4[,1], type="h", 
     main=paste0("Geometric(", p[1], ")"), lwd=3, col=2)
plot(xr, fx4[,2], type="h", 
     main=paste0("Geometric(", p[2], ")"), lwd=3, col=2)
plot(xr, fx4[,3], type="h", 
     main=paste0("Geometric(", p[3], ")"), lwd=3, col=2)
plot(xr, fx4[,4], type="h", 
     main=paste0("Geometric(", p[4], ")"), lwd=3, col=2)


#예제 6-10(p.200)
ps <- 0.4; r <- c(1, 2, 3, 4); xr <- 1:30

fx5 <- array(NA, dim=c(30, 4))
for (i in 1:4) fx5[, i] <- dnbinom(xr-r[i], r[i], ps)
colnames(fx5) <- r; rownames(fx5) <- xr; round(t(fx5), 4)

apply(fx5, 2, sum)

par(mfrow=c(2,2))
plot(xr, fx5[,1], type="h", 
     main=paste0("Neg-Binom(0.4,", r[1], ")"), lwd=3, col=2)
plot(xr, fx5[,2], type="h", 
     main=paste0("Neg-Binom(0.4,", r[2], ")"), lwd=3, col=2)
plot(xr, fx5[,3], type="h", 
     main=paste0("Neg-Binom(0.4,", r[3], ")"), lwd=3, col=2)
plot(xr, fx5[,4], type="h", 
     main=paste0("Neg-Binom(0.4,", r[4], ")"), lwd=3, col=2)


#예제 6-12(p.205)
library(prob)

ps <- matrix(c(1,1,8, 1,5,4, 4,4,2, 1,1,1), nrow=4, ncol=3, byrow=T)
n <- 5

xr <- urnsamples(1:3, size=5, replace=T, ordered=F)
nr <- nrow(xr); nr

x1 <- apply(xr, 1, function(x) sum(x==1))
x2 <- apply(xr, 1, function(x) sum(x==2))
x3 <- apply(xr, 1, function(x) sum(x==3))
xr <- cbind(x1, x2, x3); t(xr)

fx6 <- array(NA, dim=c(nr, 4))
for (j in 1:4) { for (i in 1:nr) {
  fx6[i, j] <- dmultinom(xr[i, ], size=n, prob=ps[j, ]) } }
rownames(fx6) <- paste0("(",x1,",",x2,",",x3,")")
colnames(fx6) <- paste0("P", 1:4)
fx6[order(x1,x2),]

apply(fx6, 2, sum)

library(scatterplot3d)
par(mfrow=c(2,2))
scatterplot3d(x1, x2, fx6[,1], type="h", 
              main="Multinom(0.1,0.1,0.8)",
              zlab="f(x1,x2,x3)", pch=16, lwd=5, color=2)
scatterplot3d(x1, x2, fx6[,2], type="h", 
              main="Multinom(0.1,0.5,0.4)",
              zlab="f(x1,x2,x3)", pch=16, lwd=5, color=2)
scatterplot3d(x1, x2, fx6[,3], type="h", 
              main="Multinom(0.4,0.4,0.2)",
              zlab="f(x1,x2,x3)", pch=16, lwd=5, color=2)
scatterplot3d(x1, x2, fx6[,4], type="h", 
              main="Multinom(1/3,1/3,1/3)",
              zlab="f(x1,x2,x3)", pch=16, lwd=5, color=2)


#연습문제 R1(p.210)
#(1)
CD <- 1:20
S2 <- urnsamples(CD, size=2)
N2 <- nrow(S2)
N2
X2 <- apply(S2, 1, mean)
P2 <- table(X2)/N2
V2 <- as.numeric(names(P2))

S5 <- urnsamples(CD, size=5)
N5 <- nrow(S5)
N5
X5 <- apply(S5, 1, mean)
P5 <- table(X5)/N5
V5 <- as.numeric(names(P5))

S10 <- urnsamples(CD, size=10)
N10 <- nrow(S10)
N10
X10 <- apply(S10, 1, mean)
P10 <- table(X10)/N10
V10 <- as.numeric(names(P10))

par(mfrow=c(1,3))
plot(V2, P2, type="h", main="n=2", xlim=c(1,20))
plot(V5, P5, type="h", main="n=5", xlim=c(1,20))
plot(V10, P10, type="h", main="n=10", xlim=c(1,20))

#(2)
EX2 <- sum(V2*P2)
EX5 <- sum(V5*P5)
EX10 <- sum(V10*P10)
EX2;EX5;EX10
VX2 <- sum(V2^2*P2)-EX2^2
VX5 <- sum(V5^2*P5)-EX5^2
VX10 <- sum(V10^2*P10)-EX10^2
VX2;VX5;VX10

#(3)
PX2 <- sum(P2[V2 >= 15])
PX5 <- sum(P5[V5 >= 15])
PX10 <- sum(P10[V10 >= 15])
PX2;PX5;PX10


#연습문제 R2(p.211)
x10 <- 0:10
x20 <- 0:20
x50 <- 0:50
par(mfrow=c(1,3))
plot(x10, dbinom(x10, 10, 0.2), type="h", main="n=10", lwd=2, col=2)
plot(x20, dbinom(x20, 20, 0.2), type="h", main="n=20", lwd=2, col=2)
plot(x50, dbinom(x50, 50, 0.2), type="h", main="n=50", lwd=2, col=2)


#연습문제 R3(p.211)
x10 <- 0:10
x20 <- 0:20
x50 <- 0:50
par(mfrow=c(1,3))
plot(x10, dhyper(x10, 40, 160, 10), type="h", 
     main="n=10, E(x)=2", lwd=2, col=2)
plot(x20, dhyper(x20, 40, 160, 20), type="h", 
     main="n=20, E(x)=4", lwd=2, col=2)
plot(x50, dhyper(x50, 40, 160, 50), type="h", 
     main="n=50, E(x)=10", lwd=2, col=2)


#연습문제 R4(p.212)
x2 <- 0:12
x5 <- 0:20
x10 <- 0:60
par(mfrow=c(1,3))
plot(x2, dpois(x2, 6), type="h", main="Poisson(6)", lwd=2, col=2)
plot(x5, dpois(x5, 15), type="h", main="Poisson(15)", lwd=2, col=2)
plot(x10, dpois(x10, 30), type="h", main="Poisson(30)", lwd=2, col=2)


#연습문제 R5(p.213)
x2 <- 2:15
x5 <- 5:75
x10 <- 10:150
par(mfrow=c(1,3))
# R의 dnbinom 함수는 성공회수가 아니라 실패회수를 사용
plot(x2, dnbinom(x2-2, 2, 0.2), type="h", 
     main="NB(2, 0.2)", lwd=2, col=2)
plot(x5, dnbinom(x5-5, 5, 0.2), type="h", 
     main="NB(5, 0.2)", lwd=2, col=2)
plot(x10, dnbinom(x10-10, 10, 0.2), type="h", 
     main="NB(10, 0.2)", lwd=2, col=2)
