getwd()
setwd("C:\\데이터 set")
library(prob)

#예제 4-3 (p.113)
S <- rolldie(4); str(S)
X <- apply(S, 1, sum)
X.freq <- table(X); X.freq
sum(X.freq)
X.prob <- X.freq / length(X); round(X.prob, 4)

win.graph(7,5)
plot(X.prob, type="h", col="red", main="주사위 4개 눈의 합 확률분포",
     lwd=4, ylim=c(0, max(X.prob)+0.01))
text(4:24, X.prob, labels=X.freq, pos=3, col=4)
text(22, 0.1, labels="f(x)=freq/1296")

#rolldie.sum 함수 정의하기
source("C:\\데이터 set\\rolldie_sum.txt")
rolldie.sum(4)
rolldie.sum(5)
rolldie.sum(6)


#예제 4-4 (p.115)
npop <- 50; nsamp <- 10; ndef <- 8

denom <- choose(npop, nsamp)
freq <- choose(ndef, 0:nsamp) * choose(npop-ndef, nsamp-(0:nsamp)); freq
fx <- freq / denom; fx

win.graph(7,5)
plot(0:10, fx, type="h", col="red", lwd=4, xlim=c(-1,11), 
     ylim=c(0, max(fx)+0.05), main="(50개 중 8개 불량)에서 10개 추출 확률분포", 
     xlab="불량개수", ylab="f(x)")

text(0:10, fx, labels=round(fx,4), pos=3, cex=0.8, col=4)


#예제 4-6 (p.118)
Fx <- function(x) {
  if (x<0) {y <- 0
  } else if (x<1) {y <- 1/8
  } else if (x<2) {y <- 1/2
  } else if (x<3) {y <- 7/8
  } else y <- 1
  return(y)    }
VFx <- Vectorize(Fx, "x")
xrange <- (-200:500)/100

win.graph(7,5)
plot(xrange, VFx(xrange), cex=0.6, main="동전 3개 중 뒷면의 개수 CDF",
     col=2, xlab="x", ylab="F(x)")
points(0:3, VFx(0:3), pch=19, col=2, cex=1.2)
points(0:3, VFx(0:3-0.001), col=2, cex=1.2)

grid(col=3)
text(0:3, VFx(0:3), labels=VFx(0:3), col=4, pos=2)


#예제 4-7 (p.119)
Fx <- function(x) {
  if (x<0) {y <- 0
  } else {y <- 1-exp(-2*x)}
  return(y) }
VFx <- Vectorize(Fx, "x")
xrange <- (-100:300)/100

win.graph(7,5)
plot(xrange, VFx(xrange), type="l", lwd=3, main="연속형 누적확률분포함수 예",
     col=2, xlab="x", ylab="F(x)")

grid(col=3)
segments(-1, Fx(1), 1, Fx(1), lty=2, col=4)
segments(1, 0, 1, Fx(1), lty=2, col=4)
text(-0.7, Fx(1), labels=paste0("F(1)=", round(Fx(1),4)), col=4, pos=3)


#예제 4-9 (p.122)
S <- rolldie(4); str(S)
X <- apply(S, 1, max); table(X)
Y <- apply(S, 1, min); table(Y)

tabXY <- table(X, Y)
mtabXY <- addmargins(tabXY); mtabXY

ptabXY <- mtabXY/nrow(S); round(ptabXY,5)

freqXY <- matrix(0, 6, 6)
nd <- rep(0, 6)
nd[1] <- 1
for(k in 2:6) nd[k] <- k^4 - (k-1)^4 - ((k-1)^4 - (k-2)^4)
for(k in 1:6) for (m in 1:k) freqXY[k,m] <- nd[k-m+1]
print(freqXY - tabXY)


#예제 4-10 (p.125)
ex10 <- function(a, b, x, y) 2/(a+b)*(a*x+b*y)
ex10.int <- function(a, b, x1, x2, y1, y2) {
  integrate(function(y) {
    sapply(y, function(y) {
      integrate(function(x) {
        sapply(x, function(x) ex10(a,b,x,y)) }, x1, x2)$value
    })
  }, y1, y2) }

ex10.int(2, 5, 0,1, 0,1)
ex10.int(5, 10, 0,1, 0,1)
ex10.int(2,5, 0, 0.5, 0, 0.5)
ex10.int(5, 15, 0, 0.5, 0, 0.5)


#예제 4-17 (p.130)
S <- rolldie(4); str(S)
sum3 <- function(x) sum(x>=3)
X <- apply(S, 1, sum3); table(X)
even <- function(x) sum(x %% 2 == 0)
Y <- apply(S, 1, even); table(Y)

tabXY <- table(X, Y)
mtabXY <- addmargins(tabXY); mtabXY
ptabXY <- mtabXY/nrow(S); round(ptabXY,5)

fx <- ptabXY[-6, 6]; fx
fy <- ptabXY[6, -6]; fy
fxy <- ptabXY[-6, -6]; fxy
fx %o% fy - fxy
max(abs(fx %o% fy - fxy))

#예제 4-23 (p. 137)
fx <- function(x) {if (x<0|x>1) 0 else 2*x}
fxv <- Vectorize(fx, "x")

fyv <- function(y) fxv((y+4)/10)/10
fwv <- function(w) fxv((-w+4)/10)/10

px <- integrate(fxv, 0.3, 0.7)
py <- integrate(fyv, -1, 3)
pw <- integrate(fwv, -3, 1)
px[[1]]; py[[1]]; pw[[1]]

par(mfrow=c(3,1))

curve(fxv, -0.2, 1.2, type="n", ylab="f(x)", main="pdf of X")
c <- 0.3; d <- 0.7
cord.x <- c(c, seq(c, d, 0.01), d)
cord.y <- c(0, fxv(seq(c, d, 0.01)), 0)
polygon(cord.x, cord.y, col="lightcyan")
text(0.5, 0.4, cex=1.3, labels=paste0("P(", c, "<X<", d, ")\n=", round(px[[1]], 2)))
lines(-40:240/200, fxv(-40:240/200), lwd=2, col=2)

curve(fyv, -6, 8, type="n", ylab="f(y)", main="pdf of Y")
c <- -1; d <- 3
cord.x <- c(c, seq(c, d, 0.01), d)
cord.y <- c(0, fyv(seq(c, d, 0.01)), 0)
polygon(cord.x, cord.y, col="lightcyan")
text(1, 0.04, cex=1.3, labels=paste0("P(", c, "<Y<", d, ")\n=", round(py[[1]],2)))
lines(-1200:1600/200, fyv(-1200:1600/200), lwd=2, col=2)

curve(fwv, -8, 6, type="n", ylab="f(w)", main="pdf of W")
c <- -3; d <- 1
cord.x <- c(c, seq(c, d, 0.01), d)
cord.y <- c(0, fwv(seq(c, d, 0.01)), 0)
polygon(cord.x, cord.y, col="lightcyan")
text(-1, 0.04, cex=1.3, labels=paste0("P(", c, "<W<", d, ")\n=", round(pw[[1]],2)))
lines(-1600:1200/200, fyv(-1600:1200/200), lwd=2, col=2)


par(mfrow=c(1,1))

#연습문제 R1 (p.141)
umbrella <- LETTERS[1:8]
s1 <- urnsamples(umbrella,size = 8,ordered=T); str(S)
str(s1) 
options(stringAsFactors = F)
mans <- function(x){
  sum(x==umbrella)
}
X1 <- apply(s,1,mans)
X1.freq <- table(X1)
sum(X1.freq)
X1.prob <- X1.freq / length(X1); round(X1.prob, 4)

plot(X1.prob, type="h", col="red", main="8개의 우산 확률분포",
     lwd=4, ylim=c(0, max(X1.prob)+0.03))


#연습문제 R2 (p.143)
S2 <- rolldie(4); str(S2)

even_num <- function(x) {sum(S2 %% 2 == 0)}
over_3 <- function(x) {S2 >= 3}
over_4 <- function(x) {S2 >= 4}

#(1) X: 눈의 최대치 Y: 짝수 눈의 개수
X21 <- apply(S2, 1, max); table(X21)
Y21 <- apply(S2, 1, even_num); table(Y21)

tabX21Y21 <- table(X21, Y21)
mtabX21Y21 <- addmargins(tabX21Y21); mtabX21Y21

ptabX21Y21 <- mtabX21Y21/nrow(S2); round(ptabX21Y21, 5)

freqX21Y21 <- matrix(0, 6, 6)
nd21 <- rep(0, 6)
nd21[1] <- 1
for (k in 2:6) nd21[k] <- k^4 - (k-1)^4 - ((k-1)^4 - (k-2)^4)
for (k in 1:6) for (m in 1:k) freqX21Y21[k, m] <- nd21[k-m+1]
print(freqX21Y21 - tabX21Y21)

#(2) X:눈의 최소치 Y: 짝수 눈의 개수
X22 <- apply(S2, 1, min); table(X22)
Y22 <- apply(S2, 1, even_num); table(Y22)

tabX22Y22 <- table(X22, Y22)
mtabX22Y22 <- addmargins(tabX22Y22); mtabX22Y22

ptabX22Y22 <- mtabX22Y22/nrow(S2); round(ptabX22Y22, 5)

freqX22Y22 <- matrix(0, 6, 6)
nd22 <- rep(0, 6)
nd22[1] <- 1
for (k in 2:6) nd22[k] <- k^4 - (k-1)^4 - ((k-1)^4 - (k-2)^4)
for (k in 1:6) for (m in 1:k) freqX22Y22[k, m] <- nd22[k-m+1]
print(freqX22Y22 - tabX22Y22)

#(3) X:3 이상 눈의 개수 Y: 짝수 눈의 개수
X23 <- apply(S2, 1, over_3); table(X23)
Y23 <- apply(S2, 1, even_num); table(Y23)

tabX23Y23 <- table(X23, Y23)
mtabX23Y23 <- addmargins(tabX23Y23); mtabX23Y23

ptabX23Y23 <- mtabX23Y23/nrow(S2); round(ptabX23Y23, 5)

freqX23Y23 <- matrix(0, 6, 6)
nd23 <- rep(0, 6)
nd23[1] <- 1
for (k in 2:6) nd23[k] <- k^4 - (k-1)^4 - ((k-1)^4 - (k-2)^4)
for (k in 1:6) for (m in 1:k) freqX23Y23[k, m] <- nd23[k-m+1]
print(freqX23Y23 - tabX23Y23)

#(4) X:4 이상 눈의 개수 Y: 짝수 눈의 개수
X24 <- apply(S2, 1, over_4); table(X24)
Y24 <- apply(S2, 1, even_num); table(Y24)

tabX24Y24 <- table(X24, Y24)
mtabX24Y24 <- addmargins(tabX24Y24); mtabX24Y24

ptabX24Y24 <- mtabX24Y24/nrow(S2); round(ptabX24Y24, 5)

freqX24Y24 <- matrix(0, 6, 6)
nd24 <- rep(0, 6)
nd24[1] <- 1
for (k in 2:6) nd24[k] <- k^4 - (k-1)^4 - ((k-1)^4 - (k-2)^4)
for (k in 1:6) for (m in 1:k) freqX24Y24[k, m] <- nd24[k-m+1]
print(freqX24Y24 - tabX24Y24)
