#예제 2-5 (p.51)
p <- (1:199)/200
x <- qnorm(p, mean=10, sd=1)
hist(x)
xcut <- seq(from=7.25, to=12.75, by=0.5)
hist(x, breaks=xcut)
hist(x, breaks=xcut, main="N(0,1) 히스토그램", xlab="이상적 관측값", ylab="도수",
     col="skyblue")
hist(x, breaks=xcut, prob=T, col="skyblue")
lines(density(x), col=2)

par(mfrow=c(2,2))
hist(x)
hist(x, breaks=xcut)
hist(x, breaks=xcut, main="N(0,1) 히스토그램", xlab="이상적 관측값", ylab="도수",
     col="skyblue")
hist(x, breaks=xcut, prob=T, col="skyblue")
lines(density(x), lwd=2, col=2)

#예제 2-6 (p.53)
p1 <- (1:100)/181
p2 <- (1:100)/101
p3 <- (1:20)/21

x1 <- qnorm(p1, mean=10, sd=1)
x2 <- qnorm(p2, mean=10, sd=1)

y1 <- qnorm(p2, mean=6, sd=1)
y2 <- qnorm(p3, mean=6, sd=0.5)

da <- c(x1, x2)
db <- c(x2, y1)
dc <- x1[(x1>9) | (x1<8)]
dd <- x1[x1>=9.0]

par(mfrow=c(2,2))
hist(da, breaks=15, main="낙도형", xlab="(a)", col="cyan")
hist(db, breaks=15, main="쌍봉우리형", xlab="(b)", col="cyan")
hist(dc, breaks=12, main="이빠진형", xlab="(c)", col="cyan")
hist(dd, breaks=12, main="절벽형", xlab="(d)", col="cyan")

#예제 2-7 (p.56)
rm(list=ls())
setwd("C:\\데이터 set")
data21 <- read.csv("tab2-1.csv", header=F)
x <- as.matrix(data21)
xcut <- seq(from=4.575, by=0.05, length.out=16)
win.graph(7, 5)
hist(x, breaks=xcut, main="저항 데이터의 히스토그램", ylab="밀도",
     xlab="저항값", prob=T, col="cyan")
lines(density(x), lty=2, lwd=2, col=2)

#예제 2-8 (p.57)
stem(x)

#예제 2-9 (p.59)
x2 <- matrix(x, ncol=1)
win.graph(7,4)
boxplot(x2, horizontal=T, main="저항 데이터의 상자그림", col="cyan")
points(x2, rep(1,100))
xfn <- fivenum(x2); xfn
text(xfn, 0.65, labels=xfn, pos=3)
text(5.2, 1.3, labels=paste0("평균=", mean(x2), "\n표준편차=", round(sd(x2),4),
     "\n표본개수=", length(x)), col=4)

#예제 2-10 (p.60)
win.graph(7,5)
boxplot(x, main="저항데이터의 열별 상자그림", boxwex=0.5, ylab="저항", col=7)
grid(col=3)
points(rep(1:10, each=10), x, pch=19, col=2)
xstat <- apply(x, 2, fivenum)
text(rep(1:10, each=5), xstat, labels=xstat, col=4, cex=0.8, pos=4)

#예제 2-11 (p.63)
str(mtcars)
attach(mtcars)
win.graph(7,5)
plot(wt, mpg, main="자동차 중량 대 연비 산점도", xlab="자동차 중량",
     ylab="연비(mpg)", pch=19, cex=1.2)
grid(col=3)
sr <- lm(mpg~wt)
abline(sr, lty=2, lwd=2, col="red")
text(4, 27, labels=paste0("Y=", round(sr$coef[[1]], 4), "(+)",
     round(sr$coef[[2]], 4), " * X"), col=2, cex=1.2)

#예제 2-12 (p.64)
pairs(mtcars[c(1,2,4,6)])

pairs(~mpg+cyl+hp+wt, dat=mtcars, main="자동차 특성치 산점행렬도",
      panel=function(x,y) { points(x,y, pch=19, col=4)
      abline(lm(y~x), col=2) } )

#예제 2-14 (p.67)
setwd("C:\\데이터 set")
data21 <- read.csv("tab2-1.csv", header=F)

x <- as.vector(matrix(as.matrix(data21), ncol=1))
xmean <- mean(x)
xmed <- median(x)
tabx <- table(x)
xmode <- as.numeric(names(tabx[tabx==max(tabx)]))
cat("평균=", xmean, "중앙값=", xmed, "최빈값=", xmode, "\n")

gm_mean <- function(a) {prod(a)^(1/length(a))}
gmean <- gm_mean(x)
hmean <- 1/mean(1/x)
tmean <- mean(x, trim=0.1)
cat("기하평균=", gmean, "조화평균=", hmean, "절사평균(10%)=", tmean, "\n")

#예제 2-16 (p.70)
xvar <- var(x)
xsd1 <- sd(x)
xsd2 <- sqrt(xvar)
xrng <- max(x) - min(x)
xiqr <- quantile(x, 0.75) - quantile(x, 0.25)
xcv <- xsd1/xmean
cat("분산=", xvar, "표준편차=", xsd1, "범위=", xrng, "IQR=", xiqr, "CoV=", xcv,
    "\n")
summary(x)
