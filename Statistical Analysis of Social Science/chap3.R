# 패키지 설치 시 관리자 권한으로 R 실행 필수
.libPaths("C:/Program Files/R/R-4.0.4/library")
.libPaths()

install.packages("prob")
install.packages("gplots")
install.packages("animation")
library(prob)
library(gplots)
library(animation)


#예제 3-2 (p.78)
S <- rolldie(2); S
S <- subset(S, X1 <= X2)
S <- S[order(S$X1, S$X2),]
rownames(S) <- paste0("e", 1:nrow(S))
t(S)

#사상 A (합이 짝수)
A <- subset(S, ((X1+X2) %% 2)==0); t(A)
#사상 B (합이 8 이상)
B <- subset(S, (X1+X2)>=8); t(B)
#사상 C (눈금 차이 1 이하)
C <- subset(S, abs(X1-X2)<=1); t(C)
#intersection
AB <- intersect(A,B); t(AB)
AC <- intersect(A,C); t(AC)
BC <- intersect(B,C); t(BC)
ABC <- intersect(A,B,C); t(ABC)
#complement
Bc <- setdiff(S, B); t(Bc)
#intersection
ABc <- intersect(A, Bc); t(ABc)


#예제 3-3 (p.80)
#사상 원소들 추출
Av <- rownames(A); Av
Bv <- rownames(B); Bv
Cv <- rownames(C); Cv
win.graph(7,4); par(mfrow=c(0,2,0,2))
venn(list(Av,Bv,Cv), showSetLogicLabel = T)


#예제 3-4 (p.82)
#사상 A (눈금의 차 3 이상)
A <- subset(S, abs(X1-X2)>=3); t(A)
#사상 B (눈금의 곱 20 이상)
B <- subset(S, X1*X2 >= 20); t(B)
#mutually exclusive
intersect(A,B)


#예제 3-5 (p.83)
ani.options(nmax=500, interval=0.01)
#대수의 법칙(상대도수확률 무한 시행 ~> 고전적 확률)
win.graph(7,4)
lln.ani(FUN=function(n,mu) rbinom(n, size=1, prob=mu), mu=0.5,
        type="n", col.poly="blue")
title(main="Law of Large Numbers (동전 던지기)")
grid()


#예제 3-6 (p.85)
S <- tosscoin(4)
S <- S[order(S$toss1, S$toss2, S$toss3, S$toss4),]
rownames(S) <- paste0("e", 1:nrow(S)); t(S)
counth <- function(x) sum(x=="H")
#앞면이 두 번 나오는 사상
A <- subset(S, apply(S, 1, counth)>=2); t(A)
#nrow는 행(원소)의 개수 구하는 함수
cat("P(A)=", nrow(A), "/", nrow(S), "=", nrow(A)/nrow(S), "\n")


#예제 3-7 (p.86)
S <- rolldie(4); str(S)
#숫자 합 15 이상인 사상
A <- subset(S, X1+X2+X3+X4 >= 15)
cat("P(A)=", nrow(A), "/", nrow(S), "=", nrow(A)/nrow(S), "\n")
#한 개 이상의 6이 나오는 사상
B <- subset(S, apply(S, 1, min)==6)
cat("P(B)=", nrow(B), "/", nrow(S), "=", nrow(B)/nrow(S), "\n")
#한 개 이상의 1이 나오는 사상
C <- subset(S, apply(S, 1, min)==1)
cat("P(C)=", nrow(C), "/", nrow(S), "=", nrow(C)/nrow(S), "\n")
#intersection
AB <- intersect(A,B)
cat("P(AB)=", nrow(AB), "/", nrow(S), "=", nrow(AB)/nrow(S), "\n")
AC <- intersect(A,C)
cat("P(AC)=", nrow(AC), "/", nrow(S), "=", nrow(AC)/nrow(S), "\n")
BC <- intersect(B,C)
cat("P(BC)=", nrow(BC), "/", nrow(S), "=", nrow(BC)/nrow(S), "\n")
ABC <- intersect(A,B,C)
cat("P(ABC)=", nrow(ABC), "/", nrow(S), "=", nrow(ABC)/nrow(S), "\n")
Av <- rownames(A)
Bv <- rownames(B)
Cv <- rownames(C)
#벤다이어그램
win.graph(7,4); par(mfrow=c(0,2,0,2))
venn(list(Av,Bv,Cv), showSetLogicLabel = T)


#예제 3-8 (p.88)
#Union
AuB <- union(A,B)
cat("P(AuB)=", nrow(AuB), "/", nrow(S), "=", nrow(AuB)/nrow(S), "\n")
AuC <- union(A,C)
cat("P(AuC)=", nrow(AuC), "/", nrow(S), "=", nrow(AuC)/nrow(S), "\n")
BuC <- union(B,C)
cat("P(Buc)=", nrow(BuC), "/", nrow(S), "=", nrow(BuC)/nrow(S), "\n")
AuBuC <- union(AuB,C)
cat("P(AuBuC)=", nrow(AuBuC), "/", nrow(S), "=", nrow(AuBuC)/nrow(S), "\n")


#예제 3-9 (p.89)
stra1 <- subset(S, isin(S, 1:4)); nrow(stra1)
stra2 <- subset(S, isin(S, 2:5)); nrow(stra2)
stra3 <- subset(S, isin(S, 3:6)); nrow(stra3)
NSt <- nrow(stra1)+nrow(stra2)+nrow(stra3)
cat("P(Straight)=", NSt, "/", nrow(S), "=", NSt/nrow(S), "\n")
stra <- union(union(stra1,stra2), stra3); nrow(stra)


#예제 3-10 (p.90)
#complement
Bc <- setdiff(S,B)
ABc <- intersect(A, Bc)
cat("P(ABc)=", nrow(ABc), "/", nrow(S), "=", nrow(ABc)/nrow(S), "\n")
Ac <- setdiff(S,A)
AcBc <- intersect(Ac,Bc)
cat("P(AcBc)=", nrow(AcBc), "/", nrow(S), "=", nrow(AcBc)/nrow(S), "\n")
Cc <- setdiff(S,C)
ABCc <- intersect(AB,Cc)
cat("P(ABCc)=", nrow(ABCc), "/", nrow(S), "=", nrow(ABCc)/nrow(S), "\n")
ABcC <- intersect(ABc,C)
cat("P(ABcC)=", nrow(ABcC), "/", nrow(S), "=", nrow(ABcC)/nrow(S), "\n")


#예제 3-12 (p.92)
#숫자의 합이 15 이상인 사상의 조건부 확률
cat("P(A|B)=", nrow(AB), "/", nrow(B), "=", nrow(AB)/nrow(B), "\n")
cat("P(A|C)=", nrow(AC), "/", nrow(C), "=", nrow(AC)/nrow(C), "\n")
cat("P(A|BC)=", nrow(ABC), "/", nrow(BC), "=", nrow(ABC)/nrow(BC), "\n")
ABuC <- intersect(A, BuC)
cat("P(A|BuC)=", nrow(ABuC), "/", nrow(BuC), "=", nrow(ABuC)/nrow(BuC), "\n")


#예제 3-13 (p.94)
CD <- c(paste0("C", 1:13), paste0("D", 1:13), paste0("H", 1:13), paste0("S", 1:13))
CD
options(stringsAsFactors = FALSE)
CD4 <- urnsamples(CD, size=4); str(CD4)
sameshape <- function(x) length(unique(substr(x,1,1)))==1
CD4.SS <- subset(CD4, apply(CD4, 1, sameshape)); nrow(CD4.SS)
cat("P(Flush)=", nrow(CD4.SS), "/", nrow(CD4), "=", nrow(CD4.SS)/nrow(CD4), "\n")


#예제 3-15 (p.96)
CD <- c(paste0("C", 1:13), paste0("D", 1:13), paste0("H", 1:13), paste0("S", 1:13))
CD
options(stringsAsFactors = FALSE)
CD4 <- urnsamples(CD, size=4); str(CD4)

diffshape <- function(x) length(unique(substr(x,1,1)))==4
CD4.S4 <- subset(CD4, apply(CD4, 1, diffshape)); nrow(CD4.S4)
cat("P(S4)=", nrow(CD4.S4), "/", nrow(CD4), "=", nrow(CD4.S4)/nrow(CD4), "\n")

diffnum <- function(x) length(unique(substr(x,2,3)))==4
CD4.N4 <- subset(CD4, apply(CD4, 1, diffnum)); nrow(CD4.N4)
cat("P(N4)=", nrow(CD4.N4), "/", nrow(CD4), "=", nrow(CD4.N4)/nrow(CD4), "\n")

CD4.SN4 <- intersect(CD4.S4, CD4.N4); nrow(CD4.SN4)
cat("P(S4N4)=", nrow(CD4.SN4), "/", nrow(CD4), "=", nrow(CD4.SN4)/nrow(CD4), "\n")

cat("P(S4)P(N4)=", nrow(CD4.S4), "/", nrow(CD4), "*", nrow(CD4.N4), "/", 
    nrow(CD4), "=", nrow(CD4.S4)/nrow(CD4)*nrow(CD4.N4)/nrow(CD4), "\n")

cat("P(S4|N4)=", nrow(CD4.SN4), "/", nrow(CD4.N4), "=", nrow(CD4.SN4)/nrow(CD4.N4), "\n")
cat("P(N4|S4)=", nrow(CD4.SN4), "/", nrow(CD4.S4), "=", nrow(CD4.SN4)/nrow(CD4.S4), "\n")


#예제 3-18 (p.100)
prior <- c(0.2, 0.4, 0.3, 0.1)
cond <- c(4, 2, 1, 5)/100
tot <- prior*cond; tot
sum(tot)


#예제 3-19 (p.102)
prior <- c(0.2, 0.4, 0.3, 0.1)
cond <- c(0.04, 0.02, 0.01, 0.05)
tot <- prior*cond; tot
sum(tot)
stot <- sum(tot); stot
post <- tot / stot; post
sum(post)

win.graph(7,4)
barplot(cbind(prior, post), col=c("yellow", "lightgreen", "pink", "cyan"), 
        main = "사전확률(prior) 대 사후확률(posterior)", horiz=T)

centprior <- cumsum(prior)-prior/2
centpost <- cumsum(post)-post/2

pline <- LETTERS[1:4]
text(centprior, 0.7, labels=paste0("P(", pline, ")\n", prior))
text(centpost, 1.9, labels=paste0("P(", pline, "|F)\n", format(post, digits=4)))


#연습문제 R1 (p.104)
S1 <- rolldie(4); nrow(S1)
#(2)
S2 <- urnsamples(1:6, 4, replace = T); nrow(S2)
S2 <- subset(S1, X1<=X2 & X2<=X3 & X3<=X4); nrow(S2)
#(3)
S3 <- subset(S1, apply(S1, 1, sum) >= 20); nrow(S3)
nrow(S3)/nrow(S1)
#(4)
X <- apply(S1, 1, sum)
X1 <- subset(X, X>=8 & X <=20)
tabX1 <- table(X1); tabX1
length(X1)
round(tabX1/length(X1), 4)

#연습문제 R2 (p.104)
S <- tosscoin(10); S
S <- S[order(S$toss1, S$toss2, S$toss3, S$toss4, S$toss5, 
             S$toss6, S$toss7, S$toss8, S$toss9, S$toss10),]
rownames(S) <- paste0("e", 1:nrow(S)); t(S)
counth <- function(x) sum(x=="H")
countt <- function(x) sum(x=="T")

#(1) complement of A
A <-subset(S, apply(S, 1, counth)>=3); t(A)
1 - (nrow(A)/nrow(S))
#A의 여사상이므로 1-A로 계산

#(2) intersection A and complement of B
A <-subset(S, apply(S, 1, counth)>=3); t(A)
B <- subset(S, apply(S, 1, counth)>=6); t(B)
AnB <- intersect(A,B)
result1 <- nrow(AnB)/nrow(S)
result2 <- nrow(A)/nrow(S)
result2 - result1
#A와 B의 여사상의 교집합은 A와 B의 차집합

#(3) intersection A and C
A <- subset(S, apply(S, 1, counth)>=3)
C <- subset(S, apply(S, 1, countt)>=3)
AnC <- intersect(A,C)
nrow(AnC)/nrow(S)

#(4) intersection B and C
B <- subset(S, apply(S, 1, counth)>=6)
C <- subset(S, apply(S, 1, countt)>=3)
BnC <- intersect(B,C)
nrow(BnC)/nrow(S)

#(5) intersection A and B and C
A <- subset(S, apply(S, 1, counth)>=3)
B <- subset(S, apply(S, 1, counth)>=6)
C <- subset(S, apply(S, 1, countt)>=3)
AnBnC <- intersect(A, BnC)
nrow(AnBnC)/nrow(S)


#연습문제 R3 (p.105)
S <- rolldie(4); nrow(S)

#solve 1
minN <- function(x) {
    m <- min(x)
    sum(x==m)
}
A <- subset(S, apply(S, 1, minN)==1) #가장 작은 수가 1개인 경우
nrow(A)

#solve 2
Dmin <- functiond(x) {
    m <- min(x)
    sum(x==m) > 1
}
A <- subset(S, !apply(S, 1, Dmin)) #가장 작은 수가 중복 안 되는 경우
nrow(A)
nrow(A)/nrow(S) #첫 시행에서 당첨자가 나올 확률

#연습문제 R4 (p.105)
S <- rolldie(4); nrow(S)
me <- c(2,3,4,5)

#(1)
A1 <- subset(S, apply(S, 1, sum)<sum(me))
nrow(A1)/nrow(S)
#(2)
rng <- function(x) {
    max(x) - min(x)
}
A2 <- subset(S, apply(S, 1, rng)<rng(me))
nrow(A2)/nrow(S)
#(3)
A3 <- subset(S, apply(S, 1, max)<max(me))
nrow(A3)/nrow(S)
#(4)
#이 문제 정답은 3번임

#연습문제 R5 (p.105)
balls <- c(rep("B", 3), rep("R", 7), rep("Y", 4), rep("G", 6)); balls
S <- urnsamples(balls, size=6); str(S)
countB <- function(x) sum(x=="B")
countR <- function(x) sum(x=="R")
countY <- function(x) sum(x=="Y")
countG <- function(x) sum(x=="G")

#(1)
A <- subset(S, apply(S, 1, countB)==3 & apply(S, 1, countY)==3)
nrow(A)/nrow(S)
#(2)
B <- subset(S, apply(S, 1, countR)==3 & apply(S, 1, countG)==3)
nrow(B)/nrow(S)
#(3)
c <- subset(S, apply(S, 1, countB)==1 & apply(S, 1, countR)==2 &
            apply(S, 1, countY)==1 & apply(S, 1, countG)==2)
#(4)
D <- subset(S, apply(S, 1, countB)==2 & apply(S, 1, countR) & 
            apply(S, 1, countY)==2 & apply(S, 1, countG)==1)

#연습문제 R6 (p.106)
CD <- c(paste0("C", 1:13), paste0("D", 1:13), paste0("H", 1:13), paste0("S", 1:13))
CD
options(stringsAsFactors = FALSE)

#(1) 2장의 카드가 같은 무늬 확률
CD2 <- urnsamples(CD, size = 2)
sameshape2 <- function(x) length(unique(substr(x,1,1)))==1
CD2.SS <- subset(CD2, apply(CD2, 1, sameshape2)); nrow(CD2.SS)
nrow(CD2.SS)/nrow(CD2)
#(2) 3장의 카드가 같은 무늬 확률
CD3 <- urnsamples(CD, size = 3)
sameshape3 <- function(x) length(unique(substr(x,1,1)))==1
CD3.SS <- subset(CD3, apply(CD3, 1, sameshape3)); nrow(CD3.SS)
nrow(CD3.SS)/nrow(CD3)
#(3) 4장의 카드가 같은 무늬 확률
CD4 <- urnsamples(CD, size = 4)
sameshape4 <- function(x) length(unique(substr(x,1,1)))==1
CD4.SS <- subset(CD4, apply(CD4, 1, sameshape4)); nrow(CD4.SS)
nrow(CD4.SS)/nrow(CD4)
#(4) 3장이 같은 무늬일 때, 4장이 같은 무늬일 확률
(nrow(CD3.SS) + nrow(CD4.SS))/nrow(CD4)
#(5) 2장이 같은 무늬일 때, 4장이 같은 무늬일 확률
(nrow(CD2.SS) + nrow(CD4.SS))/nrow(CD4)
#(6)

#(7)

#(8)


#연습문제 R7 (p.107)
diffshape < -function(x) {
    length(unique(substr(x,1,1)))==4
}
CD4.DS <- subset(CD4, apply(CD4, 1, diffshape))
pa <- nrow(CD4.DS)/nrow(CD4)

onein <- function(x) {
    min(as.numeric(substr(x, 2, 3)))==1
}
CD4.one <- substr(CD4, apply(CD4, 1, onein))
PB <- nrow(CD4.one)/nrow(CD4)

cd4.DSone <- subset(CD4.DS, apply(CD4.DS, 1, onein))
PAnB <- nrow(CD4.DSone)/nrow(CD4)

PA * PB == PAnB #FALSE가 나오므로 독립이 아닌 종속

#연습문제 R8 (p.107)
fprob <- function(n, p, x) {
    choose(n, x)*p^x*(1-p)^(n-x)
}
x <- 0:10
fprob(10, 0.1, x)
par(mar=c(2,2,2,2))
plot(x, fprob(10, 0.1, x), type="h", lwd=4, col=2,
     main="불량개수의 확률분포 (n=10, p=0.1)")

#연습문제 R9 (p.107)
age <- c(0.15, 0.24, 0.26, 0.20, 0.15)
support <- c(0.58, 0.52, 0.44, 0.40, 0.35)
tot <- age * support; tot
stot <- sum(tot); stot
post1 <- tot/stot; post1
sum(post1)

barplot(cbind(age, support), col=c("yellow", "lightgreen", "pink", "cyan", "blue"), 
        main="사전확률(age)과 사후확률(support)", horiz=T)
centage <- cumsum(age)-age/2
centpost <- cumsum(post1)-post1/2
#20대와 30대의 지지율이 상대적으로 다른 연령대에 비해 더 높다고 보여집니다.

#연습문제 R10 (p.108)
prior <- c(0.15, 0.35, 0.5)
conA <- c(0.88, 0.15, 0.05)
conB <- c(0.1, 0.65, 0.2)
conC <- c(0.02, 0.2, 0.75)
totA <- sum(prior*conA)
totB <- sum(prior*conB)
totc <- sum(prior*conC)
totA; totB; totC

postA <- prior*conA/totA
postB <- prior*conB/totB
postC <- prior*conC/totC
postA; postB; postC

#연습문제 R11 (p.108)
coinH <- functinod(n, m) { #동전 n번 던지고 앞면이 m번 나오는 함수
    S <- tosscoin(n)
    countH <- function(x) {
        sum(x=="H")
    }
    A <- subset(S, apply(S, 1, countH)>=m)
    p <- nrow(A)/nrow(S)
    return(p)
}

nv <- (1:10)*2 #seq(2, 20, by=2)
pv <- NULL
for (n in nv) {
    pv <- c(pv, coinH(n, n/2))
}
pv
plot(nv, pv, type="b", pch=19, col=2,
     main="동전 n개 중 앞면이 n/2개 이상 나올 확률", xlab="동전개수",
     ylab="확률")
grid(col=3) 
#동전의 개수가 늘어남에 따라 앞면이 반 이상 나올 확률은
#감소함을 확인

#연습문제 R12 (p.108)
dieS <- function(n, m) { #주사위 n번 던짐, 합 m 이상
    S <- rolldie(n)
    A <- subset(S, apply(S, 1, sum)>m)
    p <- nrow(A)/nrow(S)
    return(p)
}
nv <- 1:8
pv <- NULL
for (n in nv) {
    pv <- c(pv, dies(n, 3*n))
}
plot(nv, pv, type="b", pch=19, col=2,
     main="주사위 n개의 합이 3n 이상 나올 확률",
     xlab="주사위 개수", ylab="확률")
grid(col=3)

#연습문제 R13 (p.108)

#연습문제 R14 (p.108)

#연습문제 R15 (p.108)

