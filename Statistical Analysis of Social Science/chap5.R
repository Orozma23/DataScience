install.packages("pracma")

#예제 5-2(p.147)
xfx <- function(x) {
  x*2*exp(-2*x)
}
integrate(xfx, 0, Inf)$value


#예제 5-4(p.148)
yfx <- function(x) {
  (3*x-3)*2*exp(-2*x)
}
integrate(yfx, 0, Inf)$value


#예제 5-5(p.148)
library(prob)
S <- rolldie(2)
X <- apply(S, 1, max)
Y <- apply(S, 1, min)
pXY <- table(X, Y)/nrow(S)
XY <- 1:6 %o% 1:6
XY * pXY
sum(XY * pXY)


#예제 5-6(p.150)
library(pracma)
zfxy <- function(x,y) {
  x*y*1/2*(x+3*y)
}
integral2(zfxy, xmin=0, xmax=1, ymin=0, ymax=1)$Q


#예제 5-8(p.152)
x2fx <- function(x) {
  x^2*2*exp(-2*x)
}
Ex2 <- integrate(x2fx, 0, Inf)$value
Ex <- integrate(xfx, 0, Inf)$value
Ex
Ex2-Ex^2


#예제 5-15(p.159)
S <- rolldie(4)
N <- nrow(S); N
X <- vector("list", 4)
X[[1]] <- apply(S, 1, sum)
X[[3]] <- apply(S, 1, max)
X[[4]] <- apply(S, 1, min)
X[[2]] <- abs(X[[4]] - X[[3]])

Xf <- vector("list", 4)
for (i in 1:4) {
  Xf[[i]] <- table(X[[i]])
}
Xv <- vector("list", 4)
for (i in 1:4) {
  Xv[[i]] <- as.numeric(names(Xf[[i]]))
}
Xp <- vecotr("list", 4)
for (i in 1:4) {
  Xp[[i]] <- Xf[[i]]/N
}

Ex <- vector("list", 4)
for (i in 1:4) {
  Ex[[i]] <- sum(Xv[[i]]*Xp[[i]])
}
Ex
Vx <- vector("list", 4)
for (i in 1:4) {
  Vx[[i]] <- sum(Xv[[i]]^2*Xp[[i]])-Ex[[i]]^2
}
XYp <- vector("list", 6)
k <- 1
for (i in 1:3) {
  for (j in 1:3) {
    XYp[[k]] <- table(X[[i]], X[[j]])/N
    k <- k+1
  }
}
COVxy <- vector("list", 6)
k <- 1
for (i in 1:3) {
  for (j in (i+1):4) {
    COVxy[[k]] <- sum(Xv[[i]]%o%Xv[[j]] * XYp[[k]]) - Ex[[i]]*Ex[[j]]
    k <- k+1
  }
}
CORRxy <- vector("list", 6)
k <- 1
for (i in 1:3) {
  for (j in (i+1):4) {
    CORRxy[[k]] <- COVxy[[k]]/sqrt(Vx[[i]])/sqrt(Vx[[j]])
    k <- k+1
  }
}
CORRxy


#연습문제 R1(p.169)
umbrella <- LETTERS[1:8]
s1 <- urnsamples(umbrella,size = 8,ordered=T)
str(s1)
options(stringAsFactors = F)
mans <- function(x){
  sum(x==umbrella)
}
X1 <- apply(s1,1,mans)
Xp <- table(X1)/nrow(s1)
round(Xp, 6)
Xv <- as.numeric(names(Xp))
Ex <- sum(Xv*Xp)
Ex
Vx <- sum(Xv^2*Xp)-Ex^2
Vx


#연습문제 R2(p.170)
s2 <- rolldie(3)
n2 <- nrow(s2)

#(1)
x1 <- apply(s2 ,1, sum)
xp1 <- table(x1)/n2
xv1 <- as.numeric(names(xp1))
ex1 <- sum(xv1*xp1)
ex1*100-1000
#(2)
?prod
prod(1,2,3) #prod는 모든 원소들을 곱해주는 함수
x2 <- apply(s2, 1, prod)
#product <- function(x) {x[1]*x[2]*x[3]} //사용자 지정함수
#x2 <- apply(s2,1,product)
xp2 <- table(x2)/n2
xv2 <- as.numeric(names(xp2))
ex2 <- sum(xv2*xp2)
ex2*100-4000
#(3)
rng <- function(x) {max(x) - min(x)}
x3 <- apply(s2, 1, rng)
xp3 <- table(x3)/n2
xv3 <- as.numeric(names(xp3))
ex3 <- sum(xv3*xp3)
ex3*100-300
#(4)
#같은 값인 경우면 같은 값끼리 곱
#총 3개라 중복되는 값은 최대 1가지만 가능
sameprod <- function(x) {
  if(x[1]==x[2] & x[2]==x[3]) {result <- prod(x)}
  else if(x[1]==x[2] | x[1]==x[3]) {result <- x[1]^2}
  else if(x[2]==x[3]) {result <- x[2]^2}
  else {result <- 0}
  return(result)
}
x4 <- apply(s2, 1, sameprod)
xp4 <- table(x4)/n2
xv4 <- as.numeric(names(xp4))
ex4 <- sum(xv4*xp4)
ex4*100-800
#(5)
rprod <- function(x) {max(x)*min(x)}
x5 <- apply(s2, 1, rprod)
xp5 <- table(x5)/n2
xv5 <- as.numeric(names(xp5))
ex5 <- sum(xv5*xp5)
ex5*100-1000


#연습문제 R3(p.170)
s3 <- rolldie(5)
n3 <- nrow(s3)
X <- vector("list", 4)

X[[1]] <- apply(s3, 1, sum)
X[[3]] <- apply(s3, 1, max)
X[[4]] <- apply(s3, 1, min)
X[[2]] <- abs(X[[3]] - X[[4]])

Xf <- vector("list", 5)
for (i in 1:4) {
  Xf[[i]] <- table(X[[i]])
}
Xv <- vector("list", 5)
for (i in 1:5) {
  Xv[[i]] <- as.numeric(names(Xf[[i]]))
}
Xp <- vector("list", 5)
for (i in 1:5) {
  Xp[[i]] <- Xf[[i]]/n3
}

Ex <- vector("list", 5)
for (i in 1:5) {
  Ex[[i]] <- sum(Xv[[i]]*Xp[[i]])
}
Ex

Vx <- vector("list", 5)
for (i in 1:5) {
  Vx[[i]] <- sum(Xv[[i]]^2*Xp[[i]])-Ex[[i]]^2
}
Vx

XYp <- vector("list", 10)
k <- 1
for (i in 1:5) {
  for (j in 1:5) {
    XYp[[k]] <- table(X[[i]], X[[j]])/n3
    k <- k+1
  }
}

COVxy <- vector("list", 10)
k <- 1
for (i in 1:5) {
  for (j in (i+1):6) {
    COVxy[[k]] <- sum(Xv[[i]]%o%Xv[[j]] * XYp[[k]]) - Ex[[i]]*Ex[[j]]
    k <- k+1
  }
}
COVxy

CORRxy <- vector("list", 10)
k <- 1
for (i in 1:5) {
  for (j in (i+1):6) {
    CORRxy[[k]] <- COVxy[[k]]/sqrt(Vx[[i]])/sqrt(Vx[[j]])
    k <- k+1
  }
}
CORRxy
