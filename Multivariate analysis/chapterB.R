getwd()
setwd('C:\\Users\\jkds5\\OneDrive\\바탕 화면\\2022-1\\다변량자료분석1\\data')


# ex B.1
a <- -4.7
b <- c(1,2,3,4,5,6,7,8,9) # 벡터 생성: c 함수
M <- matrix(b, nrow=3, ncol=3) # 벡터를 행렬로 바꾸기: matrix 함수
print(M)

v <- c(M) # 행렬을 벡터로 바꾸기: c 함수
print(v)

student <- read.csv('student.csv', header=T)
student.x <- as.matrix(student[-1]) # 데이터 프레임을 행렬로 바꾸기
print(student.x)

dim(student.x) # 행렬의 차수: dim 함수
attributes(student.x) # 행렬의 차수, 행 이름, 열 이름: attributes 함수


# ex B.2
A <- matrix(c(2,5,7,9,3,4), nrow=3, ncol=2)
print(A)

B <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2)
print(B)

B.t <- t(B) # 행렬의 전치: t 함수
print(B.t)

c <- A+B # 두 행렬의 합
print(C)

D <- A-B # 두 행렬의 차
print(D)

E <- 4*A # 스칼라와 행렬의 곱
print(E)

f <- A%*%B.t # 두 행렬의 곱: %*% 연산자
print(f)

x <- c(1,2,3,4,5)
y <- c(3,2,4,1,6)
xy <- t(x)%*%y # 두 벡터의 스칼라곱
print(xy)

xy.u <- sqrt(t(x-y)%*%(x-y)) # 두 벡터 사이의 유클리드 거리
print(xy.u)

G <- A*B # 두 행렬의 동일한 위치의 원소들의 산술 곱
print(G)

L <- cbind(A,B) # 두 행렬의 가로 결합: cbind 함수
print(L)

R <- rbind(A,B) # 두 행렬의 세로 결합: rbind 함수
print(R)


# ex B.3
A <- diag(3) # 단위행렬 생성: diag 함수
print(A)

v <- c(1,2,3)
v.D <- diag(v) # 벡터를 대각행렬로 바꾸기: diag 함수
print(v.D)

M <- matrix(c(1,2,3,4,5,6,7,8,9), nrow=3) # M <- matrix(1:9, nrow=3)
print(M)

M.v <- diag(M) # 행렬의 대각원소로 벡터 생성: diag 함수
print(M.v)

M.D <- diag(diag(M))
print(M.D)

one <- rep(1,9) # 모든 원소가 동일한 벡터: rep 함수
print(one)

J3 <- matrix(rep(1,9), nrow=3, ncol=3) # 모든 원소가 동일한 행렬
print(J3)

M.L <- M[c(1,3),] # 행 추출, 참조: M[1:3,]
print(M.L)

M.R <- M[,c(1,3)] # 열 추출, 참조: M[,1:3]
print(M.R)

M.K <- M[c(1,3),c(1,3)] # 행/열 추출
print(M.K)


# ex B.4
A <- matrix(c(1,4,6,4,2,5,6,5,3), nrow=3)

sum(diag(A)) # 고유화: sum, diag 함수
# install.packages('matrixcalc')
library(matrixcalc)
matrix.trace(A) # 고유화: matrixcalc::matrix.trace 함수

det(A) # 행렬식: det 함수

matrix.rank(A) # 계수: matrixcalc::matrix.rank 함수

solve(A) # 역행렬: solve 함수
matrix.inverse(A) # 역행렬: matrixcalc::matrix.inverse 함수

library(MASS)
ginv(A) # 일반화 역행렬: MASS::ginv 함수

# ex B.5
satis <- read.csv('satis.csv', header=T)
satis.X <- satis[c('x1','x2','x3','x4','x5')]

satis.S <- cov(satis.X) # 분산-공분산 행렬: cov 함수
print(satis.S)

satis.ED <- eigen(satis.S) # 고유값 분해
print(satis.ED$values) # 고유값 출력

print(satis.ED$vectors) # 고유벡터 출력

satis.C <- scale(satis.X, center=T, scale=F)/sqrt(9) # C'C=S
print(satis.C)

satis.SVD <- svd(satis.C) # 특이값 분해
print(satis.SVD$d) # 특이값 출력 (주성분의 표준편차)

print(satis.SVD$u) # U 행렬 출력

print(satis.SVD$v) # V 행렬 출력 (고유벡터)

satis.SVD$u%*%diag(satis.SVD$d)*sqrt(9) # 참조: 주성분점수


# 고유값 분해와 특이값 분해
# install.packages('OpenImageR')
library(OpenImageR)
x <- readImage('pansy.jpg')
dim(x) # 768 1024 3
imageShow(x)
r <- rgb_2gray(x) # 컬러 이미지에서 흑백사진으로 변경. 차원축소
dim(r)
# plot(0, type='n', axes=F, xlab='', ylab='')
imageShow(r)

## SVD 적용
x.svd <- svd(r)
d <- diag(x.svd$d)
dim(d) # 768 768
u <- x.svd$u
v <- x.svd$v
plot(1:length(x.svd$d), x.svd$d) # y축은 특이값의 크기

## SVD 근사
# k개의 특이값을 이용한 근사
depth <- 50 # k=20, 30, 50, 100, 150에 대해서 바꿔가며 하기 가능
us <- as.matrix(u[, 1:depth])
vs <- as.matrix(v[, 1:depth])
ds <- as.matrix(d[1:depth, 1:depth])
ls <- us %*% ds %*% t(vs)
imageShow(ls)
dim(us);dim(vs);dim(ds)
dim(us)[1]*dim(us)[2]+dim(vs)[1]*dim(vs)[2]+dim(ds)[1]*dim(ds)[2]
# 첫 회색그림은 768*1024=786432 메모리 필요
# but, k=50인 SVD그림 사용시 92100 필요
# 92100/786432*100
# 11.71%의 메모리만 사용하는 이미지 생성

# RGB 칼라이미지로 SVD 그림 생성
depth <- 50 # k=50으로 근사
for (i in 1:3) {
  x.svd <- svd(x[,,i])
  d <- diag(x.svd$d)
  u <- x.svd$u
  v <- x.svd$v
  us <- as.matrix(u[, 1:depth])
  vs <- as.matrix(v[, 1:depth])
  ds <- as.matrix(d[1:depth, 1:depth])
  assign(paste('ls', i, sep=''), us %*% ds %*% t(vs))
  # assign a valueto a same
}
ls <- array(c(ls1, ls2, ls3), c(nrow(ls1), ncol(ls1), 3))
plot(0, type='n', axes=F, xlab='', ylab='')
imageShow(ls)