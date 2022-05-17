getwd()
setwd('C:\\Users\\jkds5\\OneDrive\\바탕 화면\\2022-1\\다변량자료분석1\\data')


# ex 1.1
student <- read.csv('student.csv', header=TRUE)

head(student, n=3)
?head
tail(student)

student.X <- student[-1]
summary(student.X)
cov(student.X)
cor(student.X)

# ex 1.2
student.C <- scale(student.X, center=TRUE, scale=FALSE) # 중심화
print(student.C)
student.Z <- scale(student.X, center=TRUE, scale=TRUE) # 표준화
print(student.Z)


# ex 1.3
?plot
plot(x2~x1, data=student, pch='*', cex=3, col='blue')
plot(student$x1, student$x2, pch=8, cex=2, col='blue')


# ex 1.4
par(mfrow=c(1,2))
student.ox1 <- student[order(student$x1),] # order는 인덱싱, 정렬
plot(x2~x1, data=student, pch='*', cex=3, col='blue')
plot(x2~x1, data=student.ox1, cex=2, col='red', type='o', lty=4, lwd=2)


# ex 1.5 한 화면에 여러개의 그래프를 표현하기
par(mfrow=c(1,2))
plot(x4~x3, data=student, pch=20, cex=2, xlim=c(0,100), ylim=c(0,100))
plot(x4~x5, data=student, pch=20, cex=2, xlim=c(0,100), ylim=c(0,100))


# ex 1.6 산점도행렬
par(mfrow=c(1,1))
plot(student.X, pch=20, cex=2, xlim=c(0,100), ylim=c(0,100), cex.labels=1.2)
pairs(student.X, cex=2, xlim=c(0,100), ylim=c(0,100), cex.labels=1.2)
# install.packages('car')
library(car)
?scatterplotMatrix
scatterplotMatrix(student.X, ellipse=TRUE, smoth=TRUE)


# ex 1.7 별도표
stars(student.X, full=TRUE, scale=TRUE, radius=TRUE, draw.segments=FALSE,
      frame.plot=TRUE, labels=rownames(student.X), nrow=3, ncol=5,
      cex=0.8, len=0.8, lwd=1, axes=TRUE, ylim=c(1,8), key.loc=c(7,2))


# ex 1.8
# install.packages('plotrix')
library(plotrix)
radial.plot(student.X, rp.type='p', radial.lim=c(0,100),
            lwd=2, lty=1, labels=names(student.X),
            show.grid=TRUE, show.radial.grid=TRUE, show.grid.labels=1)


# ex 1.9
par(mfrow=c(2,5))
for (i in 1:nrow(student.X)){
  radial.plot(student.X[i,], rp.type='rp', radial.lim=c(0,100),
              lty=1, lwd=2, labels=names(student.X),
              show.grid=TRUE, show.radial.grid=TRUE, show.grid.labels=FALSE,
              rad.col='gray', line.col='red', grid.col='gray',
              main=rownames(student.X)[i])
}


# ex 1.10
par(mfrow=c(1,1))
library(plotrix)
student.part <- student.X[c(2,5,6,9,10),]
ladderplot(student.part, scale=FALSE, lty=1, pch=1:5, col=1:5,
           ylim=c(0,100), vertical=TRUE)
legend('topright', pch=1:5, col=1:5, c('2','5','6','9','10'))


# ex 1.11 체르노프 얼굴 그림
# install.packages('aplpack')
library(aplpack)
faces(student.X, face.type=0, nrow.plot=2, ncol.plot=5, scale=TRUE, cex=1.5)


# ex 1.12 표준화
student.X.scale <- as.data.frame(lapply(student.X, FUN=function(x)
  (x-min(x))/(max(x)-min(x))))
# install.packages('scales')
library(scales)
student.X.scale <- as.data.frame(lapply(student.X, rescale))
student.X.scale


########################################################################


# practice 1
x1 <- matrix(c(2,4,12,5,8,4,18,3), nrow=4, ncol=2, byrow=T)
x1

bar_1 <- mean(x1[,1])
bar_2 <- mean(x1[,2])

mean_vec <- c(bar_1, bar_2)
mean_vec

s <- cov(x1)
s

r <- cor(x1)
r


# practice 2
x2 <- matrix(c(3,33,73,8,12,
              3,30,59,28,20,
              35,83,91,32,34,
              35,83,85,33,32,
              15,40,55,68,52,
              3,53,76,10,8,
              68,83,85,48,50,
              15,47,77,76,76,
              46,60,83,83,68,
              98,83,91,80,72), nrow=10, ncol=5, byrow=T)
x2

par(mfrow=c(1,1))
pairs(x2)

bar_1 <- mean(x2[,1])
bar_2 <- mean(x2[,2])
bar_3 <- mean(x2[,3])
bar_4 <- mean(x2[,4])
bar_5 <- mean(x2[,5])

mean_vec <- c(bar_1, bar_2, bar_3, bar_4, bar_5)
mean_vec

s <- cov(x2)
s

r <- cor(x2)
r


# practice 3
dataset <- read.csv('company.csv')
company <- dataset[,-c(1,10)]
company

# (a)
par(mfrow=c(1,1))
pairs(company)

# (b)
bar_1 <- mean(company[,1])
bar_2 <- mean(company[,2])
bar_3 <- mean(company[,3])
bar_4 <- mean(company[,4])
bar_5 <- mean(company[,5])
bar_6 <- mean(company[,6])
bar_7 <- mean(company[,7])
bar_8 <- mean(company[,8])

mean_vec <- c(bar_1, bar_2, bar_3, bar_4, bar_5, bar_6, bar_7, bar_8)
mean_vec

s <- cov(company)
s

r <- cor(company)
r


# practice 4

# (a)
company_standardization <- scale(company, center=T, scale=F)
company_standardization

company_generalization <- scale(company, center=T, scale=T)
company_generalization


# (b)
sx <- cor(company_generalization)
sx


# (c)
company_generalization_2 <- cbind(company_generalization, dataset[,10])
company_generalization_2

colnames(company_generalization_2) <- c('x1', 'x2', 'x3', 'x4', 'x5', 'x6',
                                        'x7', 'x8', 'class')
company_generalization_2

mean_vec <- aggregate(company_generalization_2,
                      list(company_generalization_2[,9]), mean)
rowMeans(mean_vec[,-c(1,9)])

std_vec <- aggregate(company_generalization_2,
                     list(company_generalization_2[,9]), sd)
apply(std_vec[,-c(1,9)], 1, sd)

# (d)
library(plotrix)
comp_mean_vec <- mean_vec[,-c(1,10)]
par(mfrow=c(1,1))
ladderplot(comp_mean_vec, scale=F, pch=1:3, col=1:3, vertical=T)
legend('topleft', pch=1:3, col=1:3, c('1', '2', '3'))

# (e)
company_standardization_2 <- as.data.frame(lapply(company, FUN=function(x)
                             (x-min(x))/(max(x)-min(x))))
company_standardization_2

# (f)
par(mfrow=c(1,1))
stars(company_standardization_2, full=T, scale=T, radius=T, draw.segments=T,
      frame.plot=T, labels=rownames(company_standardization_2), nrow=8, ncol=6,
      axes=T, key.loc=c(13,11))

win.graph(20,10)
par(mfrow=c(4,7))
for(i in 1:nrow(company_standardization_2)) {
  radial.plot(company_standardization_2[i,], rp.type='rp', show.grid=T,
              show.radial.grid=T, show.grid.labels=F, rad.col='gray',
              line.col='blue', grid.col='gray')
}


###########################################################################
# 연습문제 해답
###########################################################################

# 해답 1
x1 <- c(2, 12, 8, 18)
x2 <- c(4, 5, 4, 3)
x <- matrix(c(2, 12, 8, 18, 4, 5, 4, 3), nrow=4)
ex1.1 <- data.frame(x1,x2)
ex1.1 <- cbind(x1, x2)
# 기술통계량
# install.packages('psych')
library(psych)
describe(ex1.1)
summary(ex1.1)
# 공분산행렬
cov(ex1.1)
# 상관행렬
cor(ex1.1)


# 해답 2
student <- read.csv('student.csv', header=T)
student.x <- student[-1]
# 산점도행렬
plot(student.x, pch=20, cex=2, xlim=c(0,100), ylim=c(0,100))
pairs(student.x, pch=20, cex=2, xlim=c(0,100), ylim=c(0,100))
# 평균벡터
apply(student.x, 2, FUN=mean)
apply(student.x, 2, mean)
?apply
# 공분산행렬
cov(student.x)
# 상관행렬
cor(student.x)


# 해답 3
company <- read.csv('company.csv', header=T)
company.x <- company[,2:9]
# 산점도행렬
plot(company.x, pch=20, cex=1.5)
pairs(company.x, cex=1, cex.labels=1.5)
# 평균벡터
round(apply(company.x, MARGIN=2, FUN=mean),3)
# 공분산행렬
round(cov(company.x),3)
# 상관행렬
round(cor(company.x),3)


# 해답 4
# (a) 중심화, 표준화
company.c <- scale(company.x, center=T, scale=F)
company.c
company.z <- scale(company.x, center=T, scale=T)
company.z
# (b) z의 공분산행렬
round(cov(company.z),3)
# (c) class별 평균, 표준편차
company.mean <- aggregate(.~class, data=company[-1], mean)
company.mean
?tapply
?lapply
is(company)
tapply(company[,2], company$class, mean)
lapply(company[,c(2:9)], function(x) tapply(x, company$class, mean))
aggregate(cbind(x1,x2,x3,x4,x5,x6,x7,x8)~class, data=company, sd)
# (d) 프로파일도표
library(plotrix)
ladderplot(company.mean[-1], scale=T, lty=1, pch=1:3, col=1:3)
legend('topright', pch=1:5, col=1:5, c('1','2','3'))
# (e) 0-1 표준화
library(scales)
company.x.scale <- as.data.frame(lapply(company.x, rescale))
company.x.scale
# (f) 별도표, 레이더도표
