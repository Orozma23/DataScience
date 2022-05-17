getwd()
setwd('C:\\Users\\jkds5\\OneDrive\\바탕 화면\\2022-1\\다변량자료분석1\\data')

# ex 2.1 기술통계량
satis <- read.csv('satis.csv', header=T)
library(psych)
describe(satis[c('x1','x2','x3','x4','x5')])


# ex 2.2 상관계수
cor(satis[c('x1','x2','x3','x4','x5')]) # 상관행렬
corr.test(satis[c('x1','x2','x3','x4','x5')]) # p-값

# ex 2.3 주성분분석
cov(satis[c('x1','x2','x3','x4','x5')]) # 공분산행렬
satis.prcomp <- prcomp(~x1+x2+x3+x4+x5, data=satis) # 주성분분석
print(satis.prcomp)

satis.prcomp$sdev # 주성분의 표준편차
satis.prcomp$sdev^2 # 고유값
summary(satis.prcomp) # 설명분산 요약


# ex 2.4 주성분점수
satis.score <- cbind(satis, satis.prcomp$x[,1:2])
print(satis.score)


# ex 2.5 주성분점수 그래프
# 개체 번호별 그래프
plot(satis.score[c('PC1','PC2')], xlim=c(-4,4), ylim=c(-2.5,2.5))
abline(h=0, v=0)
text(satis.score[c('PC1','PC2')], labels=rownames(satis.score), pos=2)

# 성별 그래프
plot(satis.score[c('PC1','PC2')], xlim=c(-4,4), ylim=c(-2.5,2.5))
abline(h=0, v=0)
text(satis.score[c('PC1','PC2')], labels=satis.score$gender, pos=2)

# 연령대별 그래프
plot(satis.score[c('PC1','PC2')], xlim=c(-4,4), ylim=c(-2.5,2.5))
abline(h=0, v=0)
text(satis.score[c('PC1','PC2')], labels=satis.score$age, pos=2)


# ex 2.17 집단 중심점 플롯
# 주성분분석
satis.X <- satis[c('x1','x2','x3','x4','x5')]
satis.X.prcomp <- prcomp(satis.X, center=T, scale=F)
satis.score <- cbind(satis, satis.X.prcomp$x)

# 집단 평균 계산
pcm.age=aggregate(cbind(PC1,PC2)~age, data=satis.score, FUN=mean)
pcm.gender=aggregate(cbind(PC1,PC2)~gender, data=satis.score, FUN=mean)
# 개체별 플롯
plot(satis.score[c('PC1','PC2')], xlim=c(-4,4), ylim=c(-2.5,2.5))
abline(h=0, v=0)
# 연령대별 중심점
points(pcm.age[,2:3], pch=8, cex=1.5, col='darkgreen')
text(pcm.age[,2:3], labels=pcm.age$age, pos=4, col='darkgreen')
# 성별 중심점
points(pcm.gender[,2:3], pch=19, cex=1.5, col='red')
text(pcm.gender[,2:3], labels=pcm.gender$gender, pos=4, col='red')


# ex 2.6 주성분분석 행렬도(Biplot)
biplot(satis.prcomp, cex=1)
abline(h=0, v=0, lty=2)


# ex 2.7 공분산행렬에 기초한 주성분분석
satis.X <- satis[c('x1','x2','x3','x4','x5')]
satis.prcomp <- prcomp(satis.X, center=T, scale=F)
print(satis.prcomp)
satis.prcomp$sdev^2
summary(satis.prcomp)


# ex 2.8 상관행렬에 기초한 주성분분석
cor(satis.X) # 상관행렬

satis.cor.prcomp <- prcomp(satis.X, center=T, scale=T) # 분석
print(satis.cor.prcomp)

satis.cor.prcomp$sdev^2 # 고유값
summary(satis.cor.prcomp) # 설명분산

satis.cor.score <- cbind(satis, satis.cor.prcomp$x[,1:2])
print(satis.cor.score)


# practice 1
student <- read.csv('student.csv', header=T)
student.prcomp <- prcomp(~x1+x2+x3+x4+x5, data=student)
student.prcomp

summary(student.prcomp)


# practice 2
socioeco <- read.csv('socioeco.csv', header=T)
socioeco

# (a)
cov.socio <- cov(socioeco[c('pop', 'school', 'employ', 'service', 'house')])

cov.socio.prcomp <- prcomp(~pop+school+employ+service+house, data=as.data.frame(cov.socio))
cov.socio.prcomp

# (b)
socioeco.generalization <- scale(socioeco, center=T, scale=T)
cov.socio.gen <- cov(socioeco.generalization)

cov.socio.gen.prcomp <- prcomp(~pop+school+employ+service+house, data=as.data.frame(cov.socio.gen))
cov.socio.gen.prcomp

# (c)
summary(cov.socio.prcomp)
summary(cov.socio.gen.prcomp)


# practice 4
sigma = matrix(c(10000, 60, 60, 1), nrow=2, byrow=T)
colnames(sigma) <- c('x1', 'x2')
sigma

# (a)
cor(sigma)

# (b)
sigma.prcomp <- prcomp(~x1+x2, data=as.data.frame(sigma))
sigma.prcomp

sigma.prcomp$sdev^2

# (c)
cor.sigma.prcomp <- prcomp(~x1+x2, data=as.data.frame(cor(sigma)))
cor.sigma.prcomp

cor.sigma.prcomp$sdev^2

# (d)
summary(sigma.prcomp)
summary(cor.sigma.prcomp)


# practice 5
sigma_5 <- matrix(c(5, -3, 0, -3, 4, 0, 0, 0, 2), nrow=3, byrow=T)
colnames(sigma_5) <- c('x1', 'x2', 'x3')
sigma_5

# (a)
sigma_5.prcomp <- prcomp(~x1+x2+x3, data=as.data.frame(sigma_5))
sigma_5.prcomp
sigma_5.prcomp$sdev^2

# (b)
summary(sigma_5.prcomp)

# (c)
sigma_5.score <- cbind(sigma_5, sigma_5.prcomp$x[,1:3])
sigma_5.score

# (d)
round(mean(sigma_5.prcomp$x[1:3,]),2)

# (e)
sum_var.sigma_5 <- var(sigma_5[,1])+var(sigma_5[,2])+var(sigma_5[,3])
sum_var.sigma_5                       

sum_PCI.sigma_5 <- sum(sigma_5.prcomp$sdev^2)
sum_PCI.sigma_5

# (f)
sigma_5.score <- cbind(sigma_5, sigma_5.prcomp$x[,1])
colnames(sigma_5.score) <- c('x1','x2','x3','PC1')
cor(sigma_5.score)


# practice 6
s <- matrix(c(5,0,0,0,1,0,0,0,2), nrow=3, byrow=T)
colnames(s) <- c('x1','x2','x3')
s

s.prcomp <- prcomp(~x1+x2+x3, data=as.data.frame(s))
s.prcomp


# practice 7
#######################################################################
tvad <- read.csv('tvad.csv', header=T)
head(tvad)
tvad.X <- na.omit(tvad[-1])

tvad.X.prcomp <- prcomp(tvad.X)
tvad.X.prcomp$sdev^2
summary(tvad.X.prcomp)

# install.packages('Gifi')
library(Gifi)
tvad.X.princals <- princals(data=tvad.X, ndim=4, ordinal=T, degrees=3)
summary(tvad.X.princals, cutoff=0.0001)
#######################################################################
tvad.T <- data.frame(tvad.X.princals$transform)
tvad.T.prcomp <- prcomp(tvad.T)
summary(tvad.T.prcomp)


# practice 8
satis <- read.csv('satis.csv', header=T)
satis.x <- satis[,-c(1:3)]
satis.x

# (a)
satis.centered <- scale(satis.x, center=T, scale=F)
satis.centered

# (b)
satis.svd <- svd(satis.centered)
satis.svd$d

# (c)
satis.ctc <- t(satis.centered) %*% satis.centered

satis.ed <- eigen(satis.ctc)
satis.ed


# practice 9
# 주성분분석
satis.X <- satis[c('x1','x2','x3','x4','x5')]
satis.X.prcomp <- prcomp(satis.X, center=T, scale=F)
satis.score <- cbind(satis, satis.X.prcomp$x)

# 집단 평균 계산
pcm.age=aggregate(cbind(PC1,PC2)~age, data=satis.score, FUN=mean)
pcm.gender=aggregate(cbind(PC1,PC2)~gender, data=satis.score, FUN=mean)
# 개체별 플롯
plot(satis.score[c('PC1','PC2')], xlim=c(-4,4), ylim=c(-2.5,2.5))
abline(h=0, v=0)
# 연령대별 중심점
points(pcm.age[,2:3], pch=8, cex=1.5, col='darkgreen')
text(pcm.age[,2:3], labels=pcm.age$age, pos=4, col='darkgreen')
# 성별 중심점
points(pcm.gender[,2:3], pch=19, cex=1.5, col='red')
text(pcm.gender[,2:3], labels=pcm.gender$gender, pos=4, col='red')


# practice solution

# practice 1
student <- read.csv('student.csv', header=T)

library(psych)
describe(student[c('x1','x2','x3','x4','x5')])

cor(student[c('x1','x2','x3','x4','x5')])
corr.test(student[c('x1','x2','x3','x4','x5')])

cov(student[c('x1','x2','x3','x4','x5')])
student.prcomp <- prcomp(~x1+x2+x3+x4+x5, data=student)
student.X <- student[c('x1','x2','x3','x4','x5')]
student.prcomp <- prcomp(student.X, center=T, scale=F)
student.prcomp <- prcomp(student.X)

print(student.prcomp)
student.prcomp$sdev
student.prcomp$sdev^2
summary(student.prcomp)

student.score <- cbind(student, student.prcomp$x[,1:2])
print(student.score)

plot(student.score[c('PC1','PC2')])
abline(h=0, v=0)
text(student.score[c('PC1','PC2')],
     labels=rownames(student.score), pos=2)

biplot(student.prcomp, cex=1)
abline(h=0, v=0, lty=2)


# practice 2

# (a)
socioeco <- read.csv('socioeco.csv', header=T)

library(psych)
describe(socioeco)
cov(socioeco)

socioeco.prcomp <- prcomp(socioeco)
print(socioeco.prcomp)
socioeco.prcomp$sdev
socioeco.prcomp$sdev^2
summary(socioeco.prcomp)

biplot(socioeco.prcomp, cex=1)
abline(h=0, v=0, lty=2)

cor(socioeco)

socioeco.prin <- prcomp(socioeco, center=T, scale=T)
print(socioeco.prin)
socioeco.prin$sdev
socioeco.prin$sdev^2
summary(socioeco.prin)

biplot(socioeco.prin, cex=1)
abline(h=0, v=0, lty=2)


# practice 3
# 손으로 풀어야 하는 문제


# practice 4

# (a)
sigma <- matrix(c(10000, 60, 60, 1), nrow=2)
?cov2cor
P <- cov2cor(sigma) # 공분산 행렬을 상관행렬로 바꿔주는 함수
print(P)
D <- diag(diag(sigma))
sqrt.D <- sqrt(D)
P <- solve(sqrt.D%*%sigma%*%solve(sqrt.D))
print(P)

# (b)
eigen(sigma)
sigma.princomp <- princomp(covmat=sigma)
sigma.princomp$sdev^2
summary(sigma.princomp, loadings=T, cutoff=0.0001)

# (c)
eigen(P)
P.princomp <- princomp(covmat=P)
P.princomp$sdev^2
summary(P.princomp, loadings=T, cutoff=0.0001)


# practice 5
sigma <- matrix(c(5,-3,0,-3,4,0,0,0,2), nrow=3)
eigen(sigma)$vectors
eigen(sigma)$values
sigma.princomp <- princomp(covmat=sigma)
sigma.princomp$sdev^2
summary(sigma.prcomp, loadings=T, cutoff=0.0001)


# practice 6
sigma<- matrix(c(5,0,0,0,1,0,0,0,2), nrow=3)
eigen(sigma)
sigma.princomp <- princomp(covmat=sigma)
sigma.princomp$sdev^2
summary(sigma.princomp, loadings=T, cutoff=0.0001)


# practice 7
tvad.T.prcomp <- prcomp(tvad.T)
tvad.T.prcomp$sdev^2
summary(tvad.T.prcomp)


# practice 8

# (a)
satis.X <- satis[c('x1','x2','x3','x4','x5')]
satis.C <- scale(satis.X, center=T, scale=F)
satis.C

# (b)
svd(satis.C)

# (c)
satis.CC <- t(satis.C)%*%satis.C
eigen(satis.CC)


# practice 9
# 주성분분석
satis.X <- satis[c('x1','x2','x3','x4','x5')]
satis.X.prcomp <- prcomp(satis.X, center=T, scale=F)
satis.score <- cbind(satis, satis.X.prcomp$x)

# 집단 평균 계산
pcm.age=aggregate(cbind(PC1,PC2)~age, data=satis.score, FUN=mean)
pcm.gender=aggregate(cbind(PC1,PC2)~gender, data=satis.score, FUN=mean)
# 개체별 플롯
plot(satis.score[c('PC1','PC2')], xlim=c(-4,4), ylim=c(-2.5,2.5))
abline(h=0, v=0)
# 연령대별 중심점
points(pcm.age[,2:3], pch=8, cex=1.5, col='darkgreen')
text(pcm.age[,2:3], labels=pcm.age$age, pos=4, col='darkgreen')
# 성별 중심점
points(pcm.gender[,2:3], pch=19, cex=1.5, col='red')
text(pcm.gender[,2:3], labels=pcm.gender$gender, pos=4, col='red')