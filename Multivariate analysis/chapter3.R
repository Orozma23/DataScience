getwd()
setwd('C:\\Users\\jkds5\\OneDrive\\바탕 화면\\2022-1\\다변량자료분석1\\data')

# ex 3.1 상관행렬과 고유값
tvprog <- read.csv('tvprog.csv', header=T)
tvprog.X <- na.omit(tvprog[2:9])
print(cor(tvprog.X), digit=3)

tvprog.pca <- princomp(tvprog.X, cor=T)
tvprog.pca$sdev^2

summary(tvprog.pca) # 주성분의 설명비율


# ex 3.2 KMO 표본적합성 측도
library(psych)
KMO(tvprog.X)


# ex 3.3 Bartlett의 구형성 검정
library(psych)
tvprog.cor <- cor(tvprog.X)
cortest.bartlett(tvprog.cor, n=nrow(tvprog.X))


# ex 3.4 자료행렬을 입력으로 하는 경우
library(psych)
tvprog.fa <- principal(tvprog.X, cor='cor', nfactors=2, rotate='varimax')
print(tvprog.fa, sort=T, digit=5)


# ex 3.5 상관행렬의 고유값
print(tvprog.fa$values, digit=5)


# ex 3.6 인자적재값
print(tvprog.fa$loadings, digit=5, cut=0)


# ex 3.7 상관행렬을 입력으로 하는 경우
psycho <- read.csv('psycho.csv', header=T)
print(psycho)

psycho.fa <- principal(r=psycho, n.obs=145, nfactors=3, rotate='varimax')
print(psycho.fa$loadings, digit=5, cut=0)


# ex 3.8 인자적재 플롯
biplot(x=psycho.fa$loadings[,c(1,2)], y=psycho.fa$loadings[,c(1,2)],
       xlabs=colnames(psycho), ylabs=colnames(psycho))
abline(h=0, v=0, lty=2)


# ex 3.9 주성분분석법
satis <- read.csv('satis.csv', header=T)
satis.X <- satis[c('x1','x2','x3','x4','x5')]
satis.X.pca <- prcomp(satis.X, center=T, scale=T)
satis.X.pca$sdev^2

satis.X.pca$rotation # 고유벡터

satis.X.pm <- principal(satis.X, cor='cor', nfactors=2, rotate='none')
print(satis.X.pm, digits=3)


# ex 3.10 주축인자법
satis.X.pa <- fa(satis.X, cor='cor', nfactors=2, fm='pa', rotate='none')
print(satis.X.pa, digits=3)


# ex 3.11 최대우도법
satis.X.ml <- fa(satis.X, cor='cor', nfactors=2, fm='ml', rotate='none')
print(satis.X.ml, digits=3)


# ex 3.12 인자구조 다이어그램
library(psych)
fa.diagram(psycho.fa, simple=F, cut=0.4, digits=3)
fa.diagram(satis.X.ml, simple=F, cut=0.0, digits=3)


# ex 3.13 직교회전-vaimax방법
satis.X.none <- principal(satis.X, nfactors=2, rotate='none')
print(satis.X.none, digits=3)

satis.X.varimax <- principal(satis.X, nfactors=2, rotate='varimax')
print(satis.X.varimax, digits=3)


# ex 3.14 사각회전-Promax방법
satis.X.promax <- principal(satis.X, nfactors=2, rotate='promax')
print(satis.X.promax, digits=3)

print(satis.X.promax$Structure, digits=3)


# ex 3.15 Scree 도형
satis.X.pca$sdev^2

library(psych)
scree(satis.X, hline=1) # Scree 도표


# ex 3.16 카이제곱 적합도검정 <- ex 3.11에서 계속
satis.fa1 <- fa(satis.X, nfactors=1, fm='ml', rotate='varimax')
factor.stats(r=satis.X, f=satis.fa1)

satis.fa2 <- fa(satis.X, nfactor=2, fm='ml', rotate='varimax')
factor.stats(r=satis.X, f=satis.fa2)


# ex 3.17 공분산행렬에 기초한 인자분석 <- ex 3.9에서 계속
satis.cov.ml <- fa(satis.X, cor='cov', nfactors=2, fm='ml',
                   rotate='varimax')
satis.cov.pm <- principal(satis.X, cor='cov', nfactors=2, rotate='varimax')
print(satis.cov.pm, digits=3)

f1 <- (satis.X$x1 + satis.X$x2 + satis.X$x3)/3
f2 <- (satis.X$x4 + satis.X$x5)/2
satis.X.fscore <- cbind(satis.X, f1, f2)
print(satis.X.fscore)


# ex 3.18 상관행렬에 기초한 인자분석 <- ex 3.9에서 계속
satis.cor.ml <- fa(satis.X, cor='cor', nfactors=2, fm='ml',
                   rotate='varimax')
satis.cor.pm <- principal(satis.X, cor='cor', nfactors=2, rotate='varimax')
print(satis.cor.pm, digitis=3)

satis.Z <- as.data.frame(scale(satis.X, center=T, scale=T))
f1 <- (satis.X$x1 + satis.X$x2 + satis.X$x3)/3
f2 <- (satis.X$x4 + satis.X$x5)/2
satis.Z.fscore <- cbind(satis.Z, f1, f2)
print(satis.Z.fscore, digits=3)


# ex 3.19 인자점수계수행렬과 인자점수
satis.cor.pm <- principal(satis.X, nfactors=2, rotate='varimax',
                          scores=T)
satis.cor.pm$weights

satis.Z.fscore <- cbind(satis.Z, satis.cor.pm$scores)
print(satis.Z.fscore, digits=3)


# ex 3.20 주성분분석 행렬도 <- ex 2.3에서 계속
satis.pca <- prcomp(satis.X, center=T, scale=F)

biplot(satis.pca, cex=c(1,1))
abline(h=0, v=0, lty=2)


# ex 3.21 인자분석 행렬도 <- ex 3.18 and 3.19에서 계속
satis.fa <- principal(satis.X, nfactors=2, rotate='varimax')
biplot(satis.fa, cex=c(1,1), col=c('blue','red'))
biplot(x=satis.fa$scores, y=satis.fa$loadings, col=c('blue','red'),
       xlabs=rownames(satis.X), ylabs=colnames(satis.X))
abline(h=0, v=0, lty=2)


# ex 3.22 데이터 행과 열을 바꾸기
carinfo <- read.csv('carinfo.csv', header=T)
carpref <- read.csv('carpref.csv', header=T)
carpref.T <- t(carpref[,-1]) # 전치, 변수 judge 제외
rownames(carpref.T) <- carinfo$model # 행 이름
colnames(carpref.T) <- carpref$judge # 열 이름 1:25
head(carpref.T, 4)


# ex 3.23 인자분석
carpref.fa <- principal(carpref.T, ntactors=2, rotate='none')
print(carpref.fa, digits=3)
print(carpref.fa$scores, digits=3)


# ex 3.24 행렬도 biplot
xcodin <- carpref.fa$scores
ycodin <- carpref.fa$loadings

plot(xcodin, col='blue')
biplot(carpref.fa, col=c('blue','red')) # 인자적재 플롯
biplot(x=xcodin, y=ycodin, col=c('blue','red'), cex=c(0.9, 1),
       xlabs=rownames(carpref.T), ylabs=colnames(carpref.T)) # 행렬도
abline(h=0, v=0, lty=2)

######################################################################
#practice#
######################################################################

# practice 1
student <- read.csv('student.csv', header=T)
student.X <- student[c('x1','x2','x3','x4','x5')]
student.X

student.X.fa <- principal(student.X, nfactors=2, rotate='none')
print(student.X.fa)

student.prcomp <- prcomp(~x1+x2+x3+x4+x5, data=student)
summary(student.prcomp)


# practice 2
R <- matrix(c(1.00, 0.73, 0.70, 0.58, 0.46, 0.56,
              0.73, 1.00, 0.68, 0.61, 0.43, 0.52,
              0.70, 0.68, 1.00, 0.57, 0.40, 0.48,
              0.58, 0.61, 0.57, 1.00, 0.37, 0.41,
              0.46, 0.43, 0.40, 0.37, 1.00, 0.71,
              0.56, 0.52, 0.48, 0.41, 0.71, 1.00), nrow=6, byrow=T)
rownames(R) <- c('x1','x2','x3','x4','x5','x6')
colnames(R) <- c('x1','x2','x3','x4','x5','x6')
R

R.fa <- principal(R, nfactors=2, rotate='none')
R.fa


# practice 3
socioeco <- read.csv('socioeco.csv', header=T)
socioeco

socioeco.cor.fa <- principal(socioeco, cor='cor', nfactors=2, rotate='none')
socioeco.cor.fa

socioeco.cov.fa <- principal(socioeco, cor='cov', nfactors=2, rotate='none')
socioeco.cov.fa

cov.socio <- cov(socioeco[c('pop', 'school', 'employ', 'service', 'house')])
cov.socio.prcomp <- prcomp(~pop+school+employ+service+house, data=as.data.frame(cov.socio))
cov.socio.prcomp


# practice 4
protein <- read.csv('protein.csv', header=T)
protein.X <- protein[c('x1','x2','x3','x4','x5','x6','x7','x8','x9')]
protein.X

# (a)
protein.cor.X <- cor(protein.X)
protein.cor.prcomp<- prcomp(protein.cor.X)
protein.cor.prcomp

protein.cov.X <- cov(protein.X)
protein.cov.prcomp <- prcomp(protein.cov.X)
protein.cov.prcomp

# (b)
protein.cor.prcomp$sdev^2

library(psych)
scree(protein.cor.X, hline=1)

protein.fa=fa(protein.X, nfactor=2, fm='ml', rotate='varimax')
factor.stats(r=protein.X, f=protein.fa)

# (c)
protein.fa1 <- principal(protein.X, nfactors=2, rotate='none')
protein.fa1

# (d)
biplot(x=protein.fa1$loadings, y=protein.fa1$loadings,
       xlabs=colnames(protein.X), ylabs=colnames(protein.X))
abline(h=0, v=0, lty=2)

biplot(x=protein.fa1$scores, y=protein.fa1$scores)
abline(h=0, v=0, lty=2)

biplot(protein.fa1, cex=c(1,1), col=c('blue','red'))
abline(h=0, v=0, lty=2)


# practice 5
library(psych)
tvprog <- read.csv('tvprog.csv', header=T)
tvprog.X <- na.omit(tvprog[2:9])
tvprog.fa <- principal(tvprog.X, cor='cor', nfactors=2, rotate='varimax')
print(tvprog.fa, sort=T, digit=5)

# (a)
biplot(x=tvprog.fa$loadings[,c(1,2)], y=tvprog.fa$loadings[,c(1,2)])
abline(h=0, v=0, lty=2)

# (b)
f1 <- (tvprog.X$x1 + tvprog.X$x2 + tvprog.X$x3 + tvprog.X$x4)/4
f2 <- (tvprog.X$x5 + tvprog.X$x6 + tvprog.X$x7 + tvprog.X$x8)/4
tvprog.X.fscore <- cbind(tvprog.X, f1, f2)
print(head(tvprog.X.fscore, 20))

# (c)
biplot(tvprog.fa, cex=c(1,1), col=c('blue','red'))
biplot(x=tvprog.fa$scores, y=tvprog.fa$loadings, col=c('blue','red'))
abline(h=0, v=0, lty=2)


# practice 6
# in ex 2.11
tvad <- read.csv('tvad.csv', header=T)
tvad.X <- na.omit(tvad[-1])
head(tvad.X, 10)

tvad.fa <- principal(tvad.X, nfactors=2, rotate='none')
tvad.fa

# in ex 2.14
library(Gifi)
tvad.X.princals <- princals(data=tvad.X, ndim=4, ordinal=T, degrees=3)
tvad.T <- data.frame(tvad.X.princals$transform)
head(tvad.T)

tvad.T.fa <- principal(tvad.T, nfactors=2, rotate='none')
tvad.T.fa


# practice 7
recinfo <- read.csv('recinfo.csv', header=T)
recpref <- read.csv('recpref.csv', header=T)
recpref.T <- t(recpref[,-1]) 
rownames(recpref.T) <- recinfo$model
colnames(recpref.T) <- recpref$judge
head(recpref.T, 4)

recpref.fa <- principal(recpref.T, ntactors=2, rotate='none')
print(recpref.fa, digits=3)
print(recpref.fa$scores, digits=3)

xcodin <- recpref.fa$scores
ycodin <- recpref.fa$loadings

plot(xcodin, col='blue')
biplot(recpref.fa, col=c('blue','red'))
biplot(x=xcodin, y=ycodin, col=c('blue','red'), cex=c(0.9, 1),
       xlabs=rownames(recpref.T), ylabs=colnames(recpref.T))
abline(h=0, v=0, lty=2)


# practice solution

# practice 1
student.fa <- principal(student.X, cor='cor', nfactors-2, rotate='varimax')
print(student.fa, digits=5)
print(student.fa$values, digits=5)

# practice 2
ability <- matrix(c(1.00, 0.73, 0.70, 0.58, 0.46, 0.56,
              0.73, 1.00, 0.68, 0.61, 0.43, 0.52,
              0.70, 0.68, 1.00, 0.57, 0.40, 0.48,
              0.58, 0.61, 0.57, 1.00, 0.37, 0.41,
              0.46, 0.43, 0.40, 0.37, 1.00, 0.71,
              0.56, 0.52, 0.48, 0.41, 0.71, 1.00), nrow=6, byrow=T)
print(ability)

library(psych)

KMO(r=ability)

cor.test.barlett(ability, n=556)

ability.fa <- principal(r=ability, n.obs=556, nfactors=2, rotate='varimax')
print(ability.fa, digits=5)
print(ability.fa$values, digits=5)


# practice 3
socioeco <- read.csv('socioeco.csv', header=T)
library(psych)

KMO(socioeco)
socioeco.cor <- cor(socioeco)
cor.test.bartlett(socioeco)

socioeco.cov.fa <- principal(socioeco, cor='cor', nfactors=2, rotate='varimax')


# practice 4
protein <- read.csv('protein.csv', header=T)
protein.X <- protein[-1]

# (a)
cor(protein.X)
protein.cor.prcomp <- prcomp(protein.X, center=T, scale=T)
print(protein.co.prcomp)
protein.cor.prcomp$sdev^2
summary(protein.cor.prcomp)

cov(protein.X)
protein.cov.prcomp <- prcomp(protein.X, center=T, scale=F)
print(protein.cov.prcomp)
protein.cov.prcomp$sdev^2
summary(protein.cov.prcomp)

# (b)
library(psych)
scree(protein.X, hline=1)

protein.fa1=fa(protein.X, covar=F, nfactor=1, fm='ml', rotate='varimax')
factor.stats(r=protein.X, f=protein.fa1)
protein.fa2=fa(protein.X, covar=F, nfactor=1, fm='ml', rotate='varimax')
factor.stats(r=protein.X, f=protein.fa2)
protein.fa3=fa(protein.X, covar=F, nfactor=1, fm='ml', rotate='varimax')
factor.stats(r=protein.X, f=protein.fa3)

# (c)
KMO(protein.X)

protein.cor.fa <- principal(protein.X, cor='cor', nfactors=2, rotate='varimax')
print(protein.cor.fa)

# (d)


# practice 5

# (a)
biplot(tvprog.fa, cex=c(1,1), col=c('blue','red'))
biplot(x=tvprog.fa$loadings, y=tvprog.fa$loadings)
abline(h=0, v=0, lty=2)

# (b)
head(tvprog.fa$scores)
fa.score <- cbind(na.omit(tvprog), tvprog.fa$scores)
head(fa.score)

# (c)
fa.gender <- aggregate(cbind(RC1, RC2)~gender, data=fa.score, FUN=mean)
fa.gender$group <- c('male','femal')

fa.mean <- rbind(fa.gender[-1], fa.age[-1], fa.married[-1],
                 fa.house[-1], fa.job[-1])
fa.mean

# (d)
biplot(x=fa.mean[,c(1,2)], y=tvprog.fa$loadings, col=c('blue','red'),
       xlabs=fa.mean$group, ylabs=colnames(tvprog.X), ylim=c(-0.4, 0.3))
abline(h=0, v=0, lty=2)

# practice 6
tvad.X.princals <- princals(data=tvad.X, ndim=5, ordinal=T, degrees=3)
tvad.T.fa <- principal(tvad.T, cor='cor', nfactors=4, rotate='varimax')


# practice 7
recpref.fa <- principal(recpref.T, nfactors=2, rotate='none')
