#연습문제 2
setwd("C:\\데이터 set")
ex22 <- read.csv("exdat-2-2.csv", header=F)

#연습문제 2-(1)
str(ex22)
x <- as.matrix(ex22)
max(x)
min(x)
round((max(x)-min(x))/15, 1)
round(min(x)-0.1/2, 2)
xcut <- seq(from=24.15, by=0.1, length.out=16)
xcut
xgroup <- factor(cut(x, breaks=xcut,dig.lab=4))
levels(xgroup)
table(xgroup)
xfreq <- as.matrix(table(xgroup))
xfreq
as.vector(xfreq)
mids <- (xcut[-c(16)]+xcut[-c(1)])/2
mids
xcf <- cumsum(xfreq)
xcf
xrf <- xfreq/length(x)
xrf
xrcf <- xcf/length(x)
xrcf
as.vector(xrf)
freq_table <- cbind(mids, xfreq, xcf, xrf, xrcf)
colnames(freq_table) <- c("계급값", "도수", "누적도수", "상대도수", "누적상대도수")
freq_table

#연습문제 2-(2)
stem(x, scale=0.25)

#연습문제 2-(3)
hist(x, nclass=15, main="제품의 측정치")

#연습문제 2-(4)
mean(x) #평균
sd(x) #표준편차
(sd(x))^2 #분산
median(x) #중위수

#연습문제 2-(5)
#24.5~25.5 사이가 규격이므로 범위를 초과한 값을 찾으면 됨
ex22[ex22 < 24.5]
ex22[ex22 > 25.5]
#전체 제품 100개 중 불량품은 9개이므로 불량률은 0.09(9%)

#연습문제 2-(6)
hist(x, nclass=15, main="제품의 측정치", xlab="규격: 24.5~25.5 불량률: 9%")


#연습문제 3
ex23 <- read.csv("exdat-2-3.csv", header=F)
ex23

#연습문제 3-(1)
str(ex23)
new_ex23 <- ex23[2:11]
new_ex23
x <- as.matrix(new_ex23)
xmax <- max(x)
xmax
xmin <- min(x)
xmin
xcut <- seq(from=7.5, by=5, length.out=21)
xgroup <- factor(cut(x, breaks=xcut, dig.lab=4))
levels(xgroup)
xfreq <- as.matrix(table(xgroup))
mids <- (xcut[-21]+xcut[-1])/2
xcf <- cumsum(xfreq)
xrf <- xfreq/length(x)
xrcf <- xcf/length(x)
as.vector(xrf)
freq_table <- cbind(mids, xfreq, xcf, xrf, xrcf)
colnames(freq_table) <- c("계급값", "도수", "누적도수", "상대도수", "누적상대도수")
freq_table

#연습문제 3-(2)
stem(x)

#연습문제 3-(3)
hist(x, xlim=c(0,100), main="시험성적")

#연습문제 3-(4)
mean(x) #평균
sd(x) #표준편차
(sd(x))^2 #분산
median(x) #중위수

#연습문제 3-(5)
#남자 지원자의 히스토그램
str(ex23)
ex23_1 <- ex23[1:4,]
ex23_1
ex23_1_1 <- ex23_1[2:11]
ex23_1_1
x1 <- as.matrix(ex23_1_1)
xmax1 <- max(x1)
xmax1
xmin1 <- min(x1)
xmin1
xcut1 <- seq(from=7.5, by=5, length.out=21)
xgroup1 <- factor(cut(x1, breaks=xcut1, dig.lab=4))
levels(xgroup1)
xfreq1 <- as.matrix(table(xgroup1))
mids1 <- (xcut1[-21]+xcut1[-1])/2
xcf1 <- cumsum(xfreq1)
xrf1 <- xfreq1/length(x1)
xrcf1 <- xcf1/length(x1)
as.vector(xrf1)
freq_table1 <- cbind(mids1, xfreq1, xcf1, xrf1, xrcf1)
colnames(freq_table1) <- c("계급값", "도수", "누적도수", "상대도수", "누적상대도수")
freq_table1
#여자 지원자의 히스토그램
str(ex23)
ex23_2 <- ex23[5:8,]
ex23_2
ex23_2_2 <- ex23_2[2:11]
ex23_2_2
x2 <- as.matrix(ex23_2_2)
xmax2 <- max(x2)
xmax2
xmin2 <- min(x2)
xmin2
xcut2 <- seq(from=7.5, by=5, length.out=21)
xgroup2 <- factor(cut(x2, breaks=xcut2, dig.lab=4))
levels(xgroup2)
xfreq2 <- as.matrix(table(xgroup2))
mids2 <- (xcut2[-21]+xcut2[-1])/2
xcf2 <- cumsum(xfreq2)
xrf2 <- xfreq2/length(x2)
xrcf2 <- xcf2/length(x2)
as.vector(xrf2)
freq_table2 <- cbind(mids2, xfreq2, xcf2, xrf2, xrcf2)
colnames(freq_table2) <- c("계급값", "도수", "누적도수", "상대도수", "누적상대도수")
freq_table2

#연습문제 3-(6)
#남자 지원자의 줄기잎그림
stem(x1)
#여자 지원자의 줄기잎그림
stem(x2)

#연습문제 3-(7)
#남자 지원자의 히스토그램
hist(x1, xlim=c(0,100), main="남자 지원자의 시험성적")
#여자 지원자의 히스토그램
hist(x2, xlim=c(0,100), main="여자 지원자의 시험성적")

#연습문제 3-(8)
#남자 지원자의 평균, 분산, 중앙값
mean(x1) #평균
sd(x1) #표준편차
(sd(x1))^2 #분산
median(x1) #중위수
#여자 지원자의 평균, 분산, 중앙값
mean(x2) #평균
sd(x2) #표준편차
(sd(x2))^2 #분산
median(x2) #중위수

#연습문제 3-(9)
#평균은 여성 지원자들이 앞서나, 중위수의 값이 남성 지원자들이 높고, 
#고득점(60점 이상) 지원자의 비율이 남성이 더 높으므로 남성 지원자들이 약간 더 우세하다고 봅니다.

#연습문제 3-(10)
#남성지원자 5명(96,94,94,89,89)과 여성지원자 4명(97,95,94,90)이 합격할 것입니다.

#연습문제 3-(11)
#최저 점수가 89점이므로 커트라인은 89점입니다.

#연습문제 3-(12)
#남성지원자의 평균은 92.4점입니다.
(96+94+94+89+89)/5
#여성지원자의 평균은 94점입니다.
(97+95+94+90)/4

#연습문제 3-(13)
#남성지원자 7명(96,94,94,89,89,87,86)과 여성지원자 5명(97,95,94,90,87)이 합격할 것입니다.

#연습문제 3-(14)
#남성지원자의 평균은 90.71419점입니다.
(96+94+94+89+89+87+86)/7
#여성지원자의 평균은 92.6점입니다.
(97+95+94+90+87)/5

#연습문제 3-(15)
#남성지원자의 경우 커트라인은 89점입니다. 89점일 경우 -> (96,94,94,89,89)
#여성지원자의 경우 커트라인은 87점입니다. 87점일 경우 -> (97,95,94,90,87)

#연습문제 3-(R1)
boxplot(x, main="시험성적의 상자그림", boxwex=0.5, ylab="점수", col=7)
grid(col=3)
points(rep(1:10, each=10), x, pch=19, col=2)
xstat <- apply(x, 2, fivenum)
text(rep(1:10, each=5), xstat, labels=xstat, col=4, cex=0.8, pos=4)

#연습문제 3-(R2)
#남성지원자의 상자그림
boxplot(x1, main="남자 지원자의 상자그림", boxwex=0.5, ylab="점수", col=7)
grid(col=3)
points(rep(1:10, each=10), x1, pch=19, col=2)
xstat1 <- apply(x1, 2, fivenum)
text(rep(1:10, each=5), xstat1, labels=xstat1, col=4, cex=0.8, pos=4)
#여성지원자의 상자그림
boxplot(x2, main="여성지원자의 상자그림", boxwex=0.5, ylab="점수", col=7)
grid(col=3)
points(rep(1:10, each=10), x2, pch=19, col=2)
xstat2 <- apply(x2, 2, fivenum)
text(rep(1:10, each=5), xstat2, labels=xstat2, col=4, cex=0.8, pos=4)

#연습문제 R1
#tab2-2.csv가 제대로 인식이 되지 않아 문제를 풀 수 없었습니다.
#tab2-2.csv의 열 이름을 영어로 수정하여 문제를 해결했습니다.
data22 <- read.csv("tab2-2.csv", header=T)
data22
str(data22)

#연습문제 R1-(1)
stem(data22[,4], scale=0.25)

#연습문제 R1-(2)
hist(data22[,4], nclass=20, xlim=c(0,5))

#연습문제 R1-(3)
mean(data22[,4]) #평균
sd(data22[,4]) #표준편차
(sd(data22[,4]))^2 #분산
median(data22[,4]) #중위수

#연습문제 R1-(4)
table(data22$gender)
table(data22$method)

#연습문제 R1-(5)
#남학생의 GPA
x1 <- subset(data22, gender=="남")
x1[,4]
hist(x1[,4], nclass=20, xlim=c(0,5))
#여학생의 GPA
x2 <- subset(data22, gender=="여")
x2[,4]
hist(x2[,4], nclass=20, xlim=c(0,5))

#연습문제 R1-(6)
#남학생의 평균, 분산, 중위수
mean(x1[,4]) #평균
sd(x1[,4]) #표준편차
(sd(x1[,4]))^2 #분산
median(x1[,4]) #중위수
#여학생의 평균, 분산, 중위수
mean(x2[,4]) #평균
sd(x2[,4]) #표준편차
(sd(x2[,4]))^2 #분산
median(x2[,4]) #중위수

#연습문제 R1-(7)
#여학생의 GPA 평균과 중위수가 더 높으며 고득점자도 많기에 여학생이 더 우수하다고 생각합니다.

#연습문제 R1-(8)
#수시 입학생의 히스토그램
m1 <- subset(data22, method=="논술우수" | method=="학생부교과"|method=="학생부종합")
m1[,4]
hist(m1[,4], nclass=20, xlim=c(0,5))
#정시 입학생의 히스토그램
m2 <- subset(data22, method=="정시일반")
m2[,4]
hist(m2[,4], nclass=20, xlim=c(0,5))

#연습문제 R1-(9)
#수시 입학생의 평균, 분산, 중위수
mean(m1[,4]) #평균
sd(m1[,4]) #표준편차
(sd(m1[,4]))^2 #분산
median(m1[,4]) #중위수
#정시 입학생의 평균, 분산, 중위수
mean(m2[,4]) #평균
sd(m2[,4]) #표준편차
(sd(m2[,4]))^2 #분산
median(m2[,4]) #중위수

#연습문제 R1-(10)
#평균에서 큰 차이가 나지 않고, 중위수는 동일합니다.
#그러나 4점 이상의 고득점자가 수시 입학생들에게서 더 많았기 때문에 수시 입학생이 조금 더 우수하다고 생각합니다,

#연습문제 R1-(11)
#수시전형으로 입학한 남학생의 평균, 분산, 중위수
y1 <- subset(data22, method=="논술우수" | method=="학생부교과"|method=="학생부종합")
z1 <- subset(y1, gender=="남")
z1
mean(z1[,4]) #평균
sd(z1[,4]) #표준편차
(sd(z1[,4]))^2 #분산
median(z1[,4]) #중위수
#정시전형으로 입학한 남학생의 평균, 분산, 중위수
y3 <- subset(data22, method=="정시일반")
z3 <- subset(y3, gender=="남")
z3
mean(z3[,4]) #평균
sd(z3[,4]) #표준편차
(sd(z3[,4]))^2 #분산
median(z3[,4]) #중위수
#수시전형으로 입학한 여학생의 평균, 분산,중위수
y2 <- subset(data22, method=="논술우수" | method=="학생부교과"|method=="학생부종합")
z2 <- subset(y2, gender=="여")
z2
mean(z2[,4]) #평균
sd(z2[,4]) #표준편차
(sd(z2[,4]))^2 #분산
median(z2[,4]) #중위수
#정시전형으로 입학한 여학생의 평균, 분산, 중위수
y4 <- subset(data22, method=="정시일반")
z4 <- subset(y4, gender=="여")
z4
mean(z4[,4]) #평균
sd(z4[,4]) #표준편차
(sd(z4[,4]))^2 #분산
median(z4[,4]) #중위수

#연습문제 R1-(12)
boxplot(data22[,4], main="전체 성적의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(data22[,4], pch=19, col=2)
#GPA평점 중 0.8점이 이상치로 볼 수 있습니다.

#연습문제 R1-(13)
par(mfrow=c(1,2))
#남학생의 상자그림
x1 <- subset(data22, gender=="남")
x1[,4]
boxplot(x1[,4], main="남학생 성적의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(x1[,4], pch=19, col=2)
xfn1 <- fivenum(x1[,4])
xfn1
#여학생의 상자그림
x2 <- subset(data22, gender=="여")
x2[,4]
boxplot(x2[,4], main="여학생 성적의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(x2[,4], pch=19, col=2)
xfn2 <- fivenum(x2[,4])
xfn2

#연습문제 R1-(14)
#수시전형 입학자의 상자그림
m1 <- subset(data22, method=="논술우수" | method=="학생부교과"|method=="학생부종합")
m1[,4]
boxplot(m1[,4], main="수시 입학자의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(m1[,4], pch=19, col=2)
xfn1 <- fivenum(m1[,4])
xfn1
#정시전형 입학자의 상자그림
m2 <- subset(data22, method=="정시일반")
m2[,4]
boxplot(m2[,4], main="정시 입학자의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(m2[,4], pch=19, col=2)
xfn2 <- fivenum(m2[,4])
xfn2

#연습문제 R1-(15)
par(mfrow=c(2,2))
#수시전형으로 입학한 남학생의 상자그림
y1 <- subset(data22, method=="논술우수" | method=="학생부교과"|method=="학생부종합")
z1 <- subset(y1, gender=="남")
z1
boxplot(z1[,4], main="수시 입학 남학생의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(z1[,4], pch=19, col=2)
xfn1 <- fivenum(z1[,4])
xfn1
#정시전형으로 입학한 남학생의 상자그림
y3 <- subset(data22, method=="정시일반")
z3 <- subset(y3, gender=="남")
z3
boxplot(z3[,4], main="정시 입학 남학생의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(z3[,4], pch=19, col=2)
xfn1 <- fivenum(z3[,4])
xfn1
#수시전형으로 입학한 여학생의 상자그림
y2 <- subset(data22, method=="논술우수" | method=="학생부교과"|method=="학생부종합")
z2 <- subset(y1, gender=="여")
z2
boxplot(z2[,4], main="수시 입학 여학생의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(z2[,4], pch=19, col=2)
xfn2 <- fivenum(z2[,4])
xfn2
#정시전형으로 입학한 여학생의 상자그림
y4 <- subset(data22, method=="정시일반")
z4 <- subset(y4, gender=="여")
z4
boxplot(z4[,4], main="정시 입학 여학생의 상자그림", boxwex=0.5, ylab="GPA", col=7)
grid(col=3)
points(z4[,4], pch=19, col=2)
xfn4 <- fivenum(z4[,4])
xfn4
