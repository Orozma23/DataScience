2+1
x=2
x
y=1
y
x+y
x=y=z=1 #다중할당
x;y;z;
ls()
rm(list=ls())
ls()

install.packages("tidyverse")
library(tidyverse)
search()

x=c(1,2,3) #c로 벡터 생성
x
y=c(x1=1, x2=2, x3=3) #벡터 원소에 이름 할당하여 벡터 생성
y
names(x)=c('x1','x2','x3') #names()로 벡터 원소에 이름 할당
c(x,y) #벡터 이어붙이기

1:10
seq(1,10)
seq(1,10,2)
seq(1,10,by=2) #간격이 2인 수열
?seq
seq(1,10,0.2)
seq(0,100,length=5) #벡터 크기가 5인 수열

c(1,1,1,1,1,1,1,1,1,1)
rep(1,times=10)
rep(c(1,2,3),times=10) #벡터를 반복
paste('x',1:10,sep='') #문자와 숫자를 결합한 연속되는 변수명 만들기
matrix(c(1:12), nrow=3, ncol=4, byrow=T)
matrix(c(1:12), nrow=3, ncol=4, byrow=F)

c1=c(1,2,3,4) #길이 4인 벡터
c2=c(5,6,7,8) #길이 4인 벡터
c3=c(9,10,11,12) #길이 4인 벡터
m1=cbind(c1,c2) #col 열벡터로 묶어서 행렬 생성
m1
m2=rbind(c1,c2) #row 행벡터로 묶어서 행렬 생성
m2
m3=rbind(c1,c2,c3)
m3
t(m3) #transpose
dim(m1)
dim(m3)

rownames(m1)
rownames(m1)=c("A","B","C","D")
m1
m1[2,] #특정 행이나 열 얻기 가능
m1[2,2] #특정 원소
m3[c(2,3),c(3,4)] #부분행렬
m3[-1,-c(1,2)] #특정 행과 열을 제외한 부분행렬 m3[c(2,3),c(3,4)]

x=c(0,NA,2,3) #두 번째 원소 결측
x+1
a<-NULL #객체만 생성
a

id=c(1,2,3,4,5)
sex=c('여','남','남','남','여')
height=c(165,183,175,178,167)
weight=c(54, 78, 68, 72, 52)
healthdata=data.frame(id,sex,height,weight) #데이터프레임 생성
healthdata
write.table(healthdata,"healthdata.txt",quote=FALSE,append=FALSE)

getwd()
setwd("C:\Users\jkds5\OneDrive\바탕 화면\사회과학통계분석1")
getwd()
write.table(healthdata,"healthdata.txt",quote=FALSE,append=FALSE)

mydata<-read.table("healthdata.txt",header=T)
mydata

write.csv(healthdata,"healthdata.csv",row.names=FALSE)
mydata2<-read.csv("healthdata.csv",header=TRUE)
mydata2

plus=function(a,b){ #사용자 지정 함수생성
c<-a+b
return(c)
}
100+200
plus(100,200)
