# 3)
z <- rbinom(400,1,0.5)
x <- (1 - z)*rnorm(400,-2,1) + z*rnorm(400,2,1)

# 4)
# 수직축(y/x)를 y로 바꿔주기위해 y/x에 x를 곱하면 된다.
# 이 때 기울기가 0.75이기 때문에 산점도의 회귀선은 우상향하게 될 것

# 5)

# 6)
par(mfrow=c(1,4))

# 정규모집단에서 추출된 크기 40,160의 모의생성 자료에 대한 정규확률플롯
qqnorm(rnorm(40,100,15))
qqnorm(rnorm(40,100,15))
qqnorm(rnorm(160,100,15))
qqnorm(rnorm(160,100,15))


# 혼합집단에서 추출된 크기 40,160의 모의생성 자료에 대한 정규확률플롯
qqnorm(c(rnorm(20,70,15), rnorm(20,130,15)))
qqnorm(c(rnorm(20,70,15), rnorm(20,130,15)))
qqnorm(c(rnorm(80,70,15), rnorm(80,130,15)))
qqnorm(c(rnorm(80,70,15), rnorm(80,130,15)))


# 특이점이 내재하는 크기 40,160의 모의생성 자료에 대한 정규확률플롯
qqnorm(c(25,175,rnorm(38,100,15)))
qqnorm(c(25,175,rnorm(38,100,15)))
qqnorm(c(25,175,rnorm(158,100,15)))
qqnorm(c(25,175,rnorm(158,100,15)))


# 꼬리가 짧은 분포에서 나온 크기 40,160의 모의생성 자료에 대한 정규확률플롯
qqnorm(runif(40,80,120))
qqnorm(runif(40,80,120))
qqnorm(runif(160,80,120))
qqnorm(runif(160,80,120))

# 꼬리가 긴 분포에서 나온 크기 40,160의 모의생성 자료의 경우
qqnorm(c(rexp(20,1), -rexp(20,1)))
qqnorm(c(rexp(20,1), -rexp(20,1)))
qqnorm(c(rexp(80,1), -rexp(80,1)))
qqnorm(c(rexp(80,1), -rexp(80,1)))


# 큰 값 쪽으로 긴 꼬리를 뻗은 기울어진 분포에서 나온 
# 크기 40,160의 모의생성 자료에 대한 정규확률플롯
qqnorm(exp(rnorm(40,5,1)))
qqnorm(exp(rnorm(40,5,1)))
qqnorm(exp(rnorm(80,5,1)))
qqnorm(exp(rnorm(80,5,1)))


# 작은 값 쪽으로 긴 꼬리를 뻗은 기울어진 분포에서 나온
# 크기 40,160의 모의생성 자료에 대한 정규확률플롯
qqnorm(1500 - exp(rnorm(40,5,1)))
qqnorm(1500 - exp(rnorm(40,5,1)))
qqnorm(1500 - exp(rnorm(80,5,1)))
qqnorm(1500 - exp(rnorm(80,5,1)))
