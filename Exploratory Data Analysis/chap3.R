setwd("C:\\실습 데이터\\3장")
singers <- read.csv("singers.csv", header=T)
singers

###############################################

# Ű ??ü ?ٱ??ٱ׸?
str(singers)
attach(singers)
stem(height)

# Ű ??ü ???????׷?
hist(height, nclass=15, right=F, freq=F)
lines(density(height), col="red")

###############################################

# ??Ʈ?? ?ٱ??ٱ׸?
str(singers)
attach(singers)
# ???? ?? ????: Alto, Bass, Soprano, Tenor
tapply(height, part, stem)

# ??Ʈ?? ???????׷?
