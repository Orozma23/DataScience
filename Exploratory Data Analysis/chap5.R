getwd()
setwd("C:\\Users\\jkds5\\OneDrive\\바탕 화면\\탐색적 자료분석")

install.packages("moments")
library(moments)

data5 <- read.table("nursing.txt", header=T); data5


#BED(numbers of beds in home)
#histogram
hist(data5$BED, main = "BED", freq=F, nclass=20, col="yellow")
lines(density(data5$BED), col="red")
#skewness & kurtosis
skewness(data5$BED)
kurtosis(data5$BED)
#histogram of changed power
hist(log(data5$BED), main = "BED(Log)", freq=F, nclass=20, col="green")
lines(density(log(data5$BED)), col="red")


#MCDAYS(annual medical in patient days(hundreds))
#histogram
hist(data5$MCDAYS, main = "MCDAYS", freq=F, nclass=20, col="yellow")
lines(density(data5$MCDAYS), col="red")
#skewness & kurtosis
skewness(data5$MCDAYS)
kurtosis(data5$MCDAYS)
#histogram of changed power
hist(sqrt(data5$MCDAYS), main = "MCDAYS(sqrt)", freq=F, nclass=20, col="green")
lines(density(sqrt(data5$MCDAYS)), col="red")


#TDAYS(annual total patient days(hundreds))
#histogram
hist(data5$TDAYS, main = "TDAYS", freq=F, nclass=20, col="yellow")
lines(density(data5$TDAYS), col="red")
#skewness & kurtosis
skewness(data5$TDAYS)
kurtosis(data5$TDAYS)
#histogram of changed power
hist(log(data5$TDAYS), main = "TDAYS(Log)", freq=F, nclass=20, col="green")
lines(density(log(data5$TDAYS)), col="red")


#PCREV(PCREV = annual total patient care revenue($hundreds))
#histogram
hist(data5$PCREV, main = "PCREV", freq=F, nclass=20, col="yellow")
lines(density(data5$PCREV), col="red")
#skewness & kurtosis
skewness(data5$PCREv)
kurtosis(data5$PCREV)
#histogram of changed power
hist(sqrt(data5$PCREV), main = "PCREV(sqrt)", freq=F, nclass=20, col="green")
lines(density(sqrt(data5$PCREV)), col="red")


#NSAL(annual nursing salaries($hundreds))
#histogram
hist(data5$NSAL, main = "NSAL", freq=F, nclass=20, col="yellow")
lines(density(data5$NSAL), col="red")
#skewness & kurtosis
skewness(data5$NSAL)
kurtosis(data5$NSAL)
#histogram of changed power
hist(log(data5$NSAL), main = "NSAL(Log)", freq=F, nclass=20, col="green")
lines(density(log(data5$NSAL)), col="red")


#FEXP(annual facilities expenditures($hundreds))
#histogram
hist(data5$FEXP, main = "FEXP", freq=F, nclass=20, col="yellow")
lines(density(data5$FEXP), col="red")
#skewness & kurtosis
skewness(data5$FEXP)
kurtosis(data5$FEXP)
#histogram of changed power
hist(sqrt(data5$FEXP), main = "FEXP(sqrt)", freq=F, nclass=20, col="green")
lines(density(sqrt(data5$FEXP)), col="red")
