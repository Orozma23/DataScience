world.temp <-read.table("WorldTemperature_Mean.txt", header=T)
twoway.model <- medpolish(world.temp)
twoway.model

attach(twoway.model)
comparison <- matrix(row, ncol=1)%*%matrix(col,nrow=1)/overall
plot(residuals ~ comparison, xlim=c(-15,15), ylim=c(-15,15))
boxplot(residuals, ylab="residuals")

round(residuals[order(row),],1)

consumption <- read.table("household.txt", header=T)
twoway.out <- medpolish(consumption)
attach(twoway.out)
comparison <- matrix(row, ncol=1)%*%matrix(col,nrow=1)/overall
plot(residuals ~ comparison)
detach(twoway.out)

twoway.log.out <- medpolish(log(consumption))
attach(twoway.log.out)
comparison <- matrix(row, ncol=1)%*%matrix(col,nrow=1)/overall
plot(residuals ~ comparison)

twoway.log.out

round(residuals, 2)

data(UCBAdmissions)
UCBAdmissions
Tab1 <- UCBAdmissions[1,,]
Tab2 <- UCBAdmissions[2,,]
Tab <- Tab1 + Tab2
addmargins(Tab)

barplot(Tab, legend=rownames(Tab))

Tab.col <- apply(Tab,2,sum)
Tab.C <- Tab %*% diag(1/Tab.col)*100
colnames(Tab.C) <- c("A", "B", "C", "D", "E", "F")
rownames(Tab.C) <- c("Male", "Female")
x11(); barplot(Tab.C, legend=rownames(Tab.C))

barplot(t(Tab), beside=T, legend=colnames(Tab))
Tab.row <- apply(Tab,1,sum)
Tab.R <- diag(1/Tab.row) %*% Tab*100
colnames(Tab.R) <- c("A", "B", "C", "D", "E", "F")
rownames(Tab.R) <- c("Male", "Female")
x11(); barplot(t(Tab.R), beside=T, legend=colnames(Tab.R))

Tab.M <- UCBAdmissions[,1,]
addmargins(Tab.M)
Tab.F <- UCBAdmissions[,2,]
addmargins(Tab.F)

Tab.M.col <- apply(Tab.M, 2, sum)
Tab.M.C <- Tab.M %*% diag(1/Tab.M.col) * 100
colnames(Tab.M.C) <- c("A", "B", "C", "D", "E", "F")
x11(); barplot(Tab.M.C, legend=rownames(Tab.M.C), main="Male")
Tab.F.col <- apply(Tab.F, 2, sum)
Tab.F.C <- Tab.M %*% diag(1/Tab.F.col) * 100
colnames(Tab.F.C) <- c("A", "B", "C", "D", "E", "F")
x11(); barplot(Tab.F.C, legend=rownames(Tab.F.C), main="Female")

mosaicplot(~Dept+Gender, data=UCBAdmissions, color=T)
x11(); mosaicplot(~Gender+Dept, data=UCBAdmissions, color=T)

Tab.M <- as.table(UCBAdmissions[,1,])
Tab.F <- as.table(UCBAdmissions[,2,])
x11(); mosaicplot(~Dept+Admit, data=Tab.M, color=T, main="Male")
x11(); mosaicplot(~Dept+Admit, data=Tab.F, color=T, main="Female")

mosaicplot(~Dept+Admit+Gender, data=UCBAdmissions, off=2, color=T)
x11(); mosaicplot(~Gender+Admit, data=UCBAdmissions, off=2, color=T)
