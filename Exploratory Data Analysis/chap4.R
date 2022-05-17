fastfood <- read.csv("Fastfood.csv", header=T)
fastfood

attach(fastfood)
str(fastfood)
boxplot(fastfood)

summary(fastfood)
