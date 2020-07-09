setwd("c:/Rdata")
library(dplyr)
data <- read.csv("winnerDrink.csv")
head(data)
str(data)
View(data)

data1 = data%>% 
  filter(CATEGORY == "에너지음료")
data2 = data%>% 
  filter(CATEGORY == "일반탄산음료")
data3 = data%>% 
  filter(CATEGORY == "차음료")

head(data1)
head(data2)
head(data3)

shapiro.test(data1$QTY)
shapiro.test(data2$QTY)
shapiro.test(data3$QTY)

hist(data1$QTY)
hist(data2$QTY)
hist(data3$QTY)

data1=data1[,c(-3)]
data2=data2[,c(-3)]
data3=data3[,c(-3)]

cor(data1[,c(-3)])
cor(data2[,c(-3)])
cor(data3[,c(-3)])

out1=lm(QTY~.,data=data1)
out2=lm(QTY~.,data=data2)
out3=lm(QTY~.,data=data3)

both1=step(out1,direction="both",trcce=FALSE)
both2=step(out2,direction="both",trcce=FALSE)
both3=step(out3,direction="both",trcce=FALSE)

summary(both1)
summary(both2)
summary(both3)


both1=step(out1,direction="both",trcce=FALSE)
both2=step(out2,direction="both",trcce=FALSE)



summary

View(attitude)
cov(attitude)
cor(attitude)
with(attitude,cor.test(rating, complaints))
cor.test(attitude$rating,attitude$complaints)
plot(attitude$rating,attitude$complaints)

fa=c(150,160,170,180,190)
su=c(176,179,182,178,185)
fasu=data.frame(fa,su)
fasu
lm(su~fa,data=fasu)

data
View(data)
out=lm(QTY~YM,data=data)
summary(out)
plot(QTY~YM,data=cars,col="blue")
abline(out,col="red")
out1=lm(QTY~YM+0,data=data)
summary(lm(QTY~YM+0,data=data))
plot(out1)
par(mfrow=c(1,1))
plot(out1)
shapiro.test(data$QTY)
shapiro.test(log(data$QTY))
shapiro.test(sqrt(data$QTY))
out3=lm(sqrt(QTY)~YM+0,data=data)
summary(out3)
plot(out3)

out3$fitted.values
cbind(data$YM,out3$fitted.values)
new=data.frame(YM=data$YM)
cbind(new$YM,predict(out3,new,interval = "confidence"))
cbind(new$YM,predict(out3,new,interval = "prediction"))
      
#다중회귀분석
data=read.csv("winnerDrink.csv")
head(data)
out=lm(salary~experience+score,data=data)
summary(out)
plot(out)
cbind(data$experience, data$score, out$fitted.values)


summary(lm(rating~complaints+learning,data=attitude))
summary(lm(rating~learning,data=attitude))
out=lm(rating~.-critical,data=attitude)
summary(out)
backward=step(out,direction="backward",trace=FALSE)
summary(backward)
both=step(out,direction="both",trace=FALSE)
summary(both)
##All subset method
install.packages("leaps")
library(leaps)
leaps=regsubsets(rating~.,data=attitude,nbest=5)
summary(leaps)

plot(leaps,scale='bic')
out_bic=lm(rating~complaints,data=attitude)
summary(out_bic)
plot(leaps,scale='Cp')
out_cp=lm(rating~complaints+learning,data=attitude)
summary(out_cp)
plot(leaps,scale="adjr2")
out_adjr=lm(rating~complaints+learning+advance,data=attitude)
summary(out_adjr)

