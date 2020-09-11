setwd("c:/Rdata") # C드라이브 Rdata파일내
library(dplyr)    # dplyr패키지 로드
data <- read.csv("Project.csv")  # Project.csv파일을 불러옴
head(data)       # 불러온 데이터 확인
str(data)        
                 
View(data)       # 데이터 보기

# 각각의 히스토그램
library(car)
boxplot(data$Nas,horizontal = T)
shapiro.test(data$Nas)
tt=powerTransform(data$Nas)
kk=data$Nas^tt
hist(kk)
str(data)
cor(data)
mean(data$Nas)
sd(data$Nas)
IQR(data$Nas)
var(data$Nas)
summary(data$Nas)

kk=data$Dow
IQR(data$Dow)

boxplot(data$Nas)$stats
data$Nas <- ifelse(data$Nas < 3160.780 | data$Nas > 6959.960, NA, data$Nas)
table(is.na(data$Nas))

boxplot(kk,horizontal = T,col=2)
shapiro.test(kk)
Nas_log = log(data$Nas)
Nas_sqrt = sqrt(data$Nas)
shapiro.test(Nas_log)
shapiro.test(Nas_sqrt)
data_z = transform(scale(data))
head(data_z)
View(data_z)
hist(data_z$Dow)
# 정규성 테스트
shapiro.test(data$Nas) # p-value = 2.936e-05
# -> 이를통해 유의확률 p-value가 어느정도 인지 판별할수 있음.

# 상관관계 분석을 위해 필요한 값들만 다시 저장
#data=data[,-11:-19]

# 판매량 상관관계 분석
cor(data)
cor(data_z)
# -> S.P500, silver가 양의상관관계, CNY, EUR, JPY, USD, 변동률이 음의 상관관계임

# 데이터에 대해 회귀분석을 적용
out=lm(Dow~.,data=data)

# 데이터의 회귀분석 그래프
plot(out)

plot(data$Dow, data$S.P)
plot(data$Dow, data$USD)
plot(data$Dow, data$JYP)
library(car)     # car패키지 로드
vif(out)         # 다중공선성 확인


# (모형간소화)변수선택 방법을 지정. both이므도 단계
both=step(out,direction="both",trcce=FALSE) # 최종 AIC=2673.91

# F분포를 통한 가설검정  F = (군간변동)/(군내변동)
anova(both)

# 회귀식
summary(both)
vif(both)

# 에너지음료 R-squared: 0.8229    판매량 = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY
# 일반탄산음료 R-squared: 0.8846  판매량 = 410.7761+68.6257(X)-73.2499(ITEM_CNT)-2.4391(PRICE)+93.8410(MAXTEMP)+0.0223(SALEDAY)
# 차음료 R-squared: 0.8814        판매량 = 407.0016-19.8398(ITEM_CNT)+50.1766(MAXTEMP)+0.0114(SALEDAY)-0.0065

# 회귀식을 통한 예측 판매량을 반올림하여 pred에 저장
pred = data %>%
  mutate(pred_QTY = round(-3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY))%>%
  summarise(QTY,pred_QTY)


# 실제 판매량과 예측 판매량 비교
pred

# 그래프를 통한 X=Y그래프에 유사함을 볼 수 있음
plot(pred)


# 실제 판매량과 예측값의 각각의 ACCURACY
acc <- pred %>%
  mutate(ACCURACY = round((abs(pred_QTY/QTY * 100)), 2))%>%
  summarise(ACCURACY)
acc <- ifelse(acc$ACCURACY > 100.00, acc$ACCURACY <- round((abs(pred$QTY/pred$pred_QTY * 100)), 2), acc$ACCURACY <-round((abs(pred$pred_QTY/pred$QTY * 100)), 2))
acc

# ACCURACY 평균
mean(acc)