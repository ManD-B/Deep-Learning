setwd("c:/Rdata") # C드라이브 Rdata파일내
library(dplyr)    # dplyr패키지 로드
data <- read.csv("winnerDrink.csv")  # winner팀의 winnerDrink.csv파일을 불러옴
head(data)       # 불러온 데이터 확인
str(data)        # CATEGORY값만이 chr 형식임을 확인
                 # 데이터는 10개의 변수와 166개의 object로 구성되어 있음
View(data)       # 데이터 보기

data1 = data%>%                       # 에너지음료 데이터 추출 46/166
  filter(CATEGORY == "에너지음료")
data2 = data%>%                       # 일반탄산음료 데이터 추출 60/166
  filter(CATEGORY == "일반탄산음료")
data3 = data%>%                       # 차음료 데이터 추출 60/166
  filter(CATEGORY == "차음료")

head(data1)
head(data2)
head(data3)

# 각각의 히스토그램
hist(data1$QTY)
hist(data2$QTY)
hist(data3$QTY)

# 정규성 테스트
shapiro.test(data1$QTY) # p-value = 0.0001
shapiro.test(data2$QTY) # p-value = 0.01
shapiro.test(data3$QTY) # p-value = 0.08
# -> 이를통해 유의확률 p-value가 어느정도 인지 판별할수 있음.

# 상관관계 분석을 위해 cha 값인CATEGORY를 제외하여 다시 저장
data1=data1[,c(-3)]
data2=data2[,c(-3)]
data3=data3[,c(-3)]

# 판매량 상관관계 분석
cor(data1)
cor(data2)
cor(data3)

# 각각의 데이터에 대해 회귀분석을 적용
out1=lm(QTY~.,data=data1)
out2=lm(QTY~.,data=data2)
out3=lm(QTY~.,data=data3)

# 각데이터의 회귀분석 그래프
plot(out1)
plot(out2)
plot(out3)

# (모형간소화)변수선택 방법을 지정. both이므도 단계
both1=step(out1,direction="both",trcce=FALSE) # 최종 AIC=542.58
both2=step(out2,direction="both",trcce=FALSE) # 최종 AIC=761.21
both3=step(out3,direction="both",trcce=FALSE) # 최종 AIC=654.38

# F분포를 통한 가설검정  F = (군간변동)/(군내변동)
anova(both1)
anova(both2)
anova(both3)

# 회귀식
summary(both1)
summary(both2)
summary(both3)
# 에너지음료 R-squared: 0.8229    판매량 = -66.7442-3.4651(PRICE)+7.7444(MAXTEMP)+0.3500(SALEDAY)
# 일반탄산음료 R-squared: 0.8846  판매량 = 410.7761+68.6257(X)-73.2499(ITEM_CNT)-2.4391(PRICE)+93.8410(MAXTEMP)+0.0223(SALEDAY)
# 차음료 R-squared: 0.8814        판매량 = 407.0016-19.8398(ITEM_CNT)+50.1766(MAXTEMP)+0.0114(SALEDAY)-0.0065

#data1


#QCY1 = -66.7442-3.4651(PRICE)+7.7444(MAXTEMP)+0.3500(SALEDAY)
#QCY2 = 410.7761+68.6257(X)-73.2499(ITEM_CNT)-2.4391(PRICE)+93.8410(MAXTEMP)+0.0223(SALEDAY)
#QCY3 = 407.0016-19.8398(ITEM_CNT)+50.1766(MAXTEMP)+0.0114(SALEDAY)-0.0065(RAIN_DAY)