setwd("c:/Rdata") # C����̺� Rdata���ϳ�
library(dplyr)    # dplyr��Ű�� �ε�
data <- read.csv("winnerDrink.csv")  # winner���� winnerDrink.csv������ �ҷ���
head(data)       # �ҷ��� ������ Ȯ��
str(data)        # CATEGORY������ chr �������� Ȯ��
                 # �����ʹ� 10���� ������ 166���� object�� �����Ǿ� ����
View(data)       # ������ ����

data1 = data%>%                       # ���������� ������ ���� 46/166
  filter(CATEGORY == "����������")
data2 = data%>%                       # �Ϲ�ź������ ������ ���� 60/166
  filter(CATEGORY == "�Ϲ�ź������")
data3 = data%>%                       # ������ ������ ���� 60/166
  filter(CATEGORY == "������")

head(data1)
head(data2)
head(data3)

# ������ ������׷�
hist(data1$QTY)
hist(data2$QTY)
hist(data3$QTY)

# ���Լ� �׽�Ʈ
shapiro.test(data1$QTY) # p-value = 0.0001
shapiro.test(data2$QTY) # p-value = 0.01
shapiro.test(data3$QTY) # p-value = 0.08
# -> �̸����� ����Ȯ�� p-value�� ������� ���� �Ǻ��Ҽ� ����.

# ������� �м��� ���� cha ����CATEGORY�� �����Ͽ� �ٽ� ����
data1=data1[,c(-3)]
data2=data2[,c(-3)]
data3=data3[,c(-3)]

# �Ǹŷ� ������� �м�
cor(data1)
cor(data2)
cor(data3)

# ������ �����Ϳ� ���� ȸ�ͺм��� ����
out1=lm(QTY~.,data=data1)
out2=lm(QTY~.,data=data2)
out3=lm(QTY~.,data=data3)

# ���������� ȸ�ͺм� �׷���
plot(out1)
plot(out2)
plot(out3)

# (��������ȭ)�������� ����� ����. both�̹ǵ� �ܰ�
both1=step(out1,direction="both",trcce=FALSE) # ���� AIC=542.58
both2=step(out2,direction="both",trcce=FALSE) # ���� AIC=761.21
both3=step(out3,direction="both",trcce=FALSE) # ���� AIC=654.38

# F������ ���� ��������  F = (��������)/(��������)
anova(both1)
anova(both2)
anova(both3)

# ȸ�ͽ�
summary(both1)
summary(both2)
summary(both3)
# ���������� R-squared: 0.8229    �Ǹŷ� = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY
# �Ϲ�ź������ R-squared: 0.8846  �Ǹŷ� = 410.7761+68.6257(X)-73.2499(ITEM_CNT)-2.4391(PRICE)+93.8410(MAXTEMP)+0.0223(SALEDAY)
# ������ R-squared: 0.8814        �Ǹŷ� = 407.0016-19.8398(ITEM_CNT)+50.1766(MAXTEMP)+0.0114(SALEDAY)-0.0065

# ȸ�ͽ��� ���� ���� �Ǹŷ��� �ݿø��Ͽ� pred�� ����
pred1 = data1 %>%
  mutate(pred_QTY = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY)%>%
  summarise(QTY,round(pred_QTY))
pred2 = data2 %>%
  mutate(pred_QTY = 410.7761+68.6257*X-73.2499*ITEM_CNT-2.4391*PRICE+93.8410*MAXTEMP+0.0223*SALEDAY)%>%
  summarise(QTY,round(pred_QTY))
pred3 = data3 %>%
  mutate(pred_QTY = 407.0016-19.8398*ITEM_CNT+50.1766*MAXTEMP+0.0114*SALEDAY-0.0065*RAIN_DAY)%>%
  summarise(QTY,round(pred_QTY))

# ���� �Ǹŷ��� ���� �Ǹŷ� ��
pred1
pred2
pred3

# �׷����� ���� X=Y�׷����� ���ϻ��� �� �� ����
plot(pred1)
plot(pred2)
plot(pred3)