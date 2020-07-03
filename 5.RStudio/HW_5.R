# Q1. ggplot2 패키지의 mpg 데이터를 사용할 수 있도록 불러온 뒤 복사본을 만드세요.
mpg <- as.data.frame(ggplot2::mpg)         # mpg 데이터 불러오기
mpg_new <- mpg                             # 복사본 만들기

# Q2. 복사본 데이터를 이용해서 cty는 city로, hwy는 highway로 변수명을 수정하세요.
mpg_new <- rename(mpg_new, city = cty)     # cty를 city로 수정
mpg_new <- rename(mpg_new, highway = hwy)  # hwy를 highway로 수정

# Q3. 데이터 일부를 출력해서 변수명이 바뀌었는지 확인해 보세요.
# 아래와 같은 결과물이 출력되어야 합니다.
head(mpg_new)                              # 데이터 일부 출력

# 분석 도전!
# 문제1. ggplot2의 midwest 데이터를 데이터 프레임 형태로 불러와서 데이터의 특성을 파악하세요.
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

# 문제2. poptotal(전체 인구)을 total로, popasian(아시아 인구)을 asian으로 변수명을 수정하세요.
library(dplyr)
midwest <- rename(midwest, total = poptotal)
midwest <- rename(midwest, asian = popasian)

# 문제3. total, asian 변수를 이용해 '전체 인구 대비 아시아 인구 백분율' 파생변수를 만들고, 히스토그램을 만들어 도시들이 어떻게 분포하는지 살펴보세요.
midwest$ratio <- midwest$asian/midwest$total*100
hist(midwest$ratio)

# 문제4. 아시아 인구 백분율 전체 평균을 구하고, 평균을 초과하면 "large", 그 외에는 "small"을 부여하는 파생변수를 만들어 보세요.
mean(midwest$ratio)
midwest$group <- ifelse(midwest$ratio > 0.4872462, "large", "small")

# 문제5. "large"와 "small"에 해당하는 지역이 얼마나 되는지, 빈도표와 빈도 막대 그래프를 만들어 확인해 보세요.
table(midwest$group)
library(ggplot2)
qplot(midwest$group)