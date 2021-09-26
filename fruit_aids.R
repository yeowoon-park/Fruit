library(plyr)
library(dplyr)
library(psych)
library(micEconAids)
library(plm)
library(reshape2)

load(file = "p.695.fruit.RData")
fruit <- p.695.fruit
rm(p.695.fruit)
fruit <- fruit %>% filter(cate_new %in% c('감귤', '바나나', '사과', '포도'))

# 1. 일일 기준으로 소비자 전체가 구입한 품목별 과일 가격을 평균 (average.d) -------------------------------------
fruit.1 <- fruit %>% group_by(year, month, shop_day, cate_new) %>% summarise(average.d= mean(purchase))

# 2. average.d를 각 소비자가 구입한 품목별 일별 소비지출액에 나눔 (share.d) -------------------------------------
fruit.2 <- left_join(fruit, fruit.1, by = c('shop_day'='shop_day', 'cate_new'='cate_new')) %>% mutate(share.d = (purchase/average.d))

# 3. share.d를 1개월 단위로 합해 월별 소비지출액을 산출한 후, 이 값을 각 소비자가 지출한 월별 소비지출액에 나눔 (pjkt) -------------------------------------
fruit.3 <- fruit.2 %>% group_by(panel_code, year.x, month.x, cate_new) %>% 
  summarise(share.m = sum(share.d), purchase.m = sum(purchase), pjkt = (purchase.m/share.m))
fruit.3 <- rename(fruit.3, "year" = "year.x", "month" = "month.x")

# 4. 각 소비자의 월별 분석 대상 과일 품목 전체 지출 (total.purchase)  -------------------------------------
fruit.w.1 <- fruit %>% group_by(panel_code, year, month) %>% summarise(total.purchase = sum(purchase))

# 5. 각 소비자가 월별 품목별 과일에 지출한 금액을 total.purchase로 나눔 (wikt)  -------------------------------------
fruit.w.2 <- fruit %>% group_by(panel_code, year, month, cate_new) %>% summarise(total.purchase.fruit = sum(purchase))
fruit.w.3 <- left_join(fruit.w.2, fruit.w.1, by = c('panel_code'='panel_code','year'='year', 'month'='month')) %>% 
  mutate(wikt = (total.purchase.fruit/total.purchase))

# 6. aids.panel 데이터셋 생성 (1단계: 과일별) -------------------------------------
aids.panel <- inner_join(fruit.3, fruit.w.3, by = c('panel_code'='panel_code','year'='year', 'month'='month', 'cate_new'='cate_new'))
fruit.list <- unique(subset(aids.panel, select=c(cate_new)))
id <- unique(subset(aids.panel, select=c(panel_code, year, month)))
write.csv(aids.panel, file = "aids.panel.csv")

#총지출
expenditure <- unique(subset(aids.panel, select=c(panel_code, year, month, total.purchase)))
expenditure[is.na(expenditure)] <- 0

#감귤
tangerine <- aids.panel %>% filter(cate_new == '감귤')
tangerine.1 <- left_join(id, tangerine, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
tangerine.1$cate_new[is.na(tangerine.1$cate_new)] <- '감귤'
tangerine.1[is.na(tangerine.1)] <- 0
tangerine.2 <- subset(tangerine.1, select=c(panel_code, year, month, pjkt, wikt))
tangerine.2<- rename(tangerine.2, "감귤_p" = "pjkt", "감귤_w" = "wikt")

#딸기
straw <- aids.panel %>% filter(cate_new == '딸기')
straw.1 <- left_join(id, straw, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
straw.1$cate_new[is.na(straw.1$cate_new)] <- '딸기'
straw.1[is.na(straw.1)] <- 0
straw.2 <- subset(straw.1, select=c(panel_code, year, month, pjkt, wikt))
straw.2<- rename(straw.2, "딸기_p" = "pjkt", "딸기_w" = "wikt")

#바나나
banana <- aids.panel %>% filter(cate_new == '바나나')
banana.1 <- left_join(id, banana, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
banana.1$cate_new[is.na(banana.1$cate_new)] <- '바나나'
banana.1[is.na(banana.1)] <- 0
banana.2 <- subset(banana.1, select=c(panel_code, year, month, pjkt, wikt))
banana.2<- rename(banana.2, "바나나_p" = "pjkt", "바나나_w" = "wikt")

#사과
apple <- aids.panel %>% filter(cate_new == '사과')
apple.1 <- left_join(id, apple, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
apple.1$cate_new[is.na(apple.1$cate_new)] <- '사과'
apple.1[is.na(apple.1)] <- 0
apple.2 <- subset(apple.1, select=c(panel_code, year, month, pjkt, wikt))
apple.2<- rename(apple.2, "사과_p" = "pjkt", "사과_w" = "wikt")

#참다래
kiwi <- aids.panel %>% filter(cate_new == '참다래')
kiwi.1 <- left_join(id, kiwi, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
kiwi.1$cate_new[is.na(kiwi.1$cate_new)] <- '참다래'
kiwi.1[is.na(kiwi.1)] <- 0
kiwi.2 <- subset(kiwi.1, select=c(panel_code, year, month, pjkt, wikt))
kiwi.2<- rename(kiwi.2, "참다래_p" = "pjkt", "참다래_w" = "wikt")

#파인애플
pine <- aids.panel %>% filter(cate_new == '파인애플')
pine.1 <- left_join(id, pine, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
pine.1$cate_new[is.na(pine.1$cate_new)] <- '파인애플'
pine.1[is.na(pine.1)] <- 0
pine.2 <- subset(pine.1, select=c(panel_code, year, month, pjkt, wikt))
pine.2<- rename(pine.2, "파인애플_p" = "pjkt", "파인애플_w" = "wikt")

#오렌지
orange <- aids.panel %>% filter(cate_new == '오렌지')
orange.1 <- left_join(id, orange, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
orange.1$cate_new[is.na(orange.1$cate_new)] <- '오렌지'
orange.1[is.na(orange.1)] <- 0
orange.2 <- subset(orange.1, select=c(panel_code, year, month, pjkt, wikt))
orange.2<- rename(orange.2, "오렌지_p" = "pjkt", "오렌지_w" = "wikt")

#참외
cham <- aids.panel %>% filter(cate_new == '참외')
cham.1 <- left_join(id, cham, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
cham.1$cate_new[is.na(cham.1$cate_new)] <- '참외'
cham.1[is.na(cham.1)] <- 0
cham.2 <- subset(cham.1, select=c(panel_code, year, month, pjkt, wikt))
cham.2<- rename(cham.2, "참외_p" = "pjkt", "참외_w" = "wikt")

#포도
grape <- aids.panel %>% filter(cate_new == '포도')
grape.1 <- left_join(id, grape, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
grape.1$cate_new[is.na(grape.1$cate_new)] <- '포도'
grape.1[is.na(grape.1)] <- 0
grape.2 <- subset(grape.1, select=c(panel_code, year, month, pjkt, wikt))
grape.2<- rename(grape.2, "포도_p" = "pjkt", "포도_w" = "wikt")

#수박
water <- aids.panel %>% filter(cate_new == '수박')
water.1 <- left_join(id, water, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
water.1$cate_new[is.na(water.1$cate_new)] <- '수박'
water.1[is.na(water.1)] <- 0
water.2 <- subset(water.1, select=c(panel_code, year, month, pjkt, wikt))
water.2<- rename(water.2, "수박_p" = "pjkt", "수박_w" = "wikt")

#메론
melon <- aids.panel %>% filter(cate_new == '메론')
melon.1 <- left_join(id, melon, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
melon.1$cate_new[is.na(melon.1$cate_new)] <- '메론'
melon.1[is.na(melon.1)] <- 0
melon.2 <- subset(melon.1, select=c(panel_code, year, month, pjkt, wikt))
melon.2<- rename(melon.2, "메론_p" = "pjkt", "메론_w" = "wikt")

#복숭아
peach <- aids.panel %>% filter(cate_new == '복숭아')
peach.1 <- left_join(id, peach, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
peach.1$cate_new[is.na(peach.1$cate_new)] <- '복숭아'
peach.1[is.na(peach.1)] <- 0
peach.2 <- subset(peach.1, select=c(panel_code, year, month, pjkt, wikt))
peach.2<- rename(peach.2, "복숭아_p" = "pjkt", "복숭아_w" = "wikt")

#체리
cherry <- aids.panel %>% filter(cate_new == '체리')
cherry.1 <- left_join(id, cherry, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
cherry.1$cate_new[is.na(cherry.1$cate_new)] <- '체리'
cherry.1[is.na(cherry.1)] <- 0
cherry.2 <- subset(cherry.1, select=c(panel_code, year, month, pjkt, wikt))
cherry.2<- rename(cherry.2, "체리_p" = "pjkt", "체리_w" = "wikt")

#배
pear <- aids.panel %>% filter(cate_new == '배')
pear.1 <- left_join(id, pear, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
pear.1$cate_new[is.na(pear.1$cate_new)] <- '배'
pear.1[is.na(pear.1)] <- 0
pear.2 <- subset(pear.1, select=c(panel_code, year, month, pjkt, wikt))
pear.2<- rename(pear.2, "배_p" = "pjkt", "배_w" = "wikt")

#기타과일
extra <- aids.panel %>% filter(cate_new == '기타과일')
extra.1 <- left_join(id, extra, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
extra.1$cate_new[is.na(extra.1$cate_new)] <- '기타과일'
extra.1[is.na(extra.1)] <- 0
extra.2 <- subset(extra.1, select=c(panel_code, year, month, pjkt, wikt))
extra.2<- rename(extra.2, "기타과일_p" = "pjkt", "기타과일_w" = "wikt")

#단감
dangam <- aids.panel %>% filter(cate_new == '단감')
dangam.1 <- left_join(id, dangam, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
dangam.1$cate_new[is.na(dangam.1$cate_new)] <- '단감'
dangam.1[is.na(dangam.1)] <- 0
dangam.2 <- subset(dangam.1, select=c(panel_code, year, month, pjkt, wikt))
dangam.2<- rename(dangam.2, "단감_p" = "pjkt", "단감_w" = "wikt")

#떫은감
xdangam <- aids.panel %>% filter(cate_new == '떫은감')
xdangam.1 <- left_join(id, xdangam, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
xdangam.1$cate_new[is.na(xdangam.1$cate_new)] <- '떫은감'
xdangam.1[is.na(xdangam.1)] <- 0
xdangam.2 <- subset(xdangam.1, select=c(panel_code, year, month, pjkt, wikt))
xdangam.2<- rename(xdangam.2, "떫은감_p" = "pjkt", "떫은감_w" = "wikt")

#베리
berry <- aids.panel %>% filter(cate_new == '베리')
berry.1 <- left_join(id, berry, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
berry.1$cate_new[is.na(berry.1$cate_new)] <- '베리'
berry.1[is.na(berry.1)] <- 0
berry.2 <- subset(berry.1, select=c(panel_code, year, month, pjkt, wikt))
berry.2<- rename(berry.2, "베리_p" = "pjkt", "베리_w" = "wikt")

#자두
plum <- aids.panel %>% filter(cate_new == '자두')
plum.1 <- left_join(id, plum, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
plum.1$cate_new[is.na(plum.1$cate_new)] <- '자두'
plum.1[is.na(plum.1)] <- 0
plum.2 <- subset(plum.1, select=c(panel_code, year, month, pjkt, wikt))
plum.2<- rename(plum.2, "자두_p" = "pjkt", "자두_w" = "wikt")

#망고
mango <- aids.panel %>% filter(cate_new == '망고')
mango.1 <- left_join(id, mango, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
mango.1$cate_new[is.na(mango.1$cate_new)] <- '망고'
mango.1[is.na(mango.1)] <- 0
mango.2 <- subset(mango.1, select=c(panel_code, year, month, pjkt, wikt))
mango.2<- rename(mango.2, "망고_p" = "pjkt", "망고_w" = "wikt")

#자몽
jamong <- aids.panel %>% filter(cate_new == '자몽')
jamong.1 <- left_join(id, jamong, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
jamong.1$cate_new[is.na(jamong.1$cate_new)] <- '자몽'
jamong.1[is.na(jamong.1)] <- 0
jamong.2 <- subset(jamong.1, select=c(panel_code, year, month, pjkt, wikt))
jamong.2<- rename(jamong.2, "자몽_p" = "pjkt", "자몽_w" = "wikt")


# 6-1. aids.panel 데이터셋 생성 (1단계: 과일별 ; 결측치 제외) -------------------------------------

#총지출
a.expenditure <- unique(subset(aids.panel, select=c(panel_code, year, month, total.purchase)))

#감귤
a.tangerine <- aids.panel %>% filter(cate_new == '감귤')
a.tangerine <- subset(a.tangerine, select=c(panel_code, year, month, pjkt, wikt))
a.tangerine <- rename(a.tangerine, "감귤_p" = "pjkt", "감귤_w" = "wikt")
#딸기
a.straw <- aids.panel %>% filter(cate_new == '딸기')
a.straw  <- subset(a.straw , select=c(panel_code, year, month, pjkt, wikt))
#바나나
a.banana <- aids.panel %>% filter(cate_new == '바나나')
a.banana <- subset(a.banana , select=c(panel_code, year, month, pjkt, wikt))
a.banana <- rename(a.banana, "바나나_p" = "pjkt", "바나나_w" = "wikt")
#사과
a.apple <- aids.panel %>% filter(cate_new == '사과')
a.apple <- subset(a.apple , select=c(panel_code, year, month, pjkt, wikt))
a.apple <- rename(a.apple, "사과_p" = "pjkt", "사과_w" = "wikt")
#참다래
a.kiwi <- aids.panel %>% filter(cate_new == '참다래')
a.kiwi <- subset(a.kiwi , select=c(panel_code, year, month, pjkt, wikt))
#파인애플
a.pine <- aids.panel %>% filter(cate_new == '파인애플')
a.pine <- subset(a.pine , select=c(panel_code, year, month, pjkt, wikt))
#오렌지
a.orange <- aids.panel %>% filter(cate_new == '오렌지')
a.orange <- subset(a.orange , select=c(panel_code, year, month, pjkt, wikt))
#참외
a.cham <- aids.panel %>% filter(cate_new == '참외')
a.cham <- subset(a.cham , select=c(panel_code, year, month, pjkt, wikt))
#포도
a.grape <- aids.panel %>% filter(cate_new == '포도')
a.grape <- subset(a.grape , select=c(panel_code, year, month, pjkt, wikt))
a.grape <- rename(a.grape, "포도_p" = "pjkt", "포도_w" = "wikt")
#수박
a.water <- aids.panel %>% filter(cate_new == '수박')
a.water <- subset(a.water , select=c(panel_code, year, month, pjkt, wikt))
#메론
a.melon <- aids.panel %>% filter(cate_new == '메론')
a.melon <- subset(a.melon , select=c(panel_code, year, month, pjkt, wikt))
#복숭아
a.peach <- aids.panel %>% filter(cate_new == '복숭아')
a.pech <- subset(a.peach , select=c(panel_code, year, month, pjkt, wikt))
#체리
a.cherry <- aids.panel %>% filter(cate_new == '체리')
a.cherry <- subset(a.cherry , select=c(panel_code, year, month, pjkt, wikt))
#배
a.pear <- aids.panel %>% filter(cate_new == '배')
a.pear <- subset(a.pear , select=c(panel_code, year, month, pjkt, wikt))
#기타과일
a.extra <- aids.panel %>% filter(cate_new == '기타과일')
a.extra <- subset(a.extra , select=c(panel_code, year, month, pjkt, wikt))
#단감
a.dangam <- aids.panel %>% filter(cate_new == '단감')
a.dangam <- subset(a.dangam , select=c(panel_code, year, month, pjkt, wikt))
#떫은감
a.xdangam <- aids.panel %>% filter(cate_new == '떫은감')
a.xdangam <- subset(a.dangam , select=c(panel_code, year, month, pjkt, wikt))
#베리
a.berry <- aids.panel %>% filter(cate_new == '베리')
a.berry <- subset(a.berry , select=c(panel_code, year, month, pjkt, wikt))
#자두
a.plum <- aids.panel %>% filter(cate_new == '자두')
a.plum <- subset(a.plum , select=c(panel_code, year, month, pjkt, wikt))
#망고
a.mango <- aids.panel %>% filter(cate_new == '망고')
a.mango <- subset(a.mango , select=c(panel_code, year, month, pjkt, wikt))
#자몽
a.jamong <- aids.panel %>% filter(cate_new == '자몽')
a.jamong <- subset(a.jamong , select=c(panel_code, year, month, pjkt, wikt))

# 7. aids.panel 데이터셋 생성 (2단계: 데이터 합치기) :aids -------------------------------------
aids.1 <- inner_join(expenditure, tangerine.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.2 <- inner_join(aids.1, straw.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.3 <- inner_join(aids.2, banana.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.4 <- inner_join(aids.3, apple.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.5 <- inner_join(aids.4, kiwi.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.6 <- inner_join(aids.5, pine.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.7 <- inner_join(aids.6, orange.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.8 <- inner_join(aids.7, cham.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.9 <- inner_join(aids.8, grape.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.10 <- inner_join(aids.9, water.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.11 <- inner_join(aids.10, melon.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.12 <- inner_join(aids.11, peach.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.13 <- inner_join(aids.12, cherry.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.14 <- inner_join(aids.13, pear.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.15 <- inner_join(aids.14, extra.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.16 <- inner_join(aids.15, dangam.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.17 <- inner_join(aids.16, xdangam.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.18 <- inner_join(aids.17, berry.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.19 <- inner_join(aids.18, plum.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.20 <- inner_join(aids.19, mango.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.21 <- inner_join(aids.20, jamong.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))

aids <- aids.21

# 7-a. aids.panel 데이터셋 생성 (2단계: 데이터 합치기 ; 결측치 있는애들) :aids -------------------------------------
a.aids.1 <- inner_join(a.expenditure, a.banana, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
a.aids.2 <- inner_join(a.aids.1, a.apple, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
a.aids.3 <- inner_join(a.aids.2, a.tangerine, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
a.aids.4 <- inner_join(a.aids.3, a.grape, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
-------------------------------------------------------------------------------------------------------------------
a.aids.5 <- inner_join(a.aids.4, a.straw, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.6 <- inner_join(aids.5, pine.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.7 <- inner_join(aids.6, orange.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.8 <- inner_join(aids.7, cham.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.9 <- inner_join(aids.8, grape.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.10 <- inner_join(aids.9, water.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.11 <- inner_join(aids.10, melon.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.12 <- inner_join(aids.11, peach.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.13 <- inner_join(aids.12, cherry.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.14 <- inner_join(aids.13, pear.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.15 <- inner_join(aids.14, extra.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.16 <- inner_join(aids.15, dangam.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.17 <- inner_join(aids.16, xdangam.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.18 <- inner_join(aids.17, berry.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.19 <- inner_join(aids.18, plum.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.20 <- inner_join(aids.19, mango.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))
aids.21 <- inner_join(aids.20, jamong.2, by = c('panel_code'='panel_code','year'='year', 'month'='month'))

aids <- aids.21


# 8. aids 모형 돌리기-a
a.aids.4 <- a.aids.4 %>% mutate(E = 바나나_p + 사과_p + 감귤_p + 포도_p)
priceNames <- c("바나나_p", "사과_p", "감귤_p", "포도_p")

shareNames <- c("바나나_w", "사과_w", "감귤_w", "포도_w")

a.aids.4 <- as.data.frame(a.aids.4)


laaidsResult <- aidsEst(priceNames, shareNames, "E", data = a.aids.4, priceIndex ="S")
print(laaidsResult)
