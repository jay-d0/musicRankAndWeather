#### 데이터 로드 ####
gaon_chart <- read.csv("gaon chart.csv", fileEncoding = "UTF-8", encoding = "cp949")
weather <- read.csv("weather2012_2022.csv")

#### 데이터 전처리####
#### gaon_chart data 파악####
hist(gaon_chart$Year)
hist(gaon_chart$week)

library(dplyr)
# 자주 등장하는 상위 20개 label company에 대해 연구 진행
label <- gaon_chart %>% 
  group_by(Production) %>% 
  summarise(n_production = n()) %>% 
  arrange(-n_production) %>% 
  head(20)

# 자주 등장하는 상위 20개 label list
label_list <- label$Production

# 자주 등장하는 상위 100명 Artists에 대해 연구 진행
artist <- gaon_chart %>% 
  group_by(Artist) %>% 
  summarise(n_artist = n()) %>% 
  arrange(-n_artist) %>% 
  head(100)

# 자주 등장하는 상위 100명 Artists list
artist_list <- artist$Artist

# label data: Rank_change data가 없는 data 제거
hist(gaon_chart$Rank_change, xlab='Rank_change', main='Rank_change')
gaon_chart <- gaon_chart %>% 
  filter(!is.na(Rank_change))

#### weather data 파악####
hist(weather$Year)
table(weather$Year)
# 결측1: 16년 42주~ 19년 7주까지의 data
# 결측2: 12년 8개 주의 data
hist(weather$week)
hist(weather$Temperature)
hist(weather$Relative.Temperature)
hist(weather$Wind)
hist(weather$Rel..humidity)
hist(weather$Dew.Point)
hist(weather$Pressure)
hist(weather$Daily.minimum.temperature)
hist(weather$Daily.maximum.temperature)
hist(weather$Clear)
hist(weather$FogRain) # log
hist(weather$FogSnowfall) #log
hist(weather$FogSnowfallRain) #log
hist(weather$FogThunderstormRain) #log
hist(weather$HailFogThunderstormRain) #log
hist(weather$Rain)
hist(weather$Snowfall) # log
hist(weather$SnowfallRain) #log
hist(weather$Thunderstorm) #log
hist(weather$ThunderstormRain) #log
hist(weather$ThunderstormSnowfallRain) #log

#### 데이터 변환 ####
# log 적용
range(weather$FogRain)
weather$ln_FogRain <- log(weather$FogRain + 1)

range(weather$FogSnowfall)
weather$ln_FogSnowfall <- log(weather$FogSnowfall + 1)

range(weather$FogSnowfallRain)
weather$ln_FogSnowfallRain <- log(weather$FogSnowfall + 1)

range(weather$FogThunderstormRain)
weather$ln_FogThunderstormRain <- log(weather$FogThunderstormRain + 1)

range(weather$HailFogThunderstormRain)
weather$ln_HailFogThunderstormRain <- log(weather$HailFogThunderstormRain + 1)

range(weather$Snowfall)
weather$ln_Snowfall <- log(weather$Snowfall + 1)

range(weather$SnowfallRain)
weather$ln_SnowfallRain <- log(weather$SnowfallRain + 1)

range(weather$Thunderstorm)
weather$ln_Thunderstorm <- log(weather$Thunderstorm + 1)

range(weather$ThunderstormRain)
weather$ln_ThunderstormRain <- log(weather$ThunderstormRain + 1)

range(weather$ThunderstormSnowfallRain)
weather$ln_ThunderstormSnowfallRain <- log(weather$ThunderstormSnowfallRain + 1)

#### 데이터 Linear Regression, moderation ####
# 고정효과가 없는 model
df <- merge(gaon_chart, weather, how='inner', by=c('Year', 'week'))
model0 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
             -Artist -Production, df)
summary(model0)

# Artist에 의한 고정효과
model0a <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Production, df)
summary(model0a)

## 너무 많다!
# 2156가지의 고정효과
length(unique(df$Artist)) 
# 977가지의 고정효과
length(unique(df$Production))

# focusing: 상위 기업들의 고정효과.
# 자주 등장하는 친구들 위주로 보자.
# Label별, Artist별 Rank_change
# 상위 20개 회사의 label, 상위 100명의 artist
label_list
artist_list

chart_label <- gaon_chart[gaon_chart$Production %in% label_list,]
chart_artist <- gaon_chart[gaon_chart$Artist %in% artist_list,]

label_df <- merge(chart_label, weather, how='inner', by=c('Year', 'week'))
artist_df <- merge(chart_artist, weather, how='inner', by=c('Year', 'week'))

model0 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
             -Artist, label_df)
model01 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Production, artist_df)

summary(model0)
summary(model01)

#### na 변수 제거: ln_FogSnowfallRain는 충분히 많은 data가 존재하지 않습니다.
label_df <- label_df[-28]
artist_df <- artist_df[-28]

model0 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
             -Artist, label_df)
model01 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Production, artist_df)

summary(model0)
summary(model01)

#### factor 변수 week
label_df$week <- as.factor(label_df$week)
artist_df$week <- as.factor(artist_df$week)

model0 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
             -Artist, label_df)
model01 <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Production, artist_df)
summary(model0)
summary(model01)

# 20~37주차에 순위가 더 하락한다. : 5, 6, 7, 8월
# 51 ~ 11주차에 덜 하락한다. : 12, 1, 2, 3월
## 별이 뜨지 않았다면, 1주차에 비해 유의미하게 다르지 않다고 해석.
# 겨울, 봄에 덜 하락하고 여름에 더 많이 하락한다.
# 여름에 순위 유지가 더 어렵다.

#### 조절변수
# Rel..humidity는 온도와의 조절변수로서도 역할하지 못함
model0m <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Artist
              + Rel..humidity * Relative.Temperature
              ,label_df)
model01m <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
               -Production
               + Rel..humidity * Relative.Temperature
               ,artist_df)

summary(model0m)
summary(model01m)

# Pressure와 상대온도 사이의 효과
# Artist로 고정했을 때는 Pressure의 조절효과가 있다.
# Artist 단위에서는, 기압이 높아지면
# 상대온도 상승에 따른 순위 하락이 덜하다.
# 회사에게는 없지만, artist에게는 추운 고기압 날씨에 낼 유인이 있다.
model0m <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Artist
              + Relative.Temperature*Pressure
              ,label_df)
model01m <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
               -Production
               + Relative.Temperature*Pressure
               ,artist_df)

summary(model0m)
summary(model01m)

# Relative.Temperature의 조절효과
# Aritst chart 경우에만 작동
# Label chart의 경우에서는 눈에 띄게 높은 확률로 기각한다.
# Rain 날씨에 상대온도가 높으면 더 많은 폭으로 순위가 하락한다.
model0m <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
              -Artist
              + Relative.Temperature*Rain
              ,label_df)
model01m <- lm(Rank_change~.-FogRain -FogSnowfall -FogSnowfallRain-FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
               -Production
               + Relative.Temperature*Rain
               ,artist_df)

summary(model0m)
summary(model01m)

# label 단위에서 확인할 수 있는 일반적인 위험은
# artist 단위에서보다 더 집합적인 것으로 보인다.

#### 몇몇의 Artist, Label에 대한 분석####
artist_list
# 오마이걸
chart_OMG <- gaon_chart[gaon_chart$Artist == '오마이걸 (OH MY GIRL)',]

OMG_df <- merge(chart_OMG, weather, how='inner', by=c('Year', 'week'))
OMG_df <- OMG_df[-3]

model0OMG <- lm(Rank_change~ .-FogRain -FogSnowfall -FogSnowfallRain -FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
                -Production, OMG_df)

summary(model0OMG)


# 아이유
chart_IU <- gaon_chart[gaon_chart$Artist == '아이유 (IU)',]

IU_df <- merge(chart_IU, weather, how='inner', by=c('Year', 'week'))
IU_df <- IU_df[-3]

model0IU <- lm(Rank_change~ .-FogRain -FogSnowfall -FogSnowfallRain -FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
               -Production, IU_df)

summary(model0IU)

# 임창정
chart_Lim<- gaon_chart[gaon_chart$Artist == '임창정',]

Lim_df <- merge(chart_Lim, weather, how='inner', by=c('Year', 'week'))
Lim_df <- Lim_df[-3]

model0Lim <- lm(Rank_change~ .-FogRain -FogSnowfall -FogSnowfallRain -FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
                -Production, Lim_df)

summary(model0Lim)

## Label
label_list
# 카카오
chart_Kakao <- gaon_chart[gaon_chart$Production == 'Kakao',]

Kakao_df <- merge(chart_Kakao, weather, how='inner', by=c('Year', 'week'))
Kakao_df <- Kakao_df[-4]

model0Kakao <- lm(Rank_change~ .-FogRain -FogSnowfall -FogSnowfallRain -FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
                  -Artist, Kakao_df)

summary(model0Kakao)

# 스타쉽
chart_star <- gaon_chart[gaon_chart$Production == '스타쉽엔터테인먼트',]

star_df <- merge(chart_star, weather, how='inner', by=c('Year', 'week'))
star_df <- star_df[-4]

model0star <- lm(Rank_change~ .-FogRain -FogSnowfall -FogSnowfallRain -FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
                 -Artist, star_df)

summary(model0star)

# 큐브
chart_cube <- gaon_chart[gaon_chart$Production == '큐브엔터테인먼트',]

cube_df <- merge(chart_cube, weather, how='inner', by=c('Year', 'week'))
cube_df <- cube_df[-4]

model0cube <- lm(Rank_change~ .-FogRain -FogSnowfall -FogSnowfallRain -FogThunderstormRain -HailFogThunderstormRain -Snowfall-SnowfallRain -Thunderstorm -ThunderstormRain -ThunderstormSnowfallRain
                 +Temperature*Relative.Temperature -Artist, cube_df)

summary(model0cube)
