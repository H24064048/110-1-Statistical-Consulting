# https://www.datanovia.com/en/lessons/subset-data-frame-rows-in-r/

dlink <- 'C:/Users/hspsp/Documents/房地產計畫/Data/2021Q3'
doc <- 'D_lvr_land_A.csv'
datlink = paste(dlink,doc, sep='/')
setwd('C:/Users/hspsp/Documents/房地產計畫')

data = read.csv("C:\\Users\\hspsp\\Documents\\房地產計畫\\Data\\TainanCSV\\A_Tainan_2021Q3.csv", sep = ",", 
                header = TRUE, stringsAsFactors = FALSE)
data = data[-c(1),]

# A 買賣
library(tidyverse)
data = data %>% filter(鄉鎮市區 %in% c("安平區","北區","安南區","新化區","善化區","新市區","永康區"),
                       交易筆棟數 %in% c("土地0建物1車位0", "土地1建物1車位0", "土地1建物1車位1", "土地1建物1車位2"),
                       建物現況格局.房 < 4, 建物現況格局.廳 < 3, 建物現況格局.衛 < 3, 交易標的 != "車位",
                       建物移轉總面積平方公尺 < 166, 
                       主要用途 %in% c("集合住宅", "住家用", "集合住宅、停車空間", "住商用", "國民住宅", "宿舍"))


# 建物移轉總面積平方公尺 < 50坪

dim(data)            
head(data)
mdn_house_price <- function(data, city, room, bath, lot, area10){
  df = data %>% filter(鄉鎮市區== city, 交易筆棟數== paste("土地1建物1車位",lot, sep=""),
                       建物現況格局.房>= room, 建物現況格局.衛>= bath,
                       建物移轉總面積平方公尺 < 3.305785*area10+15, 
                       建物移轉總面積平方公尺 > 3.305785*area10-15)
  print(dim(df)[1])
  print(median(df$總價元))
}

mdn_house_price(data, "善化區", 3, 2, 1, 30)

#library(skimr)
#skim(data)
summary(data)
unique(data$鄉鎮市區)
unique(data$交易標的)
unique(data$土地位置建物門牌)
unique(data$主要用途)
unique(data$交易筆棟數)
unique(data$備註)
unique(data$建物現況格局.房)
unique(data$建物現況格局.廳)
unique(data$建物現況格局.衛)
