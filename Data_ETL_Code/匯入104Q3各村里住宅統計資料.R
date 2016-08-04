library(dplyr)
### 讀入104年Q3各村里住宅統計資料 ###
readBin("origin_data/resident.csv", "raw", n = 3L) # 編碼為big5
readLines(file("origin_data/resident.csv", encoding = "big5"), n = 3)
resident <- read.csv("origin_data/resident.csv", fileEncoding = "big5", header = T, sep = ",", stringsAsFactors = F)

year <- rep(c(10401,10403,10405,10407,10409,10411), each = 456) # 由於各村里住宅統計資料有里是NA，故觀察後只有307個里有記錄
resident <- data.frame(統計年月 = year, resident) # 用104Q3推估至104年雙月資料

resident %>% 
  mutate(行政區域 = paste(鄉鎮,地區,sep = ''),
         單身或頂客家庭 = as.numeric(X1人一宅宅數.宅.)+as.numeric(X2人一宅宅數.宅.),
         核心家庭 = as.numeric(X3人一宅宅數.宅.)+as.numeric(X4人一宅宅數.宅.),
         折衷或大家庭 = as.numeric(X5人一宅宅數.宅.)+as.numeric(X6人以上一宅宅數.宅.),
         少年戶長數 = as.numeric(X25.含.歲以下戶長數.人.),
         青年戶長數 = as.numeric(X25.35.含.歲戶長數.人.)+as.numeric(X35.45.含.歲戶長數.人.),
         壯年戶長數 = as.numeric(X45.55.含.歲戶長數.人.)+as.numeric(X55.65.含.歲戶長數.人.),
         老年戶長數 = as.numeric(X65歲以上戶長數.人.),
         二名以上老年人口宅數 = as.numeric(二名老年人口宅數.宅.)+as.numeric(三名以上老年人口宅數.宅.)) %>% 
  select(統計年月,縣市,行政區域,設有戶籍宅數.宅..x,設有戶籍宅數之平均人口數.人.,
         單身或頂客家庭,核心家庭,折衷或大家庭,總戶長數.人..x,戶長平均年齡.歲.,
         少年戶長數,青年戶長數,壯年戶長數,老年戶長數,每戶平均老年人口數.人.,
         僅老年人口居住宅數.宅.,一名老年人口宅數.宅.,二名以上老年人口宅數,
         有偶.人.,離婚.人.,未婚.人.) -> resident_104_twomonth
colnames(resident_104_twomonth) <- c("統計年月","縣市","行政區域","設有戶籍宅數","設有戶籍宅數之平均人口數",
                                     "單身或頂客家庭","核心家庭","折衷或大家庭","總戶長數","戶長平均年齡",
                                     "少年戶長數","青年戶長數","壯年戶長數","老年戶長數","每戶平均老年人口數",
                                     "獨居老人宅數","一名老年人口宅數","二名以上老年人口宅數",
                                     "有偶人數","離婚人數","未婚人數")

write.csv(resident_104_twomonth, "~/Dropbox/DSP/Power/data/resident_104_twomonth.csv", fileEncoding = 'big5', row.names = F)

  