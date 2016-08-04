### 讀入101年各里綜合所得稅總額資料 ###
readBin("origin_data/101綜合所得稅所得總額.CSV", "raw", n = 3L) # 編碼為UTF-8
readLines(file("origin_data/101綜合所得稅所得總額.CSV", encoding = "UTF-8"), n = 3)
income101 <- read.csv("origin_data/101綜合所得稅所得總額.CSV", skip = 1, header = T, stringsAsFactors = F)

# 將村里為"其他"與"合計"的資料給刪除
# 發現101年之後大同區文昌里由於戶數太少併入老師里，故需重新調整，決定調整以下三項合併
# 文昌里納稅單位=6, 綜合所得總額=4940, 平均數=823
# 老師里納稅單位=1772, 綜合所得總額=1479872, 平均數=835
library(dplyr)
income101 %>% 
  filter(村里 != "其　他", 村里 != "合　計") %>% 
  filter(村里 != "文昌里") -> income101

income101[income101$村里=="老師里","納稅單位"] <- 1778 # 6+1772
income101[income101$村里=="老師里","綜合所得總額"] <- 1484812 # 1479872+4940
income101[income101$村里=="老師里","平均數"] <- 835 # 1484812/1778

#檢查村里與edu.csv資料有無統一，發現有四個里不統一，故將其統一，且糖廍里暫時改為糖部里
income101[income101$村里=="富臺里","村里"] <- "富台里"
income101[income101$村里=="舊庄里","村里"] <- "舊莊里"
income101[income101$村里=="羣賢里","村里"] <- "群賢里"
income101[income101$村里=="羣英里","村里"] <- "群英里"
income101[income101$村里=="糖廍里","村里"] <- "糖部里"


# 將資料推估至月資料，並且將101年的資料推估至102-104年資料
month_1 <- rep(c(1,3,5,7,9), each=456)
month_2 <- rep(11, each=456)
month <- c(paste('0', month_1, sep = ''), month_2)

income101_month <- NULL
for(i in 1:6){
  income101_month <- bind_rows(income101_month, income101)
}
income101_month %>% 
  mutate(月份 = month,
         行政區域 = paste(鄉鎮市區, 村里, sep = '')) -> income101_month

year <- rep(101:104, each=2736)
income <- NULL
for(i in 1:4){
  income <- bind_rows(income, income101_month)
}

income %>% 
  mutate(統計年月 = paste(year, month, sep = ''),
         綜合所得IQR = 第三分位數 - 第一分位數) %>% 
  select(統計年月, 行政區域, 納稅單位, 綜合所得總額,
             平均數, 中位數, 綜合所得IQR, 第一分位數, 第三分位數, 標準差, 變異係數)-> income
colnames(income) <-
  c(colnames(income)[1], '行政區域', '納稅單位', '綜合所得總額', 
    '綜合所得平均數', '綜合所得中位數', '綜合所得IQR', '綜合所得Q1', '綜合所得Q3', '綜合所得標準差', '綜合所得變異係數')

income %>% 
  filter(substr(統計年月,1,3)=='104') -> income_104

# 輸出資料
write.csv(income, "~/Dropbox/DSP/Power/data/income_twomonth.csv", fileEncoding = "big5", row.names = F)
write.csv(income_104, "~/Dropbox/DSP/Power/data/104_income_twomonth.csv", fileEncoding = "big5", row.names = F)
