library(dplyr)
library(tidyr)
library(sqldf)
library(ggplot2)
library(dbscan)
library(DT)
library(highcharter)
library(data.table)
library(radarchart)
library(plotly)

###將所有資料讀入，並且合併###
power <- read.csv("~/Dropbox/DSP/Power/data/104_power_twomonth.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)
edu <- read.csv("~/Dropbox/DSP/Power/data/104_edu_twomonth.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)
income <- read.csv("~/Dropbox/DSP/Power/data/104_income_twomonth.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)
household <- read.csv("~/Dropbox/DSP/Power/data/104_household_twomonth.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)
life <- read.csv("~/Dropbox/DSP/Power/data/104_life_twomonth.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)
resident <- read.csv("~/Dropbox/DSP/Power/data/104_resident_twomonth.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)
store <- read.csv("~/Dropbox/DSP/Power/data/台北市各里便利商店數.csv", fileEncoding = "big5", header = T, stringsAsFactors = F)

data <- sqldf(
  "select power.*, edu.教育程度總計, 大學以上比例, 大學比例, 大學以下比例,
          income.納稅單位, 綜合所得總額, 綜合所得中位數, 綜合所得IQR,
          household.戶數, 總人數, 女性比例, 男性比例, 少年人口比例, 青年人口比例, 壯年人口比例, 老年人口比例,
          life.出生數, `出生數.男`, `出生數.女`, 結婚對數, 離婚對數,
          resident.設有戶籍宅數, 設有戶籍宅數之平均人口數, 單身或頂客家庭, 核心家庭, 折衷或大家庭,
                   總戶長數, 戶長平均年齡, 少年戶長數, 青年戶長數, 壯年戶長數, 老年戶長數,
                   每戶平均老年人口數, 獨居老人宅數, 一名老年人口宅數, 二名以上老年人口宅數,
                   有偶人數, 離婚人數, 未婚人數,
          store.便利商店數
   from power left join edu on power.統計年月 = edu.統計年月
                            and power.縣市 = edu.縣市
                            and power.行政區域 = edu.行政區域
              left join income on power.統計年月 = income.統計年月
                               and power.縣市 = income.縣市
                               and power.行政區域 = income.行政區域
              left join household on power.統計年月 = household.統計年月
                                  and power.縣市 = household.縣市
                                  and power.行政區域 = household.行政區域
              left join life on power.統計年月 = life.統計年月
                             and power.縣市 = life.縣市
                             and power.行政區域 = life.行政區域
              left join resident on power.統計年月 = resident.統計年月
                                 and power.縣市 = resident.縣市
                                 and power.行政區域 = resident.行政區域
              left join store on power.縣市 = store.縣市
                              and power.行政區域 = store.行政區域"
)
data %>% 
  mutate(每戶平均用電度數 = round(售電量度數/戶數,2),
         每人平均用電度數 = round(售電量度數/總人數,2)) -> data

##EDA
data %>% 
  ggplot(aes(x = as.character(統計年月), y = 售電量度數, label = data$售電量度數)) +
  geom_bar(stat = 'identity', fill = 'darkblue')+
  geom_text(aes(y = 售電量度數), family='STHeiti',nudge_y = 1e+9, nudge_x = 0, size = 6, check_overlap = T, color='red')+
  theme_bw(base_family='STHeiti')+
  labs(title = '104年台北市售電量度數分佈',x='統計年月',y='售電量度數')+
  theme(plot.title = element_text(size=rel(1.3)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(hjust =1))+
  ggsave('~/Dropbox/DSP/Power/data/104年台北市售電量度數分佈.png', width = 10, height = 10)
  
data %>% 
  mutate(行政區 = substr(行政區域,1,3)) %>% 
  ggplot(aes(x = reorder(行政區, -每戶平均用電度數), y = 每戶平均用電度數)) +
  geom_bar(stat = 'identity', fill = 'darkblue')+
  theme_bw(base_family='STHeiti')+
  labs(title = '104年台北市各區每戶平均用電度數分佈',x='行政區')+
  theme(plot.title = element_text(size=rel(1.3)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 45, hjust =1))+
  ggsave('~/Dropbox/DSP/Power/data/104年台北市各區每戶平均用電度數分佈.png', width = 10, height = 10)



#write.csv(data, "~/Dropbox/DSP/Power/data/data.csv", fileEncoding = "big5", row.names = F)

data %>% 
  filter(統計年月 == c(10407)) %>% 
  mutate(有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
          男女比 = 男性比例 / 女性比例) %>% 
  select(行政區域, 每戶平均用電度數, 戶數,
             少年人口比例, 青年人口比例, 老年人口比例, 男女比, 有偶比例,
             設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例)-> data_7

data %>% 
  filter(統計年月 == c(10407)) %>% 
  select(-c(統計年月,縣市)) %>% 
  group_by(行政區域) %>% 
  summarise_each(funs(mean(.))) %>% 
  select(行政區域, 每戶平均用電度數, 戶數, 戶長平均年齡, 總人數,
             少年人口比例, 青年人口比例, 老年人口比例, 女性比例, 男性比例, 有偶人數,
             設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
  mutate(有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
            男女比 = round(男性比例/女性比例,4)) %>% 
  select(行政區域, 每戶平均用電度數, 戶數,
             少年人口比例, 青年人口比例, 老年人口比例, 男女比, 有偶比例,
             設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>%
  mutate_each(funs(scale(.)),-行政區域)  -> data.h

data.h %>% as.data.frame() %>% datatable() %>% formatRound(2:44,digit=2)


## 標準化後分群
data.h %>% 
  mutate(cluster = kmeans(data.h %>% select(-1),centers=10)$cluster %>% as.factor())  -> data_cluster

atcc <- data.frame(data_7, cluster = data_cluster$cluster)
write.csv(atcc, "~/Desktop/data_cluster.csv", fileEncoding = "big5", row.names = F)

## 分完群後計算各群的指標平均，在對指標scale到0、1
atcc %>% 
  group_by(cluster) %>% 
  summarise_each(funs(mean(.)),-行政區域) -> atcc_1
atcc_1 <- apply(atcc_1[,2:15], 2, FUN = function(X) (X - min(X))/diff(range(X)))
radar.data <- data.frame(cluster = as.factor(1:10), atcc_1)

radar.data %>% 
  gather('index','value',-cluster) %>%
  spread(cluster,value) -> radar.data

write.csv(radar.data, "~/Desktop/radar_data.csv", fileEncoding = "big5", row.names = F)


## 定義顏色
c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A") -> col.raw
cbind(grDevices::col2rgb(col.raw),3) -> col


## 畫雷達圖
radar.data %>% 
  select(1:11) %>% 
  chartJSRadar(scaleStartValue = 0, showToolTipLabel=TRUE, maxScale=1,colMatrix=col[,1:10],
               labelSize = 10)

radar.data %>% 
  select(1,3) %>% 
  chartJSRadar(scaleStartValue = 0, showToolTipLabel=TRUE, maxScale=1,colMatrix=col[,1:10],
               labelSize = 10)




### 利用Density-Based Clustering，列出各種esp, min的組合下分群結果
eps.set <- seq(0,5,0.1)
temp <- NULL
for(i in 1:length(eps.set)){
  c <- dbscan(data.h %>% select(-行政區域),eps = eps.set[i] ,MinPts = 5)
  result.table <- c$cluster %>% table
  cluster.num <- result.table %>% length
  outlier.num <- result.table[[1]]
  temp <- bind_rows(temp,
                    data.frame(eps.set = eps.set[i],
                               minpts = 5,
                               cluster.num = cluster.num ,outlier.num = outlier.num))
}
temp %>% datatable %>% formatRound(1,digits = 2)
### 半徑eps = 3.4時，約有0.05%的outlier被挑出來

### 挑出outlier
c <- dbscan(data.h %>% select(-行政區域),eps = 3.5 ,MinPts = 5)
a <- data.h %>% filter(c$cluster==0)
data_nooutlier <- data.h %>% filter(c$cluster != 0)

##Cluster by kmeans##
cl <- kmeans(data_nooutlier[,-1], centers = 10)
table(cl$cluster)
# 1  2  3  4  5  6  7  8  9 10 
#62 34 46 14 70 41 24 41 27 75

data_nooutlier <- data.frame(data_nooutlier, cluster = cl$cluster)
a <- data.frame(a, cluster = 0)
data_h_cluster <- rbind(data_nooutlier, a)

atcc <- sqldf(
  "select data_7.*, data_h_cluster.cluster 
   from data_7 left join data_h_cluster
   on data_7.行政區域 = data_h_cluster.行政區域"
)

write.csv(atcc, '~/Dropbox/ATCC-IBM/atcc.csv', fileEncoding = "big5", row.names = F)


### 畫分群雷達圖

library(zmisc)
atcc[,c(2:19)] %>% 
  filter(cluster != 0)-> atcc_total
atcc_total$cluster <- factor(atcc_total$cluster, levels = c(1:10))

z.radarmap(atcc_total, "cluster", stats="median", lwd=2, angle=0,
           fontsize=1.5, facet=T, facetfontsize=0.1, color=id, linetype=NULL)+
  labs(title = '分群雷達圖')+
  theme_grey(base_family="STHeiti")+
  theme(axis.text.x=element_text(angle = 10 ,size = 15),
        axis.text.y=element_text(size = 20),
        plot.title = element_text(size = rel(3))) +
  ggsave('~/Dropbox/DSP/Power/data/族群2.png', width = 20, height = 20)




##用pca將變數維度縮減
data_pca <- prcomp(data_7[,4:45], center = T, scale = T)
print(data_pca)

# plot method
plot(data_pca, type = "l")
summary(data_pca) # 看各個主成份的解釋變異，決定取前四個主成份
data_pca_rotation <- as.data.frame(data_pca$rotation[,1:4])

# 看第一主成份loading最重的前六個變數
data_pca_rotation %>% 
  mutate(var = row.names(data_pca_rotation)) %>% 
  arrange(desc(abs(PC1))) %>% 
  head(10)


# 看第二主成份loading最重的前六個變數
data_pca_rotation %>% 
  mutate(var = row.names(data_pca_rotation)) %>% 
  arrange(desc(abs(PC2))) %>% 
  head(10)

# 看第三主成份loading最重的前六個變數
data_pca_rotation %>% 
  mutate(var = row.names(data_pca_rotation)) %>% 
  arrange(desc(abs(PC3))) %>% 
  head(10)

# 看第四主成份loading最重的前六個變數
data_pca_rotation %>% 
  mutate(var = row.names(data_pca_rotation)) %>% 
  arrange(desc(abs(PC4))) %>% 
  head(10)

# 最後挑選出來的變數為：每戶平均用電度數
# 戶數、戶長平均年齡、少年人口比例、青年人口比例、老年人口比例、女性比例、男性比例、有偶人數
# 設有戶籍宅數之平均人口數、一名老年人口宅數、每戶平均老年人口數
# 綜合所得中位數、大學以上比例、大學比例、大學以下比例

data_7 %>% 
  select(統計年月, 行政區域, 每戶平均用電度數, 戶數, 戶長平均年齡,
         少年人口比例, 青年人口比例, 老年人口比例, 女性比例, 男性比例, 有偶人數,
         設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
         綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) -> atcc

##Cluster by kmeans##
cl <- kmeans(atcc[,-c(1,2)], centers = 10)
table(cl$cluster)
# 1  2  3  4  5  6  7  8  9 10 
#25 41 37 67 97 68 39 33 25 24    

atcc <- data.frame(atcc[,-1], cluster = cl$cluster)
#write.csv(atcc, "~/Dropbox/DSP/Power/data/atcc.csv", fileEncoding = "big5", row.names = F)

### 畫分群雷達圖

library(zmisc)
atcc[,c(2:18)] -> atcc_total
atcc_total$cluster <- factor(atcc_total$cluster, levels = c(1:10))

z.radarmap(atcc_total, "cluster", stats="median", lwd=2, angle=0,
           fontsize=1.5, facet=T, facetfontsize=0.1, color=id, linetype=NULL)+
  labs(title = '分群雷達圖')+
  theme_grey(base_family="STHeiti")+
  theme(axis.text.x=element_text(angle = 10 ,size = 15),
        axis.text.y=element_text(size = 20),
        plot.title = element_text(size = rel(3)))+
  ggsave('~/Dropbox/DSP/Power/data/族群.png', width = 20, height = 20)


# atcc %>% 
#  mutate(行政區 = substr(行政區域,1,3),
#         戶數級距 = cut(戶數, breaks = quantile(atcc[,2], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         總戶長數級距 = cut(總戶長數, breaks = quantile(atcc[,3], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         壯年人口級距 = cut(壯年人口, breaks = quantile(atcc[,4], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         女性人數級距 = cut(女性人數, breaks = quantile(atcc[,5], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         教育程度總計級距 = cut(教育程度總計, breaks = quantile(atcc[,6], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         每人平均用電度數級距 = cut(每人平均用電度數, breaks = quantile(atcc[,7], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         獨居老人宅數級距 = cut(獨居老人宅數, breaks = quantile(atcc[,8], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         每戶平均用電度數級距 = cut(每戶平均用電度數, breaks = quantile(atcc[,9], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         一名老年人口宅數級距 = cut(一名老年人口宅數, breaks = quantile(atcc[,10], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         二名以上老年人口宅數級距 = cut(二名以上老年人口宅數, breaks = quantile(atcc[,11], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         綜合所得中位數級距 = cut(綜合所得中位數, breaks = quantile(atcc[,12], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         綜合所得總額級距 = cut(綜合所得總額, breaks = quantile(atcc[,13], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         戶長平均年齡級距 = cut(戶長平均年齡, breaks = quantile(atcc[,14], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T),
#         大學以上級距 = cut(大學以上, breaks = quantile(atcc[,15], probs = seq(0,1,0.05)), labels = c(1:20), include.lowest = T)
#         ) %>%
#  filter(cluster==10) %>%
#  select(行政區, 行政區域, 戶數級距, 總戶長數級距, 壯年人口級距, 女性人數級距, 教育程度總計級距, 每人平均用電度數級距, 獨居老人宅數級距,
#         每戶平均用電度數級距, 一名老年人口宅數級距, 二名以上老年人口宅數級距, 綜合所得中位數級距, 綜合所得總額級距, 戶長平均年齡級距, 大學以上級距) %>% 
#  gather(變數, 等級, 3:16) %>% 
#  ggplot(aes(x = 變數, y = 等級, fill = 行政區)) +
#  geoＺm_bar(stat="identity", color="black") +
#  coord_polar() +
#  theme(plot.title = element_text(size=rel(1.3))) + 
#  labs(title = '族群十') +
#  theme_grey(base_family="STHeiti") +
#  ggsave('~/Dropbox/DSP/Power/data/族群10.png', width = 10, height = 10)

