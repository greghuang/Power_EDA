# 20160613 分群結果 雷達圖 堆疊, 幫助說故事

## 各里的資料
data_cluster <- fread('20160613_data_cluster.csv')
## 雷達圖資料
radar.data <- read.csv('20160613_Radar_plot.csv') %>% 
  mutate_each(funs(round(.,digits=2)),-index)
radar.data$mean <- rowMeans(radar.data[,-1])
## 顏色對應各群(會跟地圖顏色相符)
c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A") -> col.raw
## 雷達圖
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_xAxis(categories = c('女男比','平均教育程度','扶老比_log',
                          '有偶比例','每戶平均用電度數_log','綜合所得中位數_log'),
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0, max = 1) %>% 
  hc_series(
    list(
      data = radar.data[,c(2)],
      pointPlacement = 'on',color=col.raw[1]),
    list(
      data = radar.data[,c(3)],
      pointPlacement = 'on',color=col.raw[2]),
    list(
      data = radar.data[,c(4)],
      pointPlacement = 'on',color=col.raw[3]),
    list(
      data = radar.data[,c(5)],
      pointPlacement = 'on',color=col.raw[4]),
    list(
      data = radar.data[,c(6)],
      pointPlacement = 'on',color=col.raw[5]),
    list(
      data = radar.data[,c(7)],
      pointPlacement = 'on',color=col.raw[6]),
    list(
      data = radar.data[,c(8)],
      pointPlacement = 'on',color=col.raw[7]),
    list(
      data = radar.data[,c(9)],
      pointPlacement = 'on',color=col.raw[8]),
    list(
      name = "各群平均",
      data = radar.data[,c(10)],
      pointPlacement = 'on',color='#474747'))
  