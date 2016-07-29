# 此 script job 結束

## clustering 
data <- fread('iconv -f big5 -t utf8 ./20160426_data.csv')

## 未進行偏態處理前
data %>% 
  filter(統計年月 == c(10407)) %>% 
  select(-c(統計年月,縣市)) %>% 
  group_by(行政區域) %>% 
  summarise_each(funs(mean(.))) %>% 
  select(行政區域,每戶平均用電度數,
             女性比例, 男性比例,
             少年人口比例,青年人口比例,壯年人口比例,老年人口比例,總人數,有偶人數, ## 有偶比例
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
  mutate(扶老比 = (1-青年人口比例-壯年人口比例-少年人口比例)/(青年人口比例+壯年人口比例),
            有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
            平均教育程度 = 大學以上比例*6+大學比例*4+大學以下比例*0,
            女男比 = round(女性比例/男性比例,4),
            每戶平均用電度數 = 每戶平均用電度數) %>% 
  select(行政區域,綜合所得中位數,平均教育程度,有偶比例,女男比,扶老比,每戶平均用電度數) %>%
  mutate_each(funs(scale(.)),-行政區域) %>% 
  gather('index','value',-行政區域) %>% 
  group_by(index) %>% 
  mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
  spread(index,value) %>% 
  gather('index','value',-行政區域) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  theme_grey(base_family = 'STHeiti') +
  facet_wrap(~index)

## 進行偏態處理
data %>% 
  filter(統計年月 == c(10407)) %>% 
  select(-c(統計年月,縣市)) %>% 
  group_by(行政區域) %>% 
  summarise_each(funs(mean(.))) %>% 
  select(行政區域,每戶平均用電度數,
         女性比例, 男性比例,
         少年人口比例,青年人口比例,壯年人口比例,老年人口比例,總人數,有偶人數, ## 有偶比例
         綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
  mutate(扶老比_log = log10((1-青年人口比例-壯年人口比例-少年人口比例)/(青年人口比例+壯年人口比例)),
         有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
         平均教育程度 = 大學以上比例*6+大學比例*4+大學以下比例*0,
         女男比 = round(女性比例/男性比例,4),
         綜合所得中位數_log = log10(綜合所得中位數),
         每戶平均用電度數_log = log10(每戶平均用電度數)) %>% 
  select(行政區域,綜合所得中位數_log,平均教育程度,有偶比例,女男比,扶老比_log,每戶平均用電度數_log) %>%
  mutate_each(funs(scale(.)),-行政區域) %>% 
  gather('index','value',-行政區域) %>% 
  group_by(index) %>% 
  mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
  spread(index,value) -> data.h

data.h %>% 
  gather('index','value',-行政區域) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  theme_grey(base_family = 'STHeiti') +
  facet_wrap(~index)


## 準備分群
data.h %>% 
  mutate(cluster = kmeans(data.h %>% select(-1),centers=6)$cluster %>% as.factor())  -> data_cluster

c8 %>% 
  group_by(cluster) %>% 
  select(-1) %>% 
  summarise_each(funs(mean),-cluster) %>% 
  gather('index','value',-cluster) %>% 
  spread(cluster,value) %>% 
  as.data.frame() -> tmp

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
      data = tmp[,c(2)]),
    list(
      data = tmp[,c(3)]),
    list(
      data = tmp[,c(4)]),
    list(
      data = tmp[,c(5)]),
    list(
      data = tmp[,c(6)]),
    list(
      data = tmp[,c(7)]),
    list(
      data = tmp[,c(8)]),
    list(
      data = tmp[,c(9)])
  )