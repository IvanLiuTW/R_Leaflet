# Goal ----
# We aim to create a map with the locations of public and private parking lots marked on it, and it's mainly for the tourists
# who are not familiar with Taichung City. On the other hand, we also put famous spots in Taichung on the map.

## ETL 資料預處理
# 去除非相關的停車場資訊(醫院附設、大學附設、政府機關單位或難以搜尋(因無確切名稱的停車場))，並將停車場名稱再細分為
# 行政區(District) - 公、私有(State) - 名稱(Name)


# Preparation ----

# install.packages("leaflet") -> use leaflet map package
library(leaflet)
library(dplyr)
setwd("~/Desktop/Projects/Project 2 - R - Open Data & Google Map") # set working directory

# Step 1: set Taichung as the center of the map ----

taichung_map <- leaflet() %>% # Create a map widget by calling leaflet()
    addTiles() %>%
    setView(lng = 120.70177757327166, lat = 24.14217245944508, zoom = 12) # zoom: 調整大小

taichung_map


# Step 2: read data of parking lots and spots info ----

# import parking lots information
parking_info <- read.table('TaichungParkingLotsInfo(ETL).csv', header = T, sep = ',')
names(parking_info)
# [1] "ID"              "District"        "State"           "Name"            "Longitude"       "Latitude"       
# [7] "AvailableCarRGB"
dim(parking_info)
# [1] 596   7

# Set different groups based on Districts 

# 將臺中市區分成29個行政區：中區、東區、西區、南區、北區、西屯區、南屯區、北屯區、豐原區、大里區、太平區、清水區、沙鹿區、
# 大甲區、東勢區、梧棲區、烏日區、神岡區、大肚區、大雅區、后里區、霧峰區、潭子區、龍井區、外埔區、和平區、石岡區、大安區、
# 新社區

d1 <- filter(parking_info, District %in% "中區")
d2 <- filter(parking_info, District %in% "東區")
d3 <- filter(parking_info, District %in% "西區")
d4 <- filter(parking_info, District %in% "南區")
d5 <- filter(parking_info, District %in% "北區")
d6 <- filter(parking_info, District %in% "西屯區")
d7 <- filter(parking_info, District %in% "南屯區")
d8 <- filter(parking_info, District %in% "北屯區")
d9 <- filter(parking_info, District %in% "豐原區")
d10 <- filter(parking_info, District %in% "大里區")
d11 <- filter(parking_info, District %in% "太平區")
d12 <- filter(parking_info, District %in% "清水區")
d13 <- filter(parking_info, District %in% "沙鹿區")
d14 <- filter(parking_info, District %in% "大甲區")
d15 <- filter(parking_info, District %in% "東勢區")
d16 <- filter(parking_info, District %in% "梧棲區")
d17 <- filter(parking_info, District %in% "烏日區")
d18 <- filter(parking_info, District %in% "神岡區")
# d19 <- filter(parking_info, District %in% "大肚區") 0
d20 <- filter(parking_info, District %in% "大雅區")
d21 <- filter(parking_info, District %in% "后里區")
d22 <- filter(parking_info, District %in% "霧峰區")
d23 <- filter(parking_info, District %in% "潭子區")
d24 <- filter(parking_info, District %in% "龍井區")
# d25 <- filter(parking_info, District %in% "外埔區") 0
# d26 <- filter(parking_info, District %in% "和平區") 0
# d27 <- filter(parking_info, District %in% "石岡區") 0
# d28 <- filter(parking_info, District %in% "大安區") 0
# d29 <- filter(parking_info, District %in% "新社區") 0

# import spots information
spots <- read.csv('TaichungSpots.csv', header = T, sep = ',')
names(spots)
# [1] "景點ID"   "狀態"     "名稱"     "簡述"     "介紹"     "鄉鎮市區" "地址"     "東經"     "北緯"     "電話"     "大眾運輸"
# [12] "門票資訊" "行車資訊" "停車資訊" "旅遊叮嚀"

dim(spots)
# [1] 363  15

# Step 3: make the base map
# add markers and show the names of the spots
base_map <- leaflet() %>%
    addTiles() %>%
    addMarkers(data = spots, ~ spot_lon, ~ spot_lan, popup = spot_names, group = 'base')

base_map

# Step 4: use District to filter the parking lots on the map ----

# 將臺中市區分成29個行政區：中區、東區、西區、南區、北區、西屯區、南屯區、北屯區、豐原區、大里區、太平區、清水區、沙鹿區、
# 大甲區、東勢區、梧棲區、烏日區、神岡區、大肚區、大雅區、后里區、霧峰區、潭子區、龍井區、外埔區、和平區、石岡區、大安區、
# 新社區

map <- leaflet() %>% # Create a map widget by calling leaflet()
    addTiles() %>% 
    addMarkers(data = spots, ~ spots$東經, ~ spots$北緯, popup = spots$名稱,
               icon = list(iconUrl = "pin.png", iconSize = c(40, 40)), clusterOptions = markerClusterOptions(), group = 'base') %>%
    # Set clusterOptions: to cluster a large number of markers on a map
    # Set icon: to change the default icon  
    addMarkers(data = d1, ~ d1$Longitude, ~ d1$Latitude, popup = d1$Name, group = '中區') %>%
    addMarkers(data = d2, ~ d2$Longitude, ~ d2$Latitude, popup = d2$Name, group = '東區') %>%
    addMarkers(data = d3, ~ d3$Longitude, ~ d3$Latitude, popup = d3$Name, group = '西區') %>%
    addMarkers(data = d4, ~ d4$Longitude, ~ d4$Latitude, popup = d4$Name, group = '南區') %>%
    addMarkers(data = d5, ~ d5$Longitude, ~ d5$Latitude, popup = d5$Name, group = '北區') %>%
    addMarkers(data = d6, ~ d6$Longitude, ~ d6$Latitude, popup = d6$Name, group = '西屯區') %>%
    addMarkers(data = d7, ~ d7$Longitude, ~ d7$Latitude, popup = d7$Name, group = '南屯區') %>%
    addMarkers(data = d8, ~ d8$Longitude, ~ d8$Latitude, popup = d8$Name, group = '北屯區') %>%
    addMarkers(data = d9, ~ d9$Longitude, ~ d9$Latitude, popup = d9$Name, group = '豐原區') %>%
    addMarkers(data = d10, ~ d10$Longitude, ~ d10$Latitude, popup = d10$Name, group = '大里區') %>%
    addMarkers(data = d11, ~ d11$Longitude, ~ d11$Latitude, popup = d11$Name, group = '太平區') %>%
    addMarkers(data = d12, ~ d12$Longitude, ~ d12$Latitude, popup = d12$Name, group = '清水區') %>%
    addMarkers(data = d13, ~ d13$Longitude, ~ d13$Latitude, popup = d13$Name, group = '沙鹿區') %>%
    addMarkers(data = d14, ~ d14$Longitude, ~ d14$Latitude, popup = d14$Name, group = '大甲區') %>%
    addMarkers(data = d15, ~ d15$Longitude, ~ d15$Latitude, popup = d15$Name, group = '東勢區') %>%
    addMarkers(data = d16, ~ d16$Longitude, ~ d16$Latitude, popup = d16$Name, group = '梧棲區') %>%
    addMarkers(data = d17, ~ d17$Longitude, ~ d17$Latitude, popup = d17$Name, group = '烏日區') %>%
    addMarkers(data = d18, ~ d18$Longitude, ~ d18$Latitude, popup = d18$Name, group = '神岡區') %>%
    addMarkers(data = d20, ~ d20$Longitude, ~ d20$Latitude, popup = d20$Name, group = '大雅區') %>%
    addMarkers(data = d21, ~ d21$Longitude, ~ d21$Latitude, popup = d21$Name, group = '后里區') %>%
    addMarkers(data = d22, ~ d22$Longitude, ~ d22$Latitude, popup = d22$Name, group = '霧峰區') %>%
    addMarkers(data = d23, ~ d23$Longitude, ~ d23$Latitude, popup = d23$Name, group = '潭子區') %>%
    addMarkers(data = d24, ~ d24$Longitude, ~ d24$Latitude, popup = d24$Name, group = '龍井區') %>%
    # Layers Control
    addLayersControl(
        baseGroups = "base",
        overlayGroups = c("中區", "東區", "西區", "南區", "北區", "西屯區", "南屯區", "北屯區", "豐原區", "大里區", "太平區", "清水區", "沙鹿區",
                          "大甲區", "東勢區", "梧棲區", "烏日區", "神岡區", "大雅區", "后里區", "霧峰區", "潭子區", "龍井區"),
        options = layersControlOptions(collapsed = F)
    )

map