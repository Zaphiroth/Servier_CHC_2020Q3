# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q3
# Purpose:      Projection of Shanghai CHC
# programmer:   Zhe Liu
# date:         2020-11-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Sample ----
# sample
chc.history <- read.xlsx("06_Deliveries/Servier_CHC_2016Q4_2020Q2_v3.xlsx", 
                         check.names = FALSE)

sh.bj.sample <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '47775', 
                           stri_paste('58906', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '06470', 
                           stri_paste('64895', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID)) %>% 
  filter(Channel == "CHC", 
         Province %in% c("上海", "北京"), 
         stri_sub(Date, 1, 4) %in% c("2017", "2018", "2019"), 
         !is.na(Pack_ID), 
         Sales > 0) %>% 
  group_by(quarter = Date, market = MKT, atc3 = ATC3, 
           molecule = Molecule_Desc, packid = Pack_ID) %>% 
  summarise(province = first(na.omit(Province)),
            city = first(na.omit(City)),
            sales = sum(Sales, na.rm = TRUE)) %>% 
  ungroup()

sh.19q3 <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '47775', 
                           stri_paste('58906', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '06470', 
                           stri_paste('64895', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID)) %>% 
  filter(Channel == "CHC", 
         Province == "上海", 
         Date == "2019Q3", 
         !is.na(Pack_ID), 
         Sales > 0) %>% 
  group_by(quarter = Date, market = MKT, atc3 = ATC3, 
           molecule = Molecule_Desc, packid = Pack_ID) %>% 
  summarise(province = first(na.omit(Province)),
            city = first(na.omit(City)),
            units = sum(Units, na.rm = TRUE),
            sales = sum(Sales, na.rm = TRUE)) %>% 
  ungroup()

# pack ID existing & missing
sh.exist.pack <- sh.19q3$packid[which(sh.19q3$packid %in% sh.bj.sample$packid)]
sh.miss.pack <- sh.19q3$packid[which(!(sh.19q3$packid %in% sh.bj.sample$packid))]

# growth of existing pack
## 去掉growth太大的pack
growth.exist <- raw.total %>% 
  filter(city == "北京", quarter %in% c("2019Q3", "2020Q3")) %>% 
  filter(packid %in% unique(sh.19q3$packid)) %>% 
  mutate(province = "上海",
         city = "上海") %>% 
  group_by(quarter, city, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(quarter, sales, fill = 0) %>% 
  mutate(growth_1920q3 = `2020Q3` / `2019Q3`) %>% 
  select(city, packid, growth_1920q3) %>% 
  mutate(growth_1920q3 = if_else(is.na(growth_1920q3) | 
                                   is.infinite(growth_1920q3), 
                                 1, 
                                 growth_1920q3)) %>% 
  filter(growth_1920q3 <= 1.5, growth_1920q3 > 0)


##---- K-nn model ----
# ims sales
ims.raw <- fread("02_Inputs/cn_IMS_Sales_Fdata_201912_1.txt", stringsAsFactors = FALSE)

ims.sales <- ims.raw %>% 
  mutate(date = gsub("M", "", Period_Code),
         packid = stri_pad_left(Pack_ID, 7, 0)) %>% 
  filter(Geography_id == "CHT", date >= "201701") %>% 
  group_by(date, packid) %>% 
  summarise(sales = sum(LC, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(date, sales, fill = 0) %>% 
  mutate(train_flag = if_else(packid %in% growth.exist$packid, 1, 0))

# k-nn model
train.sh <- ims.sales[ims.sales$train_flag == 1, ]
test.sh <- ims.sales[ims.sales$train_flag == 0, ]

train.sh.tmp <- select(train.sh, -packid)
test.sh.tmp <- select(test.sh, -packid)

sh.model <- kknn(train_flag ~ ., train = train.sh.tmp, test = test.sh.tmp, 
                 k = 3, scale = TRUE)

sh.indice <- as.data.frame(sh.model$C) %>% 
  lapply(function(x) {
    train.sh$packid[x]
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_pack")

sh.weight <- as.data.frame(sh.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c("pack_1", "pack_2", "pack_3")) %>% 
  mutate(weight_sum = pack_1 + pack_2 + pack_3,
         pack_1 = pack_1 / weight_sum,
         pack_2 = pack_2 / weight_sum,
         pack_3 = pack_3 / weight_sum) %>% 
  bind_cols(test.sh[, c("packid")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = "packid", variable.name = "knn_level", value.name = "knn_weight")

# weighted growth
weight.growth <- sh.indice %>% 
  left_join(sh.weight, by = c("packid", "knn_level")) %>% 
  left_join(growth.exist, by = c("knn_pack" = "packid")) %>% 
  group_by(city, packid) %>% 
  summarise(growth_1920q3 = sum(growth_1920q3 * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(city, packid, growth_1920q3)

# growth
surplus <- setdiff(sh.19q3$packid[!(sh.19q3$packid %in% growth.exist$packid)], 
                   ims.sales$packid)

surplus.growth <- data.frame(city = "上海",
                             packid = surplus) %>% 
  mutate(growth_1920q3 = 1)

sh.growth <- bind_rows(merge(growth.exist, 0),
                       merge(weight.growth, 1),
                       merge(surplus.growth, 2)) %>% 
  rename("flag" = "y")


##---- Price ----
sh.price.origin <- price.origin %>% 
  filter(city == '北京') %>% 
  mutate(province = '上海', 
         city = '上海')

sh.price.city <- price.city %>% 
  filter(city == '北京') %>% 
  mutate(province = '上海', 
         city = '上海')

sh.price.province <- price.province %>% 
  filter(province == '北京') %>% 
  mutate(province = '上海')

sh.price.year <- price.year %>% 
  filter(province == '北京') %>% 
  mutate(province = '上海')


##---- Result ----
proj.sh <- sh.19q3 %>% 
  left_join(sh.growth, by = c("city", "packid")) %>% 
  mutate(sales = sales * growth_1920q3,
         quarter = "2020Q3",
         year = "2020") %>% 
  filter(sales > 0) %>% 
  left_join(sh.price.origin, by = c("province", "city", "quarter", "packid")) %>% 
  left_join(sh.price.city, by = c("province", "city", "year", "packid")) %>% 
  left_join(sh.price.province, by = c("province", "quarter", "packid")) %>% 
  left_join(sh.price.year, by = c("province", "year", "packid")) %>% 
  left_join(price.pack, by = c("quarter", "packid")) %>% 
  left_join(price.pack.year, by = c('year', 'packid')) %>% 
  mutate(price = if_else(is.na(price), price_city, price), 
         price = if_else(is.na(price), price_prov, price), 
         price = if_else(is.na(price), price_year, price), 
         price = if_else(is.na(price), price_pack, price), 
         price = if_else(is.na(price), price_pack_year, price), 
         units = sales / price) %>% 
  select(year, quarter, province, city, market, atc3, molecule, packid, 
         units, sales, price)

write.xlsx(proj.sh, "03_Outputs/06_Seriver_CHC_Shanghai.xlsx")

