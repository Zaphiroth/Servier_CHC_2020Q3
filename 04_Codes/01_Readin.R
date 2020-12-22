# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q3
# Purpose:      Readin Raw Data
# programmer:   Zhe Liu
# Date:         2020-11-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Mapping table ----
## PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20201118.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

## market definition
market.def <- read_xlsx("02_Inputs/Market_Definition_20200824.xlsx") %>% 
  distinct(molecule = Molecule_Desc, market = TA) %>% 
  filter(!is.na(market))

## target city
kTargetProv <- c("北京", "福建", "广东", "江苏", "上海", "浙江", "安徽", "山东")
kTargetCity <- c("北京", "常州", "福州", "广州", "杭州", "南京", 
                 "宁波", "泉州", "厦门", "上海", "苏州", "温州", 
                 "无锡", "济南", "徐州", "合肥", "绍兴", "青岛")


##---- Formatting raw data ----
raw.ahbjjs <- read_csv('02_Inputs/data/Servier_ahbjjssdzj_17181920Q1Q2Q3_fj1718_nozj20Q3_packid_moleinfo.csv', 
                       locale = locale(encoding = 'GB18030'))
raw.fj1 <- read.xlsx('02_Inputs/data/Servier_福建省_2019_packid_moleinfo(predicted by Servier_fj_2018_packid_moleinfo_v3).xlsx')
raw.fj2 <- read.xlsx('02_Inputs/data/Servier_福建省_2020_packid_moleinfo(predicted by Servier_fj_2018_packid_moleinfo_v3).xlsx')
raw.zj <- read.xlsx('02_Inputs/data/Servier_浙江省_2020Q3_packid_moleinfo(predicted by Servier_zj_2020Q1Q2_packid_moleinfo_v3).xlsx')
raw.gz <- read_feather('02_Inputs/data/Servier_guangzhou_17181920Q1Q2Q3_packid_moleinfo.feather')

raw.total <- raw.ahbjjs %>% 
  filter(Project == 'Servier') %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         packcode = as.character(packcode)) %>% 
  bind_rows(raw.fj1, raw.fj2, raw.zj) %>% 
  select(-IntPrd_Desc, -IntStr_Desc, -IntSize_Desc, -IntVol_Desc, -IntPck_Desc, 
         -PRES_Desc, -PROTECTION_Desc, -COMP_Desc) %>% 
  bind_rows(raw.gz) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc4 = ATC4_Code, 
           nfc = NFC123_Code, 
           molecule = Molecule_Desc, 
           product = Prd_desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = Volume, 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  left_join(market.def, by = 'molecule') %>% 
  mutate(atc3 = stri_sub(atc4, 1, 4)) %>% 
  filter(!is.na(market)) %>% 
  filter(quarter %in% c('2019Q3', '2020Q3'), 
         units > 0, sales > 0) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, atc3, 
           molecule, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units)

write_feather(raw.total, '03_Outputs/01_Servier_CHC_Raw.feather')

## QC
chk <- raw.total %>% 
  group_by(city, market, quarter, packid) %>% 
  summarise(sales = sum(sales), 
            units = sum(units)) %>% 
  ungroup() %>% 
  arrange(city, market, quarter, packid)

