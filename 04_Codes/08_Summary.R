# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q3
# Purpose:      Summary
# programmer:   Zhe Liu
# date:         2020-11-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


#---- CHC ----
## ZB flag
flag.raw <- read.xlsx("02_Inputs/13城市的招标flag_zs_flag.xlsx")

flag <- flag.raw %>% 
  filter(!is.na(`是否是13城市`)) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, zs_flag)

proj.zs <- proj.adj %>% 
  left_join(flag, by = c("province", "city", "pchc")) %>% 
  mutate(zs_flag = if_else(zs_flag != 1, 0, zs_flag)) %>% 
  filter(!(city %in% c("北京", "上海") & zs_flag != 1)) %>% 
  filter(quarter == '2020Q3')

## corporation
corp.ref <- fread("02_Inputs/cn_corp_ref_202010_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct()

## pack
pack.ref <- fread("02_Inputs/cn_prod_ref_202010_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct() %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

corp.pack <- pack.ref %>% 
  select("Pack_Id", "Prd_desc", "Pck_Desc", "PckSize_Desc", "Corp_ID") %>% 
  distinct() %>% 
  left_join(corp.ref, by = "Corp_ID") %>% 
  mutate(Corp_Desc = if_else(Corp_Desc == "LUYE GROUP", "LVYE GROUP", Corp_Desc)) %>% 
  select(packid = Pack_Id, product = Prd_desc, pack_desc = Pck_Desc, 
         pack_size = PckSize_Desc, corp_desc = Corp_Desc)

## join
chc.part <- bind_rows(proj.zs, proj.sh) %>% 
  left_join(corp.pack, by = "packid") %>% 
  filter(stri_sub(pack_desc, 1, 3) %in% c("CAP", "TAB", "PIL")) %>% 
  mutate(dosage_units = pack_size * units,
         channel = "CHC") %>% 
  group_by(province, city, year, quarter, market, atc3, molecule, packid, 
           pack_desc, product, corp_desc, channel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sales = if_else(sales < 0, 0, sales),
         units = if_else(units < 0, 0, units),
         sales = if_else(units == 0, 0, sales),
         units = if_else(sales == 0, 0, units))

## A10S
a10s <- proj.adj %>% 
  filter(atc3 == "A10S") %>% 
  left_join(corp.pack, by = "packid") %>% 
  mutate(dosage_units = pack_size * units,
         channel = "CHC") %>% 
  group_by(province, city, year, quarter, market, atc3, molecule, packid, 
           pack_desc, product, corp_desc, channel) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            dosage_units = sum(dosage_units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

## chpa
chpa.info <- read.xlsx('02_Inputs/ims_chpa_to20Q3.xlsx', cols = 1:21, startRow = 4) %>%  
  distinct(corp_desc1 = Corp_Desc, product1 = Prd_desc, packid = Pack_ID)

## total CHC
total.chc <- bind_rows(chc.part, a10s, bj.chs) %>% 
  left_join(chpa.info, by = 'packid') %>% 
  mutate(product = if_else(is.na(product), product1, product), 
         product = trimws(stri_sub(product, 1, -4)), 
         corp_desc = if_else(is.na(corp_desc), corp_desc1, corp_desc)) %>% 
  select(Pack_ID = packid, Channel = channel, Province = province, City = city, 
         Date = quarter, ATC3 = atc3, MKT = market, Molecule_Desc = molecule, 
         Prod_Desc = product, Pck_Desc = pack_desc, Corp_Desc = corp_desc, 
         Sales = sales, Units = units, DosageUnits = dosage_units)


##---- Add flag ----
## readin 4+7 flag
capital.47 <- read_xlsx("02_Inputs/4+7+省会名单.xlsx") %>% 
  filter(`类别` %in% "4+7城市") %>% 
  mutate(City = gsub("市", "", `城市`)) %>% 
  select("City", "是否是4+7城市" = "类别")

## product bid
prod.bid <- read_xlsx("02_Inputs/Displayname Mapping.xlsx", sheet = 1) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0),
         Pack_ID_5 = stri_sub(Pack_ID, 1, 5),
         `Display Name3 CN` = if_else(`Display Name3 CN` %in% c("中标品规", "非中标品规"), 
                                      "仿制", 
                                      `Display Name3 CN`),
         `Display Name3 CN` = gsub("-2|-1", "", `Display Name3 CN`),
         `Display Name2 CN` = gsub("-2|-1", "", `Display Name2 CN`)) %>% 
  distinct(name1 = `Display Name1 CN`,
           name2 = `Display Name2 CN`,
           name3 = `Display Name3 CN`,
           Pack_ID_5)

## join
chc.bid <- total.chc %>% 
  left_join(capital.47, by = "City") %>% 
  mutate(Pack_ID_5 = stri_sub(Pack_ID, 1, 5)) %>% 
  left_join(prod.bid, by = "Pack_ID_5")

## CN name
std.info <- read.xlsx('02_Inputs/Product standardization master data-A-S-1211.xlsx') %>% 
  select(PACK_ID, CORP_NAME_CH, MNF_TYPE, ATC3_CN = ATC3, MOLE_NAME_CH)

corp.add <- read_xlsx("02_Inputs/Corp_Info_20200908.xlsx") %>% 
  group_by(Corp_Desc) %>% 
  arrange(MnfType_Desc) %>% 
  summarise(Mnf_Type1 = first(MnfType_Desc)) %>% 
  ungroup() %>% 
  mutate(Mnf_Type1 = if_else(is.na(Mnf_Type1), "Local", Mnf_Type1),
         Mnf_Type1 = if_else(Mnf_Type1 %in% c("Imported", "Joint Venture"), "MNC", Mnf_Type1), 
         Mnf_Type1 = if_else(Mnf_Type1 == 'Local', 'L', 'I'))

## molecule bid
molecule.bid <- distinct(chc.bid, Molecule_Desc, name1) %>% 
  filter(name1 == "4+7分子") %>% 
  mutate(name1 = if_else(Molecule_Desc %in% c("LISINOPRIL", "CAPTOPRIL"), "4+7分子", name1)) %>% 
  select(Molecule_Desc, name1)

## join
chc.flag <- chc.bid %>% 
  select(-name1) %>% 
  left_join(molecule.bid, by = "Molecule_Desc") %>% 
  left_join(std.info, by = c('Pack_ID' = 'PACK_ID')) %>% 
  left_join(corp.add, by = "Corp_Desc") %>% 
  mutate(name3 = trimws(name3),
         MNF_TYPE = if_else(is.na(MNF_TYPE), Mnf_Type1, MNF_TYPE),
         MNF_TYPE = trimws(MNF_TYPE),
         name3 = ifelse(is.na(name3) & MNF_TYPE %in% c('I', 'J'), "原研", 
                        ifelse(is.na(name3) & MNF_TYPE == "L", "仿制", 
                               name3)),
         name2 = ifelse(name2 == "非中标产品", NA, name2)) %>% 
  rename("是否进入带量采购" = "name1",
         "是否是中标品种" = "name2",
         "是否是原研" = "name3",
         "是否是MNC" = "MNF_TYPE") %>% 
  filter(!grepl("AMP", Pck_Desc)) %>% 
  group_by(Corp_Desc) %>% 
  mutate(CORP_NAME_CH = first(na.omit(CORP_NAME_CH))) %>% 
  ungroup() %>% 
  group_by(ATC3) %>% 
  mutate(ATC3_CN = first(na.omit(ATC3_CN))) %>% 
  ungroup() %>% 
  group_by(Molecule_Desc) %>% 
  mutate(MOLE_NAME_CH = first(na.omit(MOLE_NAME_CH))) %>% 
  ungroup() %>% 
  mutate(CORP_NAME_CH = if_else(Corp_Desc == 'BJ.BAIAO PHARM', 
                                '北京百奥药业有限责任公司', 
                                CORP_NAME_CH))


##---- Add new column ----
add.info <- read.xlsx('02_Inputs/Product standardization master data-A-S-1211.xlsx') %>% 
  mutate(Prod_ID = substr(stri_pad_left(PACK_ID, 7, "0"), 1, 5)) %>% 
  distinct(Prod_ID, ATC4_Code = ATC4_CODE, Prod_CN_Name = PROD_NAME_CH)

add.split <- read.xlsx('02_Inputs/Product standardization master data-A-S-1211.xlsx') %>% 
  distinct(PACK_ID, SPEC, PACK)

city.en <- read.xlsx("02_Inputs/CityEN.xlsx")

chc.add <- chc.flag %>% 
  left_join(add.split, by = c('Pack_ID' = 'PACK_ID')) %>% 
  mutate(first_num_position = stri_locate_first(Pck_Desc, regex = "\\d")[,1], 
         last_space_position = stri_locate_last(Pck_Desc, regex = "\\s")[,1], 
         Package = str_squish(substr(Pck_Desc, 1, first_num_position - 1)), 
         Package = if_else(is.na(Package), Pck_Desc, Package), 
         Dosage = case_when(Pack_ID == '7235702' ~ '85MG', 
                            Pack_ID == '7275402' ~ '500MG', 
                            TRUE ~ SPEC), 
         Quantity = case_when(Pack_ID == '7235702' ~ 7, 
                              Pack_ID == '7275402' ~ 20, 
                              TRUE ~ PACK), 
         Pack_ID = as.integer(Pack_ID), 
         Prod_ID = substr(stri_pad_left(Pack_ID, 7, "0"), 1, 5)) %>% 
  left_join(add.info, by = 'Prod_ID') %>% 
  mutate(ATC4 = case_when(Prod_ID == '72357' ~ 'C09D3', 
                          Prod_ID == '72754' ~ 'A10J1', 
                          TRUE ~ ATC4_Code), 
         Prod_CN_Name = if_else(is.na(Prod_CN_Name), Prod_Desc, Prod_CN_Name)) %>% 
  mutate(
    Pack_ID = stri_pad_left(Pack_ID, 7, 0),
    TherapeuticClsII = case_when(
      ATC3 == "A10H" ~ "SULPHONYLUREA",
      ATC3 == "A10J" ~      "BIGUANIDE",
      ATC3 == "A10K" ~     "GLITAZONE",
      ATC3 == "A10L" ~     "AGIs",
      ATC3 == "A10M" ~    "METAGLINIDE",
      ATC3 == "A10N" ~    "DPP-IV",
      ATC3 == "A10P" ~     "SGLT2",
      ATC3 == "A10S" ~     "GLP-1",
      ATC3 == "A10X" ~     "OTHERS",
      ATC3 == "C02A" ~     "ANTI-HTN",
      ATC3 == "C02B" ~     "ANTI-HTN",
      ATC3 == "C02C" ~     "ANTI-HTN",
      ATC3 == "C03A" ~     "DIURETICS",
      ATC3 == "C07A" ~     "BB",
      ATC3 == "C08A" ~     "CCB",
      ATC3 == "C08B" ~     "CCB",
      ATC3 == "C09A" ~     "ACEi PLAIN",
      ATC3 == "C09C" ~     "ARB PLAIN",
      ATC3 == "C07A" ~     "BB",
      ATC3 == "C08A" ~     "CCB",
      ATC3 == "C01E" ~     "NITRITES",
      Molecule_Desc == "TRIMETAZIDINE" ~   "TMZ",
      ATC3 == "C01D" & Molecule_Desc != "TRIMETAZIDINE" ~  "OTHERS",
      ATC4 %in% c("C09B3", "C09D3") ~ "A+C FDC",
      ATC4 %in% c("C09B1", "C09D1") ~ "A+D FDC",
      !is.na(ATC4) ~ "OTHERS",
      TRUE ~ NA_character_
    )
  ) %>% 
  select(Pack_ID, 
         Channel, 
         Province, 
         City,  
         Date, 
         ATC3, 
         MKT, Molecule_Desc,
         Prod_Desc, Pck_Desc, Corp_Desc, Sales,  Units, DosageUnits,
         ATC4, TherapeuticClsII, Prod_CN_Name,  Package, Dosage, Quantity, 
         `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`,
         `是否是MNC`, `ATC3中文分类` = ATC3_CN) %>% 
  left_join(city.en, by = "City") %>% 
  mutate(`Period-MAT` = NA_character_, 
         TherapeuticClsII = case_when(
           MKT == "HTN" & ATC3 == "C09A" ~ "RAASi Plain",
           MKT == "HTN" & ATC3 == "C09C" ~ "RAASi Plain",
           MKT == "HTN" & ATC3 == "C09B" ~ "RAASi FDC",
           MKT == "HTN" & ATC3 == "C09D" ~ "RAASi FDC",
           MKT == "HTN" & ATC3 == "C02A" ~ "ANTI-HTN",
           MKT == "HTN" & ATC3 == "C02B" ~ "ANTI-HTN",
           MKT == "HTN" & ATC3 == "C02C" ~ "ANTI-HTN",
           MKT == "HTN" & ATC3 == "C03A" ~ "DIURETICS",
           MKT == "HTN" & ATC3 == "C07A" ~ "BB",
           MKT == "HTN" & ATC3 == "C08A" ~ "CCB",
           MKT == "HTN" & ATC3 == "C08B" ~ "CCB",
           MKT == "OAD" & ATC3 == "A10H" ~ "SULPHONYLUREA",
           MKT == "OAD" & ATC3 == "A10J" ~ "BIGUANIDE",
           MKT == "OAD" & ATC3 == "A10K" ~ "GLITAZONE",
           MKT == "OAD" & ATC3 == "A10L" ~ "AG Is",
           MKT == "OAD" & ATC3 == "A10M" ~ "METAGLINIDE",
           MKT == "OAD" & ATC3 == "A10N" ~ "DPP-IV",
           MKT == "OAD" & ATC3 == "A10P" ~ "SGLT2",
           MKT == "OAD" & ATC3 == "A10S" ~ "GLP-1",
           MKT == "OAD" & ATC3 == "A10X" ~ "OTHERS",
           MKT == "IHD" & ATC3 == "C07A" ~ "BB",
           MKT == "IHD" & ATC3 == "C08A" ~ "CCB",
           MKT == "IHD" & ATC3 == "C01E" ~ "NITRITES",
           MKT == "IHD" & ATC3 == "C01D" & Molecule_Desc == "TRIMETAZIDINE" ~ "TMZ",
           MKT == "IHD" & ATC3 == "C01D" & Molecule_Desc != "TRIMETAZIDINE"~ "OTHERS",
           TRUE ~ NA_character_
         ),
         Sales = round(Sales, 2),
         Units = round(Units),
         DosageUnits = round(DosageUnits)) %>% 
  filter(Molecule_Desc != "ENALAPRIL+FOLIC ACID") %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
         `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
         Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
         `是否是中标品种`, `是否是MNC`, `ATC3中文分类`)

write.xlsx(chc.add, '03_Outputs/08_Servier_CHC_2020Q3.xlsx')


##---- Result ----
chc.history <- read.xlsx("06_Deliveries/CHC_MAX_16Q420Q2_0910.xlsx", 
                         check.names = FALSE)

chc.result <- chc.history %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '06470', 
                           stri_paste('64895', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Pack_ID = if_else(stri_sub(Pack_ID, 1, 5) == '47775', 
                           stri_paste('58906', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Corp_Desc = if_else(Prod_Desc == "GLUCOPHAGE", "MERCK GROUP", Corp_Desc),
         Corp_Desc = if_else(Prod_Desc == 'ONGLYZA', 'ASTRAZENECA GROUP', Corp_Desc)) %>% 
  bind_rows(chc.add) %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0, 
         !(MKT %in% c("HTN", "IHD") & 
             !(stri_sub(Package, 1, 3) %in% c("CAP", "TAB", "PIL")))) %>% 
  mutate(`Period-MAT` = case_when(
    Date %in% c('2020Q3', '2020Q2', '2020Q1', '2019Q4') ~ 'MAT20Q3', 
    Date %in% c('2019Q3', '2019Q2', '2019Q1', '2018Q4') ~ 'MAT19Q3', 
    Date %in% c('2018Q3', '2018Q2', '2018Q1', '2017Q4') ~ 'MAT18Q3', 
    TRUE ~ NA_character_
  )) %>% 
  mutate(first_num_position = stri_locate_first(Pck_Desc, regex = "\\d")[,1],
         last_space_position = stri_locate_last(Pck_Desc, regex = "\\s")[,1],
         Dosage = if_else(is.na(Dosage), 
                          str_squish(substr(Pck_Desc, 
                                            first_num_position, 
                                            last_space_position - 1)), 
                          Dosage)) %>% 
  group_by(Pack_ID) %>% 
  mutate(ATC3 = first(ATC3), 
         Molecule_Desc = first(Molecule_Desc), 
         Prod_Desc = first(Prod_Desc), 
         Pck_Desc = first(Pck_Desc), 
         Corp_Desc = first(Corp_Desc), 
         TherapeuticClsII = first(TherapeuticClsII), 
         Prod_CN_Name = first(Prod_CN_Name), 
         Package = first(Package), 
         Dosage = first(Dosage), 
         Quantity = first(Quantity)) %>% 
  ungroup() %>% 
  group_by(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
           Prod_Desc, Corp_Desc, `Period-MAT`, `CITY-EN`, 
           TherapeuticClsII, Prod_CN_Name,  Package, Quantity, 
           `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
           `是否是MNC`, `ATC3中文分类`) %>% 
  summarise(Pck_Desc = last(Pck_Desc), 
            Dosage = last(Dosage), 
            Sales = round(sum(Sales, na.rm = TRUE), 2),
            Units = round(sum(Units, na.rm = TRUE)),
            DosageUnits = round(sum(DosageUnits, na.rm = TRUE))) %>% 
  ungroup() %>% 
  mutate(Sales_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ Sales / 0.75,
                               substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ Sales / 0.25,
                               substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ Sales / 0.9,
                               substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ Sales / 0.1,
                               TRUE ~ Sales),
         Units_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ Units / 0.75,
                               substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ Units / 0.25,
                               substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ Units / 0.9,
                               substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ Units / 0.1,
                               TRUE ~ Units),
         DosageUnits_raw = case_when(substr(ATC3, 1, 3) == "C07" & MKT == "HTN" ~ DosageUnits / 0.75,
                                     substr(ATC3, 1, 3) == "C07" & MKT == "IHD" ~ DosageUnits / 0.25,
                                     substr(ATC3, 1, 3) == "C08" & MKT == "HTN" ~ DosageUnits / 0.9,
                                     substr(ATC3, 1, 3) == "C08" & MKT == "IHD" ~ DosageUnits / 0.1,
                                     TRUE ~ DosageUnits)) %>%
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, 
         `Period-MAT`, `CITY-EN`, TherapeuticClsII, Prod_CN_Name,  Package, 
         Dosage, Quantity, `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, 
         `是否是中标品种`, `是否是MNC`, `ATC3中文分类`, Sales_raw, Units_raw, 
         DosageUnits_raw) %>% 
  arrange(Channel, Date, Province, City, MKT, Pack_ID)

write.xlsx(chc.result, "03_Outputs/08_Servier_CHC_2016Q4_2020Q3.xlsx")

## QC
chk <- chc.result %>% 
  distinct(Pack_ID, ATC3, Molecule_Desc, Prod_Desc, Pck_Desc, Corp_Desc, 
           TherapeuticClsII, Prod_CN_Name, Package, Dosage, Quantity) %>% 
  add_count(Pack_ID) %>% 
  filter(n > 1)

chk <- chc.result %>% 
  add_count(Channel, Date, City, MKT, Pack_ID) %>% 
  filter(n > 1)

