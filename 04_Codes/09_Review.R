# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q3
# Purpose:      Review
# programmer:   Zhe Liu
# date:         2020-11-20
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Check SOP -----
# CHPA
chpa.format <- read.xlsx('05_Internal_Review/ims_chpa_to20Q3_format.xlsx')

market.mol <- chc.result %>% 
  distinct(Molecule_Desc, MKT)

servier.chpa <- chpa.format %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC3_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  filter(stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  left_join(market.mol, by = 'Molecule_Desc') %>% 
  filter(!is.na(MKT)) %>% 
  mutate(Prd_desc = trimws(stri_sub(Prd_desc, 1, -4))) %>% 
  select(Pack_ID, Date = quarter, MKT, ATC3 = ATC3_Code, Molecule_Desc, 
         Prod_Desc = Prd_desc, Pck_Desc, Corp_Desc, Units = UNIT, 
         Sales = RENMINBI)

write.xlsx(servier.chpa, '05_Internal_Review/Servier_CHPA_2018Q1_2020Q3.xlsx')
