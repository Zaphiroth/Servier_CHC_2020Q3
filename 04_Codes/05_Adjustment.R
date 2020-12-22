# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2020Q3
# Purpose:      Adjustment
# programmer:   Zhe Liu
# date:         2020-11-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- ATC2 adjustment ----
atc.adj.raw <- proj.price %>% 
  mutate(atc2 = stri_sub(atc3, 1, 3),
         market = if_else(atc2 %in% c("C07", "C08"), "IHD", market)) %>% 
  filter(!(market == "IHD" & molecule == "IVABRADINE")) %>% 
  filter(!(market == "OAD" & molecule == "EPALRESTAT"))

ihd.adj <- atc.adj.raw %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.25, 0.1),
         sales = sales * factor,
         units = units * factor,
         market = "IHD") %>% 
  select(-factor)

htn.adj <- atc.adj.raw %>% 
  filter(atc2 %in% c("C07", "C08")) %>% 
  mutate(factor = if_else(atc2 == "C07", 0.75, 0.9),
         sales = sales * factor,
         units = units * factor,
         market = "HTN") %>% 
  select(-factor)

proj.atc.adj <- atc.adj.raw %>% 
  filter(!(atc2 %in% c("C07", "C08"))) %>% 
  bind_rows(ihd.adj, htn.adj)


##---- Scale adjustment ----
scale.factor <- read_xlsx('02_Inputs/Servier_CHC_Factor.xlsx')

scale.adj <- proj.atc.adj %>% 
  filter(panel != 1) %>% 
  left_join(scale.factor, by = c("city", "market")) %>% 
  mutate(sales = sales * factor, 
         units = units * factor) %>% 
  select(-factor)

proj.adj <- proj.atc.adj %>% 
  filter(panel == 1) %>% 
  bind_rows(scale.adj)

write.xlsx(proj.adj, "03_Outputs/05_Servier_CHC_Adjustment.xlsx")

## QC
chk <- proj.adj %>% 
  group_by(city, market, quarter) %>% 
  summarise(sales = sum(sales)) %>% 
  ungroup() %>% 
  arrange(city, market, quarter)

proj.atc.adj %>% 
  group_by(city, panel) %>% 
  summarise(sales = sum(sales)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = panel, values_from = sales, values_fill = 0)
