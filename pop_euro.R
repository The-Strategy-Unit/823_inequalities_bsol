
# ** i. Euro pop ----------------------------------------------------------
# 
# # FROM: https://www.opendata.nhs.scot/dataset/standard-populations/resource/29ce4cda-a831-40f4-af24-636196e05c1a
# euro_2013 <- read_csv("https://www.opendata.nhs.scot/datastore/dump/29ce4cda-a831-40f4-af24-636196e05c1a?bom=True", 
#                       col_types = cols(`_id` = col_skip()))
# 
# euro_2013 <- euro_2013 %>% 
#   rename(pop_euro = EuropeanStandardPopulation, age_grp = AgeGroup, sex = Sex) %>% 
#   mutate(age_grp = str_remove(age_grp, " years")) %>% 
#   mutate(age_grp = case_when(
#     age_grp == "0-4" ~ "00-04",
#     age_grp == "5-9" ~ "05-09",
#     age_grp == "90plus" ~ "90+",
#     TRUE ~ age_grp
#   ) ) %>% 
#   mutate(sex = ifelse(sex == "Male", "1", "2"))
# 
# saveRDS(euro_2013, "euro_2013.rds")

euro_2013 <- read_rds("euro_2013.rds") %>% 
  arrange(sex, age_grp)


euro_2013_85_max <- euro_2013 %>%
  mutate(age_grp_85_max = case_when(
    age_grp %in% c("85-89", "90+")~ "85+",
    T ~ age_grp
    )) %>% 
  group_by(age_grp_85_max, sex) %>% 
  summarise(pop_euro = sum(pop_euro)) %>% 
  ungroup %>% 
  arrange(sex, age_grp_85_max)

# CHECKS:
# euro_2013_85_max %>% count(age_grp_85_max, wt = pop_euro)
# euro_2013 %>% count(age_grp, wt = pop_euro)
# euro_2013 %>% tail(5)
# euro_2013_85_max %>% tail(5)
