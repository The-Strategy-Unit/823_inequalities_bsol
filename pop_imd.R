# library(readxl)


# 0. LOAD -----------------------------------------------------------------

# saveRDS(pop_all, "pop_all.rds")
# ... AND SKIP TO SECTION 3A OR...


# from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

# NOTE: DESIRE THE UNFORMATTED VERSION:
# 
# raw_pop_females <- read_excel("SAPE21DT2-mid-2018-lsoa-syoa-estimates-unformatted.xlsx",
# sheet = "Mid-2018 Females", skip = 4)
# 
# raw_pop_males <- read_excel("SAPE21DT2-mid-2018-lsoa-syoa-estimates-unformatted.xlsx",
#                             sheet = "Mid-2018 Males", skip = 4)

# 1. CLEAN ----------------------------------------------------------------
# 
# pop_female <-
#   raw_pop_females %>%
#   janitor::clean_names() %>%
#   filter(str_detect(area_codes, "^E")) %>%  # remove a few rows
#   select(-c(
#     # la_code_2019_boundaries, la_code_2020_boundaries,
#     # la_name_2019_boundaries, la_name_2020_boundaries,
#     all_ages
#   )) %>%
#   mutate(sex = "2", .before = area_codes)

# pop_male <-
#   raw_pop_males %>% 
#   janitor::clean_names() %>%
#   filter(str_detect(area_codes, "^E")) %>%  # remove a few rows
#   select(-c(
#     # la_code_2019_boundaries, la_code_2020_boundaries,
#     # la_name_2019_boundaries, la_name_2020_boundaries,
#     all_ages
#   )) %>% 
#   mutate(sex = "1", .before = area_codes)
# 


# 2. BIND -----------------------------------------------------------------

# pop_all <- bind_rows(pop_female, pop_male)
# 
# pop_all <- pop_all %>% 
#   pivot_longer(cols = starts_with("x"), names_to = "age", values_to = "pop") %>% 
#   mutate(age = as.integer(str_replace(age, "x", "")))
# 
# 
# pop_all <- pop_all %>% 
#   mutate(age_grp = case_when(
#     age %in% 0:4 ~ "00-04",
#     age %in% 5:9 ~ "05-09",
#     age %in% 10:14 ~ "10-14",
#     age %in% 15:19 ~ "15-19",
#     age %in% 20:24 ~ "20-24",
#     age %in% 25:29 ~ "25-29",
#     age %in% 30:34 ~ "30-34",
#     age %in% 35:39 ~ "35-39",
#     age %in% 40:44 ~ "40-44",
#     age %in% 45:49 ~ "45-49",
#     age %in% 50:54 ~ "50-54",
#     age %in% 55:59 ~ "55-59",
#     age %in% 60:64 ~ "60-64",
#     age %in% 65:69 ~ "65-69",
#     age %in% 70:74 ~ "70-74",
#     age %in% 75:79 ~ "75-79",
#     age %in% 80:84 ~ "80-84",
#     age %in% 85:89 ~ "85-89",
#     age == 90 ~ "90+",
#     TRUE ~ NA_character_
#   )) %>%
#   mutate(age = case_when(
#     # age %in% c("90+", "90 and over") ~ "90",
#     age == "90" ~ "90+",
#     age %in% 0:9 ~ str_c("0", age),
#     TRUE ~ as.character(age)
#   )) %>%
#   relocate(age_grp, .after = age)
# 
# pop_all <- pop_all %>% 
#   count(sex, area_codes, area_names, age_grp, wt = pop, name = "pop")
# 
# saveRDS(pop_all, "pop_all.rds")
pop_all <- read_rds("pop_all.rds")

# ** 3a. Local pop ---------------------------------------------------------

# BLACK COUNTRY LSOAS:
# lsoa_bc_and_bsol <- lkp_lsoa %>% 
#   filter(ccg20cdh %in% c(
#     "05C", "05L", "05Y", "06A", # BLACK COUNTRY STP
#     "15E"  # BSOL ICS
#     ))
# lkp_lsoa_set

pop_imd_final <- pop_all %>%
  right_join(lkp_lsoa_set, by = c("area_codes" = "lsoa")) %>% 
  # right_join(lsoa_bc_and_bsol, by = c("area_codes" = "lsoa11cd")) %>% 
  left_join(lkp_imd, by = c("area_codes" = "lsoa_code")) %>% 
  # group_by(sex, lsoa_code, lsoa_name, age_grp) %>%
  # summarise(pop = sum(pop)) %>% 
  # ungroup() %>% 
  # NOTE: EXTRA GROUPING NEEDED WHEN DOING LOWER LEVEL
  group_by(ics, place, imd_quint, sex, age_grp) %>% 
  summarise(pop_local = sum(pop)) %>% 
  ungroup %>% 
  # summarise(sum(pop_local)) %>%
  identity()

