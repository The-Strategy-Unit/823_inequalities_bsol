# library(nomisr)


# EXPERIMENT WITH NOMIS ---------------------------------------------------
# # SEE https://docs.evanodell.com/nomisr/
# # tmp100 <- nomis_data_info()
# # # tmp100 %>% view
# # # 
# # # DC2101EW
# # # tmp100 %>% filter(str_detect(name.value, "survey")) %>% select(name.value)
# # # tmp100 %>% filter(str_detect(name.value, "ensus")) %>% select(name.value)
# # tmp100 %>% filter(str_detect(name.value, "DC2101EW")) %>% select(name.value, everything()) %>% view
# # # tmp100 %>% filter(str_detect(name.value, "eth")) %>% select(name.value, everything()) %>% view
# # # tmp100 %>% filter(str_detect(description.value, "ethnicity"))  %>%
# # # tmp100 %>% filter(str_detect(description.value, "census"))  %>%
# # #   # select(description.value) %>% 
# # #   view()
# # #   select(id, name.value)
# #   # select(name.value)
# #   
# # tmp600 <- nomis_get_metadata(id = "NM_651_1")
# # # tmp101 <- nomis_get_metadata(id = "NM_58_1")
# # # tmp110 <- nomis_get_metadata(id = "NM_58_1", concept = "GEOGRAPHY")
# # 
# # nomis_get_metadata(id = "NM_651_1", concept = "GEOGRAPHY", type = "type")
# # THIS GIVES ICS LEVEL:
# # nomis_get_metadata(id = "NM_651_1", concept = "GEOGRAPHY", type = "TYPE459")
# # MSOA LEVEL:
# tmp <- nomis_get_metadata(id = "NM_651_1",
#                           time = "latest",
#                           concept = "GEOGRAPHY", type = "TYPE297")
# 
# vec_all_geographies <-
#   tmp %>% 
#   mutate(name = str_remove_all(label.en, "[:digit:]")) %>% 
#   mutate(name = str_trim(name)) %>% 
#   # count(name)
#   filter(name %in% c("Walsall", "Birmingham", "Solihull", "Dudley", "Wolverhampton", "Sandwell")) %>% 
#   # count(name)
#   pull(id)
#   
# 
# # tmp 
# # tmp102 %>% select(label.en)
# # 
# # tmp602 <- nomis_get_metadata(id = "NM_651_1", concept = "GEOGRAPHY", type = "TYPE297") # MSOA
# # tmp603 <- nomis_get_metadata(id = "NM_651_1", concept = "GEOGRAPHY", type = "TYPE464") # 2015 LAD
# # # tmp102 <- nomis_get_metadata(id = "NM_58_1", concept = "GEOGRAPHY", type = "TYPE464")
# # tmp602
# # tmp604 %>% print(n=400)
# # 
# # 
# # tmp201 <- nomis_overview("NM_58_1")
# # 
# # tmp201
# # 
# # # pop_eth <- nomis_get_data(id = "NM_651_1", time = "latest", geography = "TYPE464")


# 1. CENSUS 2011 DATA -----------------------------------------------------
# # 
# pop_eth_raw_100 <- nomis_get_data(id = "NM_651_1", time = "latest", geography = vec_all_geographies[1:100])
# pop_eth_raw_200 <- nomis_get_data(id = "NM_651_1", time = "latest", geography = vec_all_geographies[101:200])
# pop_eth_raw_314 <- nomis_get_data(id = "NM_651_1", time = "latest", geography = vec_all_geographies[201:314])
# 
# # length(vec_all_geographies)
# 
# pop_eth_raw_msoa <- pop_eth_raw_100 %>% 
#   bind_rows(pop_eth_raw_200) %>% 
#   bind_rows(pop_eth_raw_314) 
# 
# # saveRDS(pop_eth_raw_msoa, "pop_eth_raw_msoa.rds")



# 1. LKP - msoa -------------------------------------------------------------
# to parliamentary constituency
# 
# pop_eth_raw_msoa <- read_rds("pop_eth_raw_msoa.rds") %>% janitor::clean_names()
# 
# vec_msoas_all <- pop_eth_raw_msoa %>% 
#   select(starts_with("geo")) %>%
#   count(geography_code) %>% 
#   pull(geography_code)

# lkp_msoa_to_ward <- read_csv("https://opendata.arcgis.com/datasets/fcb3d6b3dc834e3ca3b38756b8b023f2_0.csv",
#   col_types = cols(
#     WD17NMW = col_skip(),
#     LAD17CD = col_skip(),
#     LAD17NM = col_skip(),
#     FID = col_skip()
#   )
# )
# 
# # MSOAS DO MAKE UP WARDS
# # lkp_msoa_to_ward %>% count(MSOA11NM, WD17NM, sort = T)
# 
# 
# lkp_ward_to_pcon <- read_csv("https://opendata.arcgis.com/datasets/077f8814334e453883e12479098c152c_0.csv",
#   col_types = cols(
#     LAD17CD = col_skip(),
#     LAD17NM = col_skip(), UTLA17CD = col_skip(),
#     UTLA17NM = col_skip(), FID = col_skip()
#   )
# )
# 
# # lkp_ward_to_pcon %>%
# #   filter(str_detect(WD17CD, "^E")) %>% 
# #   count(WD17NM, WD17CD, sort =T)
# # 
# # lkp_ward_to_pcon %>% filter(WD17NM == "Chester Villages" & WD17CD == "E05008665")
# 
# "CONSTITUENCIES MAY SPLIT WARDS  - 
# WILL HAVE TO CHECK BIRMINGHAM CASES TO SEE IF PROBLEM. "
# 
# lkp_msoa_to_pcon <- 
# lkp_msoa_to_ward %>% 
#   select(-WD17NM) %>% 
#   left_join(lkp_ward_to_pcon, by = "WD17CD") %>% 
#   # filter(WD17NM.x != WD17NM.y)
#   filter(MSOA11CD %in% vec_msoas_all) %>% 
#   janitor::clean_names() %>% 
#   # count(pcon17nm)
#   identity()
# 
# saveRDS(lkp_msoa_to_pcon, "lkp_msoa_to_pcon.rds")

# 1. START HERE ------------------------------------------------------------
lkp_msoa_to_pcon <- read_rds("lkp_msoa_to_pcon.rds")


# -------------------------------------------------------------------------

# # pop_eth_raw_msoa %>% skimr::skim()
# # 
# # pop_eth_raw_msoa %>% count(c_sex_name, c_sex_type, sort = T)
# # pop_eth_raw_msoa %>% count(c_age_name, c_age_type, sort = T)
# 
# 
# 
# pop_eth_msoa <- pop_eth_raw_msoa %>%
#   select(geography_name, geography_code, c_sex, c_sex_name,
#                    c_age, c_age_name, c_age_type,
#                    c_ethpuk11, c_ethpuk11_name, obs_value) 
# 
# # pop_eth %>% count(c_age_name)
# # 
# pop_eth_msoa <-
# pop_eth_msoa %>% 
#   rename(
#     msoa = geography_name,
#     msoa_code = geography_code,
#     sex = c_sex,
#     sex_name = c_sex_name,
#     age = c_age,
#     age_name = c_age_name,
#     age_type = c_age_type,
#     eth_code = c_ethpuk11,
#     ethnicity = c_ethpuk11_name,
#     n = obs_value
#     )
# 



# saveRDS(pop_eth_msoa, "pop_eth_msoa.rds")
pop_eth_msoa <- read_rds("pop_eth_msoa.rds")

# CHECKS WITH MANUAL DOWNLOAD - PASSED.
# pop_eth_msoa %>%
#   filter(str_detect(ethnicity, "Total")) %>%
#   filter(sex_name %in% c("Females", "Males")) %>%
#   filter(!str_detect(age_name, "All")) %>%
#   # summarise(sum(n))
#   # filter(lad == "Birmingham") %>%
#   filter(str_detect(msoa, "Birmingham")) %>%
#   summarise(n = sum(n)) %>%
#   ungroup()
# # # 1073045 (same as Local authority level checks)

# pop_eth_msoa %>% count(age, age_name) %>% print(n=100)

pop_eth_msoa_bc_and_bir <- pop_eth_msoa %>% 
  filter(sex_name %in% c("Females", "Males")) %>% 
  # REMOVE AGE - ALL CATEGORIES:
  filter(!str_detect(age_name, "All")) %>% 
  # "age" is actually age category - so 0 is all ages
  filter(!age == 0) %>% 
  # REGROUP TO 5 YEARS:
  mutate(age_grp = case_when(
    age_name == "Age 0 to 4"~ "00-04",
    age_name == "Age 5 to 7"~ "05-09",
    age_name == "Age 8 to 9"~ "05-09",
    age_name == "Age 10 to 14"~ "10-14",
    age_name == "Age 15"~ "15-19",
    age_name == "Age 16 to 17"~ "15-19",
    age_name == "Age 18 to 19"~ "15-19",
    age_name == "Age 20 to 24"~ "20-24",
    age_name == "Age 25 to 29"~ "25-29",
    age_name == "Age 30 to 34"~ "30-34",
    age_name == "Age 35 to 39"~ "35-39",
    age_name == "Age 40 to 44"~ "40-44",
    age_name == "Age 45 to 49"~ "45-49",
    age_name == "Age 50 to 54"~ "50-54",
    age_name == "Age 55 to 59"~ "55-59",
    age_name == "Age 60 to 64"~ "60-64",
    age_name == "Age 65 to 69"~ "65-69",
    age_name == "Age 70 to 74"~ "70-74",
    age_name == "Age 75 to 79"~ "75-79",
    age_name == "Age 80 to 84"~ "80-84",
    age_name == "Age 85 and over"~ "85+",
    T ~ NA_character_
  )) %>% 
  # JUST THE BROAD ETHNIC CATEGORIES:
  filter(str_detect(ethnicity, "Total")) %>% 
  group_by(msoa, msoa_code, sex, age_grp, ethnicity) %>% 
  summarise(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(sex = as.character(sex))


# 1. lkp msoa to la --------------------------------------------------------------------

# lkp_msoa_to_la <- read_csv("https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv", 
#                            col_types = cols(OA11CD = col_skip(), 
#                                             OAC11CD = col_skip(), OAC11NM = col_skip(), 
#                                             LSOA11CD = col_skip(), LSOA11NM = col_skip(), 
#                                             SOAC11CD = col_skip(), SOAC11NM = col_skip(), 
#                                             LACCD = col_skip(), LACNM = col_skip(), 
#                                             RGN11CD = col_skip(), RGN11NM = col_skip(), 
#                                             CTRY11CD = col_skip(), CTRY11NM = col_skip(), 
#                                             FID = col_skip()))
# 
# 
# saveRDS(lkp_msoa_to_la, "lkp_msoa_to_la.rds")
lkp_msoa_to_la <- read_rds("lkp_msoa_to_la.rds")

lkp_msoa_to_la <- lkp_msoa_to_la %>% 
  group_by(across()) %>% 
  summarise() %>% 
  ungroup %>% 
  janitor::clean_names() %>% 
  select(-msoa11nm)


pop_eth_places <- 
pop_eth_msoa_bc_and_bir %>% 
  # summarise(sum(n))
  left_join(lkp_msoa_to_pcon, by = c("msoa_code" = "msoa11cd")) %>% 
  left_join(lkp_msoa_to_la, by = c("msoa_code" = "msoa11cd")) %>% 
  # # count(is.na(pcon17nm))
  # count((pcon17nm)) %>%
  # # count((lad17nm)) %>% 
  # print(n=50) %>%
  # identity()
  mutate(place = case_when(
    # BC
    lad17nm == "Dudley" ~ "dudley",
    lad17nm == "Walsall" ~ "walsall",
    lad17nm == "Wolverhampton" ~ "wolves",
    lad17nm == "Sandwell" ~ "sandwell",
    pcon17nm %in% c("Birmingham, Ladywood", "Birmingham, Perry Barr") ~ "wbir",
    # BSOL
    lad17nm == "Solihull" ~ "solihull",
    pcon17nm %in% c("Birmingham, Selly Oak", "Birmingham, Hall Green") ~ "selly_hg",
    pcon17nm %in% c("Birmingham, Edgbaston", "Birmingham, Northfield") ~ "edgb_nf",
    pcon17nm %in% c("Birmingham, Yardley", "Birmingham, Hodge Hill") ~ "yard_hh",
    pcon17nm %in% c("Birmingham, Erdington", "Sutton Coldfield") ~ "erd_cold",
    T ~ NA_character_
  )) %>% 
  mutate(ics = case_when(
    place %in% c("dudley", "walsall", "wolves", "sandwell", "wbir") ~ "bc_wb",
    place %in% c("solihull", "selly_hg", "edgb_nf", "yard_hh", "erd_cold") ~ "bsol",
    T ~ NA_character_
  )) %>% 
  # count(ics, wt = n, sort = T) %>%
  # count(place, wt = n, sort = T) %>% 
  # summarise(sum(n)) %>%
  identity()


pop_eth_places <- pop_eth_places %>% 
  mutate(ethnicity = case_when(
    str_detect(ethnicity, "Asian") ~ "Asian",
    str_detect(ethnicity, "Black") ~ "Black",
    str_detect(ethnicity, "Mixed") ~ "Mixed",
    str_detect(ethnicity, "Other") ~ "Other",
    str_detect(ethnicity, "White") ~ "White",
    T ~ NA_character_
  ))


# got to here -------------------------------------------------------------

# pop_bc_age_grp # (we want it in this format)
pop_eth_final <-
  pop_eth_places %>%
  # count(ccg, lad)
  # filter(ccg != "bsol") %>%
  group_by(ics, place, ethnicity, sex, age_grp_85_max = age_grp) %>% 
  summarise(pop_local = sum(n)) %>% 
  ungroup


# -------------------------------------------------------------------------

# CROSS REF. WITH 2018 POPS - SHOULD BE LOWER

# CAN CHECK PROCESS WITH MANUAL DOWNLOAD FROM NOMIS (WOULD HAVE BEEN EASIER)
# BUT COULD USE NOMIS IN FUTURE - ESPECIALLY FOR ANNUAL POP SURVEY FOR ETHINICITY CORRECTIONS.
# http://www.nomisweb.co.uk/census/2011/all_tables?release=3.1
# tmpeth <- read_csv("nomis_eth_manual_download.csv")



