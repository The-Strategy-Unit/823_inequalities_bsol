
# 1.  BASIC CONVERSION TABLE ----------------------------------------------

# from: https://geoportal.statistics.gov.uk/datasets/1631beea57ff4e9fb90d75f9c764ce26_0
# lkp_lsoa <- read_csv("https://opendata.arcgis.com/datasets/f0b39d6872dd4533aaf3941846134a89_0.csv")
# lkp_lsoa <- read_csv("https://opendata.arcgis.com/datasets/1631beea57ff4e9fb90d75f9c764ce26_0.csv")
# saveRDS(lkp_lsoa, "lkp_lsoa.rds")
lkp_lsoa <- read_rds("lkp_lsoa.rds") %>% janitor::clean_names()


lkp_lsoa_set <- lkp_lsoa %>% 
  filter(ccg20cdh %in% c(
  "05C", "05L", "05Y", "06A", # BLACK COUNTRY STP
  "15E"  # BSOL ICS
)) %>% 
  select(-starts_with("cal"), - fid, -lsoa11nm, -ccg20cd, -starts_with("stp")) %>% 
  rename(lsoa = lsoa11cd)


# 2. LKP CONSTITUENCY -----------------------------------------------------------------
# for place level analysis

# lkp_lsoa_to_ward <- read_csv("https://opendata.arcgis.com/datasets/500d4283cbe54e3fa7f358399ba3783e_0.csv",
#   col_types = cols(
#     WD17NMW = col_skip(),
#     LAD17CD = col_skip(), LAD17NM = col_skip(),
#     FID = col_skip()
#   )
# )
# 
# lkp_ward_to_pcon <- read_csv("https://opendata.arcgis.com/datasets/077f8814334e453883e12479098c152c_0.csv",
#   col_types = cols(
#     LAD17CD = col_skip(),
#     LAD17NM = col_skip(), UTLA17CD = col_skip(),
#     UTLA17NM = col_skip(), FID = col_skip()
#   )
# )
# 
# lkp_lsoa_to_pcon <-
#   lkp_lsoa_to_ward %>%
#     select(-WD17NM) %>%
#     left_join(lkp_ward_to_pcon, by = "WD17CD") %>%
#   #   # filter(WD17NM.x != WD17NM.y)
#   #   filter(MSOA11CD %in% vec_msoas_all) %>% 
#   #   janitor::clean_names() %>% 
#   #   # count(pcon17nm)
#     identity()
# # 
# # saveRDS(lkp_lsoa_to_pcon, "lkp_lsoa_to_pcon.rds")

# SOME WARDS SPAN LSOAs...
lkp_lsoa_to_pcon <- read_rds("lkp_lsoa_to_pcon.rds") %>% 
  janitor::clean_names()

# 3. ADD CONSTITUENCY -----------------------------------------------------------------------

# ...BUT IN OUR SUBSET, WARDS (THANKFULLY) DO NOT SPAN LSOAS
lkp_lsoa_set <- lkp_lsoa_set %>% 
  left_join(lkp_lsoa_to_pcon, by = c("lsoa" = "lsoa11cd")) %>% 
  # count(lad20nm)
  mutate(place = case_when(
    # BC
    lad20nm == "Dudley" ~ "dudley",
    lad20nm == "Walsall" ~ "walsall",
    lad20nm == "Wolverhampton" ~ "wolves",
    lad20nm == "Sandwell" ~ "sandwell",
    pcon17nm %in% c("Birmingham, Ladywood", "Birmingham, Perry Barr") ~ "wbir",
    # BSOL
    lad20nm == "Solihull" ~ "solihull",
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
  # count(ics, sort = T) %>%
  # count(place, sort = T) %>%
  # summarise(sum(n)) %>%
  identity()

# lkp_lsoa_set %>% count(ics, place)

# lsoa_set <- lkp_lsoa_set %>% pull(lsoa11cd)


# lkp_lsoa %>% count(CCG20NM)
# lkp_lsoa %>% filter(str_detect(CCG20NM, "Sandwell"))
# lkp_lsoa %>% filter(str_detect(CCG20NM, "Sandwell"))
# lkp_lsoa %>% filter(str_detect(stp20nm, "Black")) %>% count(ccg20nm)
# 
# # Walsall CCG
# # Sandwell and West Birmingham CCG
# # Dudley CCG
# # Wolverhampton CCG
# 
# # lkp_lsoa %>% 
# #   count()
# 
# lkp_lsoa %>% 
#   filter(str_detect(STP20NM, "Black Country")) %>%
#   # count(CCG20NM) %>% 
#   count(LAD20NM, CCG20NM, CCG20CDH) 
# 
# 
# lkp_lsoa %>% # count(CCG20NM, CCG20CDH)
#   filter(CCG20CDH == "15E") %>% 
#   count(LAD20NM)
# 
# # THE ISSUE IS THAT BIRMINGHAM LA IS SPLIT OVER THESE TWO HEALTH ICSs
# 
