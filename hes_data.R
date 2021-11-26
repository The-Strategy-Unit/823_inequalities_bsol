
con_hes <- dbConnect(odbc(),
                     Driver = 'SQL Server',
                     Server = 'MLCSU-BI-SQL',
                     Database = 'HESDATA',
                     Trusted_Connection = 'True')
# #

tb_ip <- tbl(con_hes, in_schema("dbo", "tbInpatients1819"))

# # 
# tb_ip %>% 
#   colnames() %>%
#   enframe %>%
#   filter(str_detect(value, "imd"))

# SLIGHTLY MORE SPELLENDS THAN EPIORDER ==1 ?
# tb_ip %>% 
#   filter(ccg_residence %in% c(
#       "05C", "05L", "05Y", "06A",
#       "15E", # NEW BSOL
#       "04X", "13P", "05P" # OLD BIR & SOL
#       )) %>%
#   #   # ALTERNATIVE: filter(ccg_responsibility)
#     filter(sex %in% c(1, 2)) %>%
#     filter(admidate >= "2018-04-01") %>%
#     filter(admidate <= "2019-03-31") %>%
#   #   # EMERGENCY ADMISSIONS:
#     filter(admimeth %in% c('21', '22', '23', '24', '28', '2A', '2B', '2C', '2D')) %>% 
#   count(spelend, epiorder ==1)

#
# 
# # 1. HES IP QUERY -----------------------------------------------------------
# 
raw_ip <-
tb_ip %>%
  # define the geographical boundaries:
  filter(ccg_residence %in% c(
    "05C", "05L", "05Y", "06A",
    "15E", # NEW BSOL
    "04X", "13P", "05P" # OLD BIR & SOL
    )) %>%
  # ALTERNATIVE: filter(ccg_responsibility)
  filter(sex %in% c(1, 2)) %>%
  filter(admidate >= "2018-04-01") %>%
  filter(admidate <= "2019-03-31") %>%
  # EMERGENCY ADMISSIONS:
  filter(admimeth %in% c('21', '22', '23', '24', '28', '2A', '2B', '2C', '2D')) %>%
 # CHOICE OF THESE TWO - VERSION 2 TRYING SPELEND:
   # filter(epiorder == 1) %>%
    filter(spelend == "Y") %>%
  # ALSO POSSIBLE SPELEND = "Y"
#   # NOT "WELL BABIES":
  filter(tretspef != "424") %>%
  count(
    lsoa11,
    ccg_residence,
    sex,
    admiage,
    ethnos,
    diag_01
  ) %>%
  collect() %>%
  ungroup

# 
# # 1. LKP ETHNICITY -----------------------------------------------------------
# 

con_ref <- dbConnect(odbc(),
                     Driver = 'SQL Server',
                     Server = 'MLCSU-BI-SQL',
                     Database = 'Reference',
                     Trusted_Connection = 'True')
#
tb_lkp_eth <- tbl(con_ref, in_schema("dbo", "DIM_tbEthnicCategory"))
#
lkp_eth <- tb_lkp_eth %>%
  select(EthnicCategoryCode, EthnicCategoryDescription, EthnicCategoryName) %>%
  collect()

lkp_eth <- lkp_eth %>%
  janitor::clean_names()
# 
# lkp_eth %>% count(ethnic_category_name)
# 
# # 1. JOIN AND SAVE --------------------------------------------------------

raw_ip <- raw_ip %>%
  left_join(lkp_eth, by = c("ethnos" = "ethnic_category_code"))

# 
# saveRDS(raw_ip, "raw_ip.rds")
# VERSION 2 WITH SPELEND INSTEAD OF EPIORDER ==1 
# saveRDS(raw_ip, "raw_ip_v2.rds")


# raw_ip <- read_rds("raw_ip.rds")
# raw_ip <- read_rds("raw_ip_v2.rds")


# 1. LABELS ADD ETH  ------------------------------------------

raw_ip <- raw_ip %>% 
  mutate(ethnicity = case_when(
    str_detect(ethnic_category_name, "Asian") ~ "Asian",
    str_detect(ethnic_category_name, "Black") ~ "Black",
    str_detect(ethnic_category_name, "Mixed") ~ "Mixed",
    str_detect(ethnic_category_name, "Other") ~ "Other",
    str_detect(ethnic_category_name, "White") ~ "White",
    T ~ NA_character_
  )) 



"PROBLEM - 'OTHER' ETHNIC CATEGORY APPEARS TO BE OVERUSED IN HES TABLES
 MAIN REASON LIKELY BECAUSE IT CONTAINS 'NOT STATED' (72%)
 IF WE REALLOCATE THESE TO 'NOT KNOWN' (AND THUS BECOME NAs) WE MAY GET MORE ACCURATE STATS"

raw_ip <- raw_ip %>% 
  mutate(ethnicity = ifelse(ethnic_category_description == "Not stated", NA, ethnicity))
  
# 2. CLEAN ----------------------------------------------------------------

# WITH EDITS BELOW, 98% HAVE AN ASSOCIATED DIAGNOSIS AND DIAG CHAP:
ip <-
  raw_ip %>% # summarise(sum(n))
  rename(diag = diag_01, age = admiage, ccg = ccg_residence, lsoa = lsoa11) %>% 
  # ALL THE SAME/ UNNECESSARY FOR NEXT PROCESSING STEPS:
  mutate(diag_primary = case_when(
    str_detect(diag, "^I48") ~ "I48X", 
    str_detect(diag, "^J96") ~ "J96",
    str_detect(diag, "^K43") ~ "K43",
    str_detect(diag, "^K64") ~ "K639", # Guess - small numbers
    TRUE ~ diag
  )) %>% 
  # count(is.na(diag_primary)) %>% 
  # mutate(p = n/sum(n))
  left_join(lkp_icd10, by = c("diag" = "icd_code")) %>% 
  # count(is.na(icd_chap)) %>%
  # mutate(p = n/sum(n))
  select(-diag_primary, -icd_sub)

ip <- ip %>%
  filter(!is.na(icd_chap)) %>% 
  uncount(n)


ip <- ip %>% 
  mutate(age_grp = case_when(
    age %in% 0:4 ~ "00-04",
    age %in% 5:9 ~ "05-09",
    age %in% 10:14 ~ "10-14",
    age %in% 15:19 ~ "15-19",
    age %in% 20:24 ~ "20-24",
    age %in% 25:29 ~ "25-29",
    age %in% 30:34 ~ "30-34",
    age %in% 35:39 ~ "35-39",
    age %in% 40:44 ~ "40-44",
    age %in% 45:49 ~ "45-49",
    age %in% 50:54 ~ "50-54",
    age %in% 55:59 ~ "55-59",
    age %in% 60:64 ~ "60-64",
    age %in% 65:69 ~ "65-69",
    age %in% 70:74 ~ "70-74",
    age %in% 75:79 ~ "75-79",
    age %in% 80:84 ~ "80-84",
    age %in% 85:89 ~ "85-89",
    age >= 90 ~ "90+",
    TRUE ~ NA_character_
  )) %>% 
  relocate(age_grp, .after = age)


ip <- ip %>% 
  mutate(age_grp_85_max = case_when(
    age >= 85 ~ "85+",
    TRUE ~ age_grp
    )) %>% 
    relocate(age_grp_85_max, .after = age_grp)


# 3. JOIN LKPS-------------------------------------------------------------------------

# ADD VARIOUS GEOGRAPHICAL BOUNDARIES:
ip <- lkp_lsoa_set %>% 
  right_join(ip, by = c("lsoa")) %>%
  select(-ccg20cdh ) %>% 
  rename(ccg_name = ccg20nm) %>% 
  relocate(ccg, .before = ccg_name)

# IMD
ip <- ip %>%
  left_join(lkp_imd, by = c("lsoa" = "lsoa_code"))

