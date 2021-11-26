library(tidyverse)
library(patchwork)
library(lubridate)
library(ggrepel)
library(janitor)
library(dbplyr) 
library(odbc)
library(DBI)
library(gt)
# options(scipen = 11L)
# source("C:/themea.r")
# theme_set(theme_b)


# 1. LooKuPs -----------------------------------------------------------

source("lkp_geog.r")
source("lkp_imd.r")
source("lkp_icd.r")

# 1. ACTIVITY -------------------------------------------------------------

source("hes_data.r")

# 2. POP ------------------------------------------------------------------
"THESE POPULATIONS ARE FOR SPECIFIED GEOGRAPHIES ONLY - BUT CAN BE ADJUSTED TO SUIT NEEDS"

source("pop_imd.r")     # populations for geography by: age_grp, sex, IMD
source("pop_eth_msoa.r") # populations for geography by: age_grp, sex, ETHNICITY
source("pop_euro.r")


# ~~~~~~~~~~~ ---------------------------------------------------------------------

# 3. DEATHS -------------------------------

# source("deaths.r")

# ~~~~~~~~~~~ ---------------------------------------------------------------------

# 4. GEO SUBSETS -------------------------------

# a. ics ------------------------------------------------------------------

level_ics_all <- ip %>%
  # So that place is not duplicated - when binding to place dfs and using 'nest' in future):
  select(-place) %>% 
  group_by(ics) %>%
  nest %>%
  ungroup

level_ics_segment <-
  ip %>%
  # So that place is not duplicated - when binding to place dfs and using 'nest' in future):
  select(-place) %>%
    mutate(segment = case_when(
      age >= 55 & age <75 ~ "55-74",
      age >= 75 ~ "75+",
      T ~ NA_character_
    )) %>%
    filter(!is.na(segment)) %>%
    group_by(ics, segment) %>%
    nest %>%
    ungroup

(level_ics <- bind_rows(level_ics_all, level_ics_segment %>% arrange(segment)) %>% 
  arrange(ics))

# b. place -------------------------------------------------------

level_place_all <- ip %>%
  group_by(ics, place) %>%
  nest %>%
  ungroup


level_place_segment <-
  ip %>%
  mutate(segment = case_when(
    age >= 55 & age <75 ~ "55-74",
    age >= 75 ~ "75+",
    T ~ NA_character_
  )) %>%
  filter(!is.na(segment)) %>%
  group_by(ics, place, segment) %>%
  nest %>%
  ungroup


(level_place <- bind_rows(level_place_all, level_place_segment %>% arrange(segment)) %>% 
    arrange(ics))


# c. both ------------------------------------------------------------------

(level_both <- 
   bind_rows(level_ics, level_place) %>% 
     select(ics, place, segment, data)
)


# ~~~~~~~~~~~ ----------------------------------------------------------------

# 5. BRANCHES -----------------------------------------------------------

source("branch_imd.r")
source("branch_eth.r")

# 6. STATS ----------------------------------------------------------------

# STANDARDISED RATES AND INDICES OF DISPARITY:
source("rates_imd.r")
source("rates_eth.r")

# ~~~~~~~~~~~ ----------------------------------------------------------------

# 7. METRICS  --------------------------------------------------------
# ADDTIONAL METRICS

# source("metrics_extra.r")

beepr::beep(sound = 2)

