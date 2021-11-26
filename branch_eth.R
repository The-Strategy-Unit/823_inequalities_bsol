# ETHNICITY SPECIFIC ------------------------------------------------------------

# QUESTION - HOW TO TREAT NA ("NOT KNOWN") ETHNICIY VALUES:
# 1. "NOT KNOWN" EQUAL SHARES?
# 2. "NOT KNOWN" PROPORTIONAL - IN LINE WITH REST OF DATA?
"# 3. NOT KNOWN EXCLUDE"
# CHOSEN OPTION 3 HERE
  

# EDA ---------------------------------------------------------------------

# level_ics$data[[1]] %>% select(starts_with("eth")) %>% count(ethnic_category_description, ethnic_category_name)
# 
# level_ics$data[[1]] %>%   # ALL   6.3% na
# # level_ics$data[[3]] %>% # 55-75 5.3% na
# # level_ics$data[[4]] %>% # 75+   3.5% na
#   select(starts_with("eth")) %>%
#   count(ethnic_category_name) %>%
#   mutate(p = n/sum(n))
# 
# level_ics$data[[1]] %>%
#   # select(starts_with("eth")) %>%
#   count(ethnic_category_name, icd_chap) %>%
#   group_by(icd_chap) %>% 
#   mutate(p = n/sum(n)) %>% 
#   select(-n) %>% 
#   pivot_wider(names_from = icd_chap, values_from = p) %>% 
#   view
# 
# level_ics$data[[1]] %>% 
#   # select(starts_with("eth")) %>%
#   count(ethnic_category_name, icd_chap) %>%
#   group_by(icd_chap) %>% 
#   mutate(p = n/sum(n)) %>% 
#   # select(-n) %>%
#   filter(ethnic_category_name == "")
#   # pivot_wider(names_from = icd_chap, values_from = p) %>% 
#   # view

# NAs are 3-7% across ICD chapters except for || 15 - Pregnancy at 20% ofin group total (n-1367)||
# 21 - Factors influencing health status/contact at 14% of in-group total (n=143)

# level_ics$data[[1]]

eth_impute <- level_both %>% 
  # unnest(data) %>% 
  mutate(data = map(data, function(df) {
    df %>% 
      # FILL IMPLICIT MISSINGS- NEEDED FOR LATER CALCS: 
      complete(ethnicity, nesting(icd_chap, icd_chap_name, icd_chap_short))
  })) %>% 
  unnest(data) %>% 
  group_by(ics, place, segment, ethnicity, icd_chap, icd_chap_name, icd_chap_short) %>% 
  nest() %>% 
  ungroup()

eth_impute <- eth_impute %>% 
  # HERE IS WHERE WE EXCLUDE "UNKNOWN" ETHNICITY:
  filter(!is.na(ethnicity))


eth <- eth_impute %>% 
  group_by(ics, place, segment) %>% 
  nest() %>% 
  arrange(ics)

# ADD POPULATIONS BY ETHNICITY:

# pop_eth_bc %>% count(age_grp)

eth_2 <-
  eth %>% # print(n=40)
    mutate(pop = case_when(
      # 1. BLACK COUNTRY
      ics == "bc_wb" & is.na(place) & is.na(segment)     ~  list(pop_eth_final %>% filter(ics == "bc_wb")),
      ics == "bc_wb" & is.na(place) & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bc_wb" & is.na(place) & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      #
      ics == "bc_wb" & place == "dudley"   & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "dudley")),
      ics == "bc_wb" & place == "sandwell" & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "sandwell")),
      ics == "bc_wb" & place == "wbir"     & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "wbir")),
      ics == "bc_wb" & place == "walsall"  & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "walsall")),
      ics == "bc_wb" & place == "wolves"   & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "wolves")),
      #
      ics == "bc_wb" & place == "dudley"   & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "dudley") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bc_wb" & place == "sandwell" & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "sandwell") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bc_wb" & place == "wbir"     & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "wbir") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bc_wb" & place == "walsall"  & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "walsall") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bc_wb" & place == "wolves"   & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "wolves") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      #
      ics == "bc_wb" & place == "dudley"   & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "dudley") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bc_wb" & place == "sandwell" & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "sandwell") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bc_wb" & place == "wbir"     & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "wbir") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bc_wb" & place == "walsall"  & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "walsall") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bc_wb" & place == "wolves"   & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bc_wb") %>% filter(place == "wolves") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      #
      # 2. BSOL
      #
      ics == "bsol" & is.na(place) & is.na(segment)     ~  list(pop_eth_final %>% filter(ics == "bsol")),
      ics == "bsol" & is.na(place) & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bsol" & is.na(place) & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      #
      ics == "bsol" & place == "selly_hg" & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "selly_hg")),
      ics == "bsol" & place == "yard_hh"  & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "yard_hh")),
      ics == "bsol" & place == "edgb_nf"  & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "edgb_nf")),
      ics == "bsol" & place == "erd_cold" & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "erd_cold")),
      ics == "bsol" & place == "solihull" & is.na(segment) ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "solihull")),
      #
      ics == "bsol" & place == "selly_hg" & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "selly_hg") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bsol" & place == "yard_hh"  & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "yard_hh") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bsol" & place == "edgb_nf"  & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "edgb_nf") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bsol" & place == "erd_cold" & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "erd_cold") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      ics == "bsol" & place == "solihull" & segment == "55-74" ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "solihull") %>% filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))),
      #
      ics == "bsol" & place == "selly_hg" & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "selly_hg") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bsol" & place == "yard_hh"  & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "yard_hh") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bsol" & place == "edgb_nf"  & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "edgb_nf") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bsol" & place == "erd_cold" & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "erd_cold") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      ics == "bsol" & place == "solihull" & segment == "75+"   ~  list(pop_eth_final %>% filter(ics == "bsol") %>% filter(place == "solihull") %>% filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))),
      #
      T ~ list(NULL)))
  
    
    
    
    
  # mutate(pop = enframe(rep(pop_bc_age_grp, vctrs::vec_size(data)))) %>%
  # mutate(pop_bc = rep(list(pop_bc_age_grp), vctrs::vec_size(data))) %>%
  # mutate(pop_bc = ifelse(is.na(segment) & ics == "bc", list(pop_eth_bc), NA)) %>% 
  # ###
  # mutate(pop_bc_55 = ifelse(segment == "55-74" & ics == "bc",
  #                           list(pop_eth_bc %>%
  #                                  filter(age_grp_85_max %in% c("55-59", "60-64", "65-69", "70-74"))), NA)) %>% 
  # ###
  # mutate(pop_bc_75 = ifelse(segment == "75+" & ics == "bc",
  #                           list(pop_eth_bc %>%
  #                                  filter(age_grp_85_max %in% c("75-79", "80-84", "85+"))), NA))


# eth_long <- eth %>% 
#   pivot_longer(cols = starts_with("pop"), names_to = "pop_seg", values_to = "pop_data",
#                values_drop_na = T
#   )


# eth_long$pop_data[[3]]

eth_unnest <-
  # eth_long %>%
  eth_2 %>% 
  unnest(data)

