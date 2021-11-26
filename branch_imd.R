
# IMD SPECIFIC ------------------------------------------------------------

imd_impute <- level_both %>% 
  # unnest(data) %>% 
  mutate(data = map(data, function(df) {
    df %>% 
      # FILL IMPLICIT MISSINGS- NEEDED FOR LATER CALCS: 
      complete(imd_quint, nesting(icd_chap, icd_chap_name, icd_chap_short))
  })) %>% 
  unnest(data) %>% 
  group_by(ics, place, segment, imd_quint, icd_chap, icd_chap_name, icd_chap_short) %>% 
  nest() %>% 
  ungroup()


# notes -------------------------------------------------------------------

# imd_impute[c(1, 177, 988, 1299, 3000),]
# 
# imd_impute$data[[1]]
# 
# test <- imd_impute %>%
#   mutate(x = map_dbl(data, ~vctrs::vec_size(.$ccg))) %>%
#   arrange(x) 

#   ggplot(aes(x))+
#   geom_histogram()+
#   xlim(0, 300)+
#   geom_vline(xintercept = 30)
"# THERE WILL BE MANY GROUPS WITH NUMBERS UNDER 30 "


# cont... ---------------------------------------------------------------

imd <- imd_impute %>% 
  group_by(ics, place, segment) %>% 
  nest() %>% 
  arrange(ics) %>% 
  # print(n =20)
  identity

# imd$data[[6]] %>% count(icd_chap)
# imd$data[[6]] %>% count(imd_quint)


"IT SHOULD NOT ACTUALLY MATTER THAT THERE ARE NO 15 AND 16 CHAPTERS FOR SEGMENTS 3 AND 4"
"want to add populations to the df such that the ages match the segments eg. "

# imd %>% 
#   # arrange(ics) %>%
#   print(n= 36)

imd_2 <-
  imd %>%
  mutate(pop = case_when(
  # 1. BLACK COUNTRY
    ics == "bc_wb" & is.na(place) & is.na(segment)     ~  list(pop_imd_final %>% filter(ics == "bc_wb")),
    ics == "bc_wb" & is.na(place) & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bc_wb" & is.na(place) & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    #
    ics == "bc_wb" & place == "dudley"   & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "dudley")),
    ics == "bc_wb" & place == "sandwell" & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "sandwell")),
    ics == "bc_wb" & place == "wbir"     & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "wbir")),
    ics == "bc_wb" & place == "walsall"  & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "walsall")),
    ics == "bc_wb" & place == "wolves"   & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "wolves")),
    #
    ics == "bc_wb" & place == "dudley"   & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "dudley") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bc_wb" & place == "sandwell" & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "sandwell") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bc_wb" & place == "wbir"     & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "wbir") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bc_wb" & place == "walsall"  & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "walsall") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bc_wb" & place == "wolves"   & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "wolves") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    #
    ics == "bc_wb" & place == "dudley"   & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "dudley") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bc_wb" & place == "sandwell" & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "sandwell") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bc_wb" & place == "wbir"     & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "wbir") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bc_wb" & place == "walsall"  & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "walsall") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bc_wb" & place == "wolves"   & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bc_wb") %>% filter(place == "wolves") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    #
    # 2. BSOL
    #
    ics == "bsol" & is.na(place) & is.na(segment)     ~  list(pop_imd_final %>% filter(ics == "bsol")),
    ics == "bsol" & is.na(place) & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bsol" & is.na(place) & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    #
    ics == "bsol" & place == "selly_hg" & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "selly_hg")),
    ics == "bsol" & place == "yard_hh"  & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "yard_hh")),
    ics == "bsol" & place == "edgb_nf"  & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "edgb_nf")),
    ics == "bsol" & place == "erd_cold" & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "erd_cold")),
    ics == "bsol" & place == "solihull" & is.na(segment) ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "solihull")),
    #
    ics == "bsol" & place == "selly_hg" & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "selly_hg") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bsol" & place == "yard_hh"  & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "yard_hh") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bsol" & place == "edgb_nf"  & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "edgb_nf") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bsol" & place == "erd_cold" & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "erd_cold") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    ics == "bsol" & place == "solihull" & segment == "55-74" ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "solihull") %>% filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
    #
    ics == "bsol" & place == "selly_hg" & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "selly_hg") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bsol" & place == "yard_hh"  & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "yard_hh") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bsol" & place == "edgb_nf"  & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "edgb_nf") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bsol" & place == "erd_cold" & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "erd_cold") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    ics == "bsol" & place == "solihull" & segment == "75+"   ~  list(pop_imd_final %>% filter(ics == "bsol") %>% filter(place == "solihull") %>% filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
    #
    T ~ list(NULL)))
 
# imd_2 %>% print(n=40)
    
  #    ###
  # ###
  # # 1. BSOL; ALL
  #   is.na(segment) & ics == "bsol" ~  list(pop_imd_final %>% filter(ics == "bsol")),
  # # 2. BSOL; 55
  #   segment == "55-74" & ics == "bsol" ~  list(pop_imd_final %>%
  #                                                 filter(ics == "bsol") %>%
  #                                                 filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))),
  # # 3. BSOL; 75
  #   segment == "75+" & ics == "bsol" ~  list(pop_imd_final %>%
  #                                                 filter(ics == "bsol") %>%
  #                                                 filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))),
  # 



# imd_2$pop[[3]] 
# imd_2$pop[[6]] 
# imd_2$pop[[3]] %>% print(n=180)
# imd_2$pop[[6]] %>% print(n=180)

# mutate(pop_bc_55 = rep(
#   list(
#        pop_bc_age_grp %>%
#          filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))
#       ),
#   vctrs::vec_size(data)
# )) %>%
# mutate(pop_bc_75 = rep(
#   list(
#     pop_bc_age_grp %>%
#       filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))
#   ),
#   vctrs::vec_size(data)
# ))
# mutate(pop = list(pop_bc_age_grp)) %>% 
# unnest(pop)


# UNNECESSARY IN THIS VERSION:
# imd_long <- imd_2 %>% 
#   pivot_longer(cols = starts_with("pop"), names_to = "pop_seg", values_to = "pop_data",
#                values_drop_na = T
#   )

imd_unnest <-
  # imd_long %>% 
  imd_2 %>% 
  unnest(data)


# ROUGH (ALTERNATIVE STRUCTURE) ---------
# 
# level_ics_all %>% 
#   # unnest(data) %>% 
#   mutate(data = map(data, function(df) {
#     df %>% 
#       # FILL IMPLICIT MISSINGS- NEEDED FOR LATER CALCS: 
#       complete(imd_quint, nesting(icd_chap, icd_chap_name))
#   })) %>% 
#   unnest(data) %>% 
#   group_by(ics, segment, imd_quint, icd_chap, icd_chap_name) %>% 
#   nest() %>% 
#   ungroup()
#  
# revamp <- level_ics_all %>% 
#   # IF YOU WANTED TO INCLUDE BSOL WOULD HAVE TO LOOK AT POPULATIONS
#   filter(ics == "bc") %>% 
#   crossing(tibble(pop_bc = rep(list(pop_bc_age_grp), vctrs::vec_size(level_ics_all$data)))) %>% 
#   crossing(tibble(pop_bc_55 = rep(
#     list(
#       pop_bc_age_grp %>%
#         filter(age_grp %in% c("55-59", "60-64", "65-69", "70-74"))
#     ),
#     vctrs::vec_size(level_ics_all$data)
#   ))) %>% 
#   crossing(tibble(pop_bc_75 = rep(
#     list(
#       pop_bc_age_grp %>%
#         filter(age_grp %in% c("75-79", "80-84", "85-89", "90+"))
#     ),
#     vctrs::vec_size(level_ics_all$data)
#   ))) %>% 
#   pivot_longer(cols = starts_with("pop"), names_to = "pop_seg", values_to = "pop_data")
# 
# 