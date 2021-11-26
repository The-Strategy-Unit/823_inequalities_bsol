library(furrr) # https://davisvaughan.github.io/furrr/
plan(multisession, workers = 3)

# ETHNICITY CALCS USE A MAXIMUM AGE GROUP OF 85+ (RATHER THAN 90+)
# DUE TO CENSUS 2011 LIMITATIONS

# eth_unnest$data[[1]]
# eth_unnest$pop_data[[1]] %>% count(age_grp_85_max)

# 1. ----------------------------------------------------------------------

# rate_101[101,]
# rate_101$calc_crude[101]
tic()
rate_701 <- eth_unnest %>%
  ungroup %>% 
  mutate(calc_crude = future_pmap(list(data, pop, ethnicity), function(df_actv, df_pop, var) {
    # THIS WOULD NEED TO CHANGE:
    df_pop %>%
      filter(ethnicity == var) %>%
      count(sex, age_grp_85_max, wt = pop_local, name = "pop_local") %>% 
      left_join(
        df_actv %>%
          count(sex, age_grp_85_max),
        by = c("age_grp_85_max", "sex")
      ) %>%
      group_by(sex, age_grp_85_max) %>%
      summarise(n = sum(n), pop_local = sum(pop_local)) %>%
      ungroup() %>%
      mutate(rate_local = n / pop_local) %>%
      identity()
  }))


# 2. ----------------------------------------------------------------------

rate_702 <- rate_701 %>% 
  mutate(calc_std = future_map(calc_crude,
                        function(df) {
                          
                          euro_2013_85_max %>% 
                            right_join(df, by = c("age_grp_85_max", "sex")) %>%
                            mutate(std_number = rate_local * pop_euro) %>%
                            mutate(std_rate = sum(std_number, na.rm = T)/sum(pop_euro, na.rm = T)) %>%
                            identity
                        }))

toc()
plan(sequential)
# 3 workers - 120 secs

# 3.  ---------------------------------------------------------------------

rate_703 <- rate_702 %>% 
  mutate(summary = map(calc_std, function(df) {
    df %>% 
      summarise(
        n = sum(n, na.rm = T),
        std_rate = max(std_rate),
        pop_local = sum(pop_local, na.rm = T)
      )
  })) %>% 
  unnest(summary)


# 4. ----------------------------------------------------------------------

rate_704 <- rate_703 %>%
  arrange(ics, icd_chap, ethnicity) %>% 
  group_by(ics, place, segment, icd_chap, icd_chap_name, icd_chap_short) %>% 
  nest()

# 5.  ---------------------------------------------------------------------

rate_705 <- rate_704 %>% 
  mutate(results = map(data, function(df) {
    df %>% 
      mutate(mu = sum(std_rate *pop_local)/sum(pop_local)) %>% 
      mutate(diffs_abs = abs(std_rate - mu)) %>% 
      mutate(diffs_weighted = diffs_abs * pop_local) %>% 
      mutate(diffs_sum = sum(diffs_weighted)) %>% 
      mutate(index = diffs_sum/mu) %>% 
      # The half is to average out the effect of negative values 
      # that have been transformed to positive with the abs diff
      # EASIEST TO SEE IN A GRAPHICAL (BAR CHART AVERAGING EXAMPLE)
      mutate(index_relative = index*(1/2)*(1/sum(pop_local))) %>% 
      mutate(index_abs = index_relative * sum(n)) %>% 
      select(starts_with("index")) %>% 
      slice(1)
    
  })) %>% 
  # ungroup %>% 
  unnest(results) 

# rate_104 %>% 
#   ungroup %>% 
#   slice(1) %>% 
#   unnest(data)
# 
# rate_705[c(1, 19, 37),]


# Tables ------------------------------------------------------------------

rate_table_eth <-
  rate_705 %>% 
  unnest(data) %>% 
  group_by(ics, place, segment, icd_chap, icd_chap_name, icd_chap_short) %>% 
  summarise(
    pop_local = sum(pop_local),
    adms = sum(n),
    index_relative = max(index_relative),
    index_abs = max(index_abs)
  ) %>% 
  ungroup

# rate_table_eth[c(1, 19, 37), ]
# 
# rate_table_eth %>% arrange(-adms)
# rate_table_eth %>% filter(icd_chap == 18)
# 
# rate_705 %>% filter(icd_chap == 18) %>% filter(is.na(segment)) %>% unnest(data)

# Plot disparity----------------------------------------------------------
# for different levels of ics, place, and segment

rate_table_eth_plots <-
  rate_table_eth %>%
  # NOT 17. CONGENITAL - SMALL NOS WHERE HIGH RELATIVE INEQUITY DOMINATES GRAPHIC:
  filter(icd_chap != 17) %>%
  # CODE TO SCALE X AXIS:
  group_by(ics, place, segment) %>%
  mutate(max_rel = max(index_relative)) %>%
  ungroup %>%
  left_join(impact %>% select(ics, icd_chap, years_lost), by = c("ics", "icd_chap")) %>%
  # arrange(-max_rel)
  # print(n = 40)
  group_by(ics, place, segment, max_rel) %>% 
  nest() %>% 
  mutate(plots = map2(data, max_rel, function(df, var) {
    df %>% 
      ggplot(aes(index_relative, index_abs))+
      geom_point(
        # aes(size = years_lost),
        alpha = .5,
        size = .4,
        col = "dodgerblue"
      ) +
      # ADDITIONAL POINT SIZED BY IMPACT:
      geom_point(
        aes(size = years_lost),
        col = "dodgerblue",
        alpha = .25
      ) +
      geom_text_repel(
        # aes(label = str_trunc(icd_chap_name, 25)),
        aes(label = icd_chap_short),
        col = "grey50"
      )+
      scale_x_continuous( 
        # aes(
        limits = c(0, var),
        # ),
        #     breaks = seq(0, .16, 0.04),
        # labels = str_c(round(.), "%")
        # labels = scales::percent
        labels = scales::label_percent(accuracy = 1)
      )+
      scale_y_continuous(
        # limits = c(0, 3000),
        labels = scales::comma)+
      scale_size_continuous(range = c(1, 12), labels = scales::comma) +
      
      #   # ylim(0, 160)+
      #   # DO AXIS TITLES IN PWPT:
      theme(axis.title = element_blank(),
            legend.justification = "top",
            
            # legend.position=c(.95,.85),
            legend.title = element_blank(),
            legend.direction = "horizontal"
            )+
        NULL
    # 
    
  }))

# rate_table_eth_plots %>% arrange(max_rel)
# rate_table_eth_plots[18,]
# rate_table_eth_plots$plots[[34]]
# THERE WILL BE A REASONABLE DIFFRENCE IN STATS BETWEEN VERSION 1 AND 2 
# GIVEN THE APPROXIMATIONS MADE IN ETHNIC POPS IN VERSION 1




# rate_table_eth %>% 
#   filter(is.na(segment)) %>% 
#   # filter(segment == "55-74") %>% 
#   # filter(segment == "75+") %>% 
#   # rate_results %>% 
#   #   # AN EASY BUT INEFFICIENT WAY TO GET ADMISSIONS:
#   #   left_join(tb_rate %>% select(-c(pop_local, starts_with("index"))), by = c("icd_chap", "icd_chap_name")) %>% 
#   ggplot(aes(index_relative, index_abs))+
#   geom_point(aes(size = adms), alpha = .4)+
#   geom_text_repel(
#     aes(label = str_trunc(icd_chap_name, 25)),
#     col = "grey50"
#   )+
#   scale_x_continuous(
#     limits = c(0, .3),
#     labels = str_c(c(0, 10, 20, 30), "%")
#   )+
#   # ylim(0, 5000)+
#   scale_y_continuous(limits = c(0, 4800), labels = scales::comma)+
#   # DO AXIS TITLES IN PWPT:
#   theme(axis.title = element_blank(),
#         legend.position=c(.95,.85),
#         legend.title = element_blank())+
#   ggsave(
#     dpi = 600,
#     filename = str_c("gph_disparity_bc_eth_all.png"),
#     width = 26.04,
#     height = 15.37,
#     units = "cm"
#   )+
#   NULL


# Plot std rates------------------------------------------------------------

# # SOLVED: PROBLEM - OTHER CATEGORY APPEARS TO BE OVERUSED IN HES TABLES
# # ONLY BECAUSE IT CONTAINS "NOT STATED" 72%
# # IF WE RELOCATE THESE TO "NOT KNOWN" (AND REMOVE) WE MAY GET MORE ACCURATE STATS
# THESE CALCS WERE DONE ON THE ORIGINAL TABLE - BEFORE CHANGES
# pop_eth_bc %>% count(ethnicity, wt = pop_local) %>% mutate(p = n/sum(n))
# ip %>% count(ethnicity) %>% mutate(p = n/sum(n))
# ip %>% filter(ethnicity == "other") %>% count(ethnic_category_description) %>%  mutate(p = n/sum(n))
# ip %>% count(ethnos, ethnicity)
#   # filter(ethnic_category_description != "Not stated") %>% 
#   mutate(ethnicity = ifelse(ethnic_category_description == "Not stated", NA, ethnicity)) %>% 
#   # filter(ethnicity == "other") %>% count(ethnic_category_description) %>%  mutate(p = n/sum(n))
#   count(ethnicity) %>% mutate(p = n/sum(n))
# pop_eth_bc %>% count(ethnicity, wt = pop_local) %>% mutate(p = n/sum(n))

# rate_703 %>% 
#   # filter(is.na(segment)) %>%
#   # filter(segment == "55-74") %>%
#   # filter(segment == "75+") %>%
#   mutate(std_rate = std_rate * 1000) %>% 
#   group_by(ics, place, segment) %>%
#   nest %>% 
#   ungroup() %>% 
#   slice(1) %>% 
#   unnest(cols = data) %>% 
#   ggplot(aes(ethnicity, std_rate))+
#   geom_col()+
#   facet_wrap(vars(icd_chap))
# 
# em$data[[1]]
# 
# tmp_90 <- em_swarks_rates1$calcs[[2]] %>% 
#   mutate(across(c(n, pop_sw, rate_sw), ~ifelse(is.na(.), 0, .)))
# # em_swarks_rates2$calcs_deux[[1]]
# 
# enframe(
# epitools::ageadjust.direct(count = tmp_90$n,
#                            pop = tmp_90$pop_sw,
#                            stdpop = pop_national %>% filter(imd_quint == 1) %>% pull(pop_nat)
#                            )
# ) %>% 
#   pivot_wider(names_from = name, values_from = value)
# 
# 
# em_swarks_rates2$calcs_deux[[2]]
# rate_701$calc_crude[[1]]

std_conf_eth <-
  rate_701 %>% 
  # FOR SEGMENT 'ALL':
  # filter(is.na(segment)) %>% 
  # mutate(icd_chap_name = str_replace(icd_chap_name, "\\d{1}. ", "")) %>% 
  # mutate(icd_chap_name = str_replace(icd_chap_name, "^\\d", "")) %>% 
  mutate(calc_crude = map(calc_crude, function(df) {
    df %>% 
      mutate(across(c(n, pop_local, rate_local), ~ifelse(is.na(.), 0, .)))
  }
  )) %>% 
  mutate(tmp_std = map(calc_crude, function(df) {
    
    with(df, 
         enframe(
           epitools::ageadjust.direct(count = n,
                                      pop = pop_local,
                                      # CARE: THIS NEEDS TO BE ORDERED IN SAME WAY AS AGE, SEX IN CALCS:
                                      stdpop = euro_2013_85_max %>% pull(pop_euro)
           )
         ) %>% 
           pivot_wider(names_from = name, values_from = value)
    )
    
  }))


std_conf_eth <- std_conf_eth %>% 
  mutate(icd_chap_name = as_factor(icd_chap_name)) 

plots_std_eth <- 
  std_conf_eth %>% 
  # ungroup() %>% 
  # select(3:5, tmp_std) %>%
  unnest(tmp_std) %>% 
  # mutate(adj.rate = adj.rate * 1000) %>% 
  mutate(across(c(adj.rate, lci, uci), ~ . * 1000)) %>%
  group_by(ics, place, segment) %>%
  nest %>% 
  ungroup() %>% 
  mutate(plots = map(data, function(df) {
    df %>% 
      ggplot(aes(ethnicity, adj.rate))+
      geom_col(alpha = .6)+
      geom_errorbar(aes(ymin = lci, ymax = uci), width = .25, alpha = .6 )+
      # coord_cartesian(ylim = c(0, 25))+
      # facet_wrap(vars(str_c(icd_chap, str_trunc(icd_chap_name, 25))))+
      facet_wrap(vars(icd_chap_name))+
      theme(axis.title = element_blank(), strip.text = element_text(size = 7))+
      # ggsave(
      #   dpi = 600,
      #   filename = str_c("gph_stdrate_bc_imd_all.png"),
      #   width = 26.04,
      #   height = 11.18,
      #   units = "cm"
      # )+
      NULL
  }))

# plots_std_imd %>% print(n=38)
# plots_std_eth$plots[[1]]


# std_conf_eth %>% 
#   ungroup() %>% 
#   select(3:5, tmp_std) %>%
#   unnest(tmp_std) %>% 
#   # mutate(adj.rate = adj.rate * 1000) %>% 
#   mutate(across(c(adj.rate, lci, uci), ~ . * 1000)) %>%
#   ggplot(aes(ethnicity, adj.rate))+
#   geom_col(alpha = .6)+
#   geom_errorbar(aes(ymin = lci, ymax = uci), width = .25, alpha = .6 )+
#   # coord_cartesian(ylim = c(0, 25))+
#   # facet_wrap(vars(str_c(icd_chap, str_trunc(icd_chap_name, 25))))+
#   facet_wrap(vars(icd_chap_name))+
#   theme(axis.title = element_blank(), strip.text = element_text(size = 7))+
#   ggsave(
#     dpi = 600,
#     filename = str_c("gph_stdrate_bc_eth_all.png"),
#     width = 26.04,
#     height = 11.18,
#     units = "cm"
#   )+
#   NULL

