
lkp_icd10 <- read_rds("lkp_icd10.rds")

lkp_icd10 <- lkp_icd10 %>% 
  mutate(icd_chap_name = case_when(
    icd_chap == 1 ~  "1. Infectious and parastic diseases",
    icd_chap == 2 ~  "2. Neoplasms",
    icd_chap == 3 ~  "3. Blood and immune system",
    icd_chap == 4 ~  "4. Endocrine, and metabolic diseases",
    icd_chap == 5 ~  "5. Mental and behavioural disorders",
    icd_chap == 6 ~  "6. Nervous system",
    icd_chap == 7 ~  "7. Eye and adnexa",
    icd_chap == 8 ~  "8. Ear and mastoid process",
    icd_chap == 9 ~  "9. Circulatory system",
    icd_chap == 10 ~ "10. Respiratory system",
    icd_chap == 11 ~ "11. Digestive system",
    icd_chap == 12 ~ "12. Skin and subcutaneous tissue",
    icd_chap == 13 ~ "13. Musculoskeletal system",
    icd_chap == 14 ~ "14. Genitourinary system",
    icd_chap == 15 ~ "15. Pregnancy, childbirth, the puerperium",
    icd_chap == 16 ~ "16. Conditions orig. in perinatal period",
    icd_chap == 17 ~ "17. Congenital diseases",
    icd_chap == 18 ~ "18. Signs and symptoms NEC",
    icd_chap == 19 ~ "19. Consequences of external causes",
    icd_chap == 20 ~ "20. External causes of morbidity",
    icd_chap == 21 ~ "21. Factors influencing health status",
    T ~ NA_character_
  ))


lkp_icd10 <- lkp_icd10 %>% 
  mutate(icd_chap_short = case_when(
    icd_chap == 1 ~  "1. Inf",
    icd_chap == 2 ~  "2. Canc",
    icd_chap == 3 ~  "3. Bld",
    icd_chap == 4 ~  "4. Endo",
    icd_chap == 5 ~  "5. Ment",
    icd_chap == 6 ~  "6. Nerv",
    icd_chap == 7 ~  "7. Eye",
    icd_chap == 8 ~  "8. Ear",
    icd_chap == 9 ~  "9. Circ",
    icd_chap == 10 ~ "10. Resp",
    icd_chap == 11 ~ "11. Dig",
    icd_chap == 12 ~ "12. Skin",
    icd_chap == 13 ~ "13. MSK",
    icd_chap == 14 ~ "14. GU",
    icd_chap == 15 ~ "15. Preg",
    icd_chap == 16 ~ "16. Peri",
    icd_chap == 17 ~ "17. Cong",
    icd_chap == 18 ~ "18. NEC",
    icd_chap == 19 ~ "19. Inj",
    icd_chap == 20 ~ "20. Ext",
    icd_chap == 21 ~ "21. Fact",
    T ~ NA_character_
  ))

