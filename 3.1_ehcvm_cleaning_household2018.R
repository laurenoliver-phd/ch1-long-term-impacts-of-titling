library(dplyr)
library(tidyr)
library(ggtext)
library(fixest)
library(broom)
library(stringr)
library(stringi)
library(xtable)
library(lubridate)
library(haven)
library(readxl)
library(ggplot2)

## Reading in village data

load("~/chapter-1/data/processed/1_pfr_ehcvm.RData")
load("~/chapter-1/data/processed/2_ht_weights.RData")
adm4_codes <- read_excel("data/raw_data/adm4_codes.xlsx") %>%
  select(codevil, iddept, idcomm, idarro, idvill)

pfr_ehcvm <- pfr_ehcvm %>%
  left_join(adm4_codes, by = "codevil") %>%
  select(grappe, vague, treated, iddept, idcomm, depart, commune, arrond, village, codevil, type, PanelHH)%>%
  mutate(across(c(depart, commune, arrond, village), str_to_title))

## EHCVM Household Data
s01_me_ben2018 <- read_dta("data/raw_data/EHCVM_2018_STATA/s01_me_ben2018.dta")
ehcvm_welfare_ben2018 <- read_dta("data/raw_data/EHCVM_2018_STATA/ehcvm_welfare_ben2018.dta")


# PART 1: Household ID (Individuals) # 

hh_id18 <- s01_me_ben2018 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, s01q00a, s01q01, s01q02, s01q03c) %>%
  rename(membres__id = s01q00a,
         gender = s01q01,
         hh_status = s01q02,
         year_birth = s01q03c) %>%
  mutate(age = 2022 - year_birth, hh_head = ifelse(hh_status == 1, 1, 0)) 

fem_head <- hh_id18 %>%
  filter(hh_head == 1) %>%
  mutate(fem_head = ifelse(gender == 2, 1, 0))

## Household Composite

hh_comp <- hh_id18 %>%
  group_by(grappe, menage) %>%
  summarise(
    num_men   = sum(gender == 1& age >= 15, na.rm = TRUE),
    num_women = sum(gender == 2 & age >= 15, na.rm = TRUE)
  )  %>%
  ungroup()

##### PART 2: Welfare (Household-Level) #####

welfare18 <- ehcvm_welfare_ben2018 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, vague, hage, heduc, hhsize, hreligion, hmstat, dali, dnal, dtot)

## Composite Data on HH Head

hh_data2018 <- welfare18 %>%
  left_join(hh_comp, by = c("grappe", "menage")) %>%
  left_join(fem_head, by = c("grappe", "menage")) %>%
  mutate(num_children = hhsize - num_women - num_men) %>%
  mutate(
    religion_head = case_when(
      hreligion %in% 1 ~ 1,   # Muslim
      hreligion %in% 2 ~ 2,   # Christian
      hreligion %in% c(3,4,5) ~ 0,  # Other
      TRUE ~ NA_real_
    ),
    religion_head = factor(
      religion_head,
      levels = c(0,1,2),
      labels = c("Other", "Muslim", "Christian")
    )
  ) %>%
  mutate(
    married = case_when(
      hmstat %in% c(2, 3) ~ 1,
      hmstat %in% c(1, 4, 5, 6, 7) ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(educ_years = case_when(
    heduc  == 1 ~ 0,   # Aucun
    heduc  == 2 ~ 1,   # Maternelle
    heduc  == 3 ~ 6,   # Primaire
    heduc  == 4 ~ 10,  # Secondaire general 1
    heduc == 5 ~ 10,  # Secondaire technique 1
    heduc == 6 ~ 13,  # Secondaire general 2
    heduc == 7 ~ 13,  # Secondaire technique 2
    heduc == 8 ~ 16,  # Postsecondaire
    heduc == 9 ~ 16,  # Superieur
    TRUE ~ NA_real_
  )) %>%
  mutate(
    educ_years = as.numeric(educ_years),
    muslim_head = if_else(religion_head == "Muslim", 1, 0, missing = NA_real_),
    christian_head = if_else(religion_head == "Christian", 1, 0, missing = NA_real_)
  )%>%
  left_join(pfr_ehcvm_sample %>% select(grappe, treated, idcomm, iddept, depart, commune, arrond, village, codevil, PanelHH), by = "grappe") %>%
  left_join(ht_probs %>% select(iddept, idcomm, selection_prob, ht_weight), by = c("iddept", "idcomm")) %>%
  select(grappe, menage, vague, treated, iddept, idcomm, depart, commune, arrond, village, selection_prob, ht_weight, codevil, PanelHH, fem_head, 
         hage, educ_years, religion_head, christian_head, muslim_head,  married, hhsize, num_men, num_women, num_children,
         dali, dnal, dtot) 


# save as rdata for use in analysis script
save(hh_data2018, file = "data/processed/3_household_2018.RData")
