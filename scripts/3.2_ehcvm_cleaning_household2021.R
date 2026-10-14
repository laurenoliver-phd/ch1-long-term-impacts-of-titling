library(dplyr)
library(tidyr)
library(ggtext)
library(fixest)
library(vtable)
library(modelsummary)
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


## Reading in Data (2018, 2021)
s01_me_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s01_me_ben2021.dta")
ehcvm_welfare_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/ehcvm_welfare_ben2021.dta")


# Section 1: Household ID data (Individuals)
hh_id21 <- s01_me_ben2021 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, membres__id, s01q01, s01q02, s01q03c) %>%
  rename(gender = s01q01,
         hh_status = s01q02,
         year_birth = s01q03c) %>%
  mutate(age = 2022 - year_birth, hh_head = ifelse(hh_status == 1, 1, 0)) 

fem_head <- hh_id21 %>%
  filter(hh_head == 1) %>%
  mutate(fem_head = ifelse(gender == 2, 1, 0))

# HH ID Composite

hh_comp <- hh_id21 %>%
  group_by(grappe, menage) %>%
  summarise(
    num_men   = sum(gender == 1& age >= 15, na.rm = TRUE),
    num_women = sum(gender == 2 & age >= 15, na.rm = TRUE)
  )  %>%
  ungroup()

# Welfare
welfare21 <- ehcvm_welfare_ben2021 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, vague, hage, heduc, hhsize, hreligion, hmstat, dali, dnal, dtot)

# Composite Data on HH Head

hh_data2021 <- welfare21 %>%
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


## Part 2: Governance Outcomes ##

s20b_1_me_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s20b_1_me_ben2021.dta")

# Section 1: Household ID data (Individuals)
gov21 <- s20b_1_me_ben2021 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage,
         s20bq03, #Satisfaction with how democracy functions in the country 
         s20bq04d, # Desirability of democratic political system for the country 
         s20bq06a, # Women and men have the same opportunities in politics. 
         s20bq06b, # Should women and men have the same opportunities in politics?
         s20bq07b, # Local elected officials do their best to listen to you
         s20bq07c, # Traditional leaders do their best to listen to you
         s20bq08__3, # Local officials consult the traditional leaders?
         s20bq09, # Local authorities have power 
         s20bq11, # Corruption a problem for the country 
         s20bq13h, # Municipal authorities are involved in corruption
         s20bq13i, # Religious authorities are involved in corruption
         s20bq13j) %>% # Traditional leaders/chiefs are involved in corruption, 
  rename(
    dem_sat     = s20bq03, # Satisfaction with how democracy functions in the country (1 - Not good at all, 4 - Very good)
    dem_pref    = s20bq04d, # Desirability of democratic political system for the country (1 - Not good at all, 4 - Very good)
    fem_eq1     = s20bq06a, # Women and men have the same opportunities in politics (Yes/No)
    fem_eq2     = s20bq06b, # Should women and men have the same opportunities in politics? (Yes/No)
    leader_elec = s20bq07b, # Local elected officials do their best to listen to you (1 - Never, 4 - Always)
    leader_trad = s20bq07c, # Traditional leaders do their best to listen to you (1 - Never, 4 - Always)
    consult     = s20bq08__3, # Local officials consult the traditional leaders? (Yes/No)
    power_loc   = s20bq09, # Local authorities have power (1 - Alot, 4 - Not at all)
    corr_cntry  = s20bq11, # Corruption is a problem for the country (1 - Alot, 4 - Not at all)
    corr_loc    = s20bq13h, # Municipal authorities are involved in corruption (1 - Alot, 4 - Not at all)
    corr_rel    = s20bq13i, # Religious authorities are involved in corruption (1 - Alot, 4 - Not at all)
    corr_trad   = s20bq13j  # Traditional leaders/chiefs are involved in corruption (1 - Alot, 4 - Not at all)
  ) %>%
  mutate( #Flipping so that 4 is most positive outcome, 1 is least positive
    dem_sat  = 5 - dem_sat,
    dem_pref = 5 - dem_pref, 
    across(c(fem_eq1, fem_eq2, consult),
           ~ case_when(
             .x == 1 ~ 1,
             .x == 2 ~ 0,
             TRUE ~ NA_real_
           )
    ))

hh_data2021 <- hh_data2021 %>%
  left_join(gov21, by = c("grappe", "menage"))

save(hh_data2021, file = "data/processed/3.2_household_2021.RData")

