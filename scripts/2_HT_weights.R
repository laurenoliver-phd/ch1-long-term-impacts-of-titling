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


## Calculating Horvitz-Thompson weights for the PFR villages
# - HT estimator weights all outcome variables by the inverse probability of selection, which varies by commune
# - Calculate by determining # of treated villages selected from each commune divided by # of villages who were determined to be eligible
# --> NB: preselected == 1 for eligible villages


### Loading Data

pre_sel <- read_dta("data/raw_data/WB_PFR_Baseline_2011/BEN_2011_PFRIE-BL_v01_M_Stata8/PRESELECTION.dta")
adm4_codes <- read_excel("data/raw_data/adm4_codes.xlsx") %>%
  mutate(village_key = str_squish(str_to_upper(village))) %>%   # normalize
  distinct(village_key, .keep_all = TRUE)                       # one row per village name

### Determining selection probability
eligible <- pre_sel %>%
  filter(preselected == 1) %>%
  select(codevil, treated) %>%
  left_join(adm4_codes, by = "codevil")

# NTS: Eligible list is just the same as treatment/control list, so can use the already joined pfr_ehcvm list to do the counts

ht_probs <- eligible %>%
  group_by(iddept,idcomm) %>%
  summarise(
    n_eligible = n_distinct(codevil),
    n_treated  = sum(treated == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    selection_prob = n_treated / n_eligible,
    ht_weight = 1 / selection_prob)


save(ht_probs, file = "data/processed/2_ht_weights.RData")

