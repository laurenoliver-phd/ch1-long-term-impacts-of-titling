library(dplyr)
library(readxl)
library(readr)
library(haven)
library(tidyr)
library(stringr)

## Goals: 
# 1. Add village names to all the EHCVM villages using the standard ADM4 codes
# 2. Figure out which treatment/control villages from PFR and ProPFR are also in the EHCVM data

### Step 1: Add village names to EHCVM data

# Read in EHCVM data

ehcvm2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s00_me_ben2021.dta")

ehcvm2021_groups <- ehcvm2021 %>%
  group_by(grappe) %>%
  slice(1) %>%    # or slice_head(n = 1)
  ungroup() %>%
  select(1:10) %>%
  mutate(village_key = str_squish(str_to_upper(s00q05)))

# Read in ADM4 codes with village names
adm4_codes <- read_excel("data/raw_data/adm4_codes.xlsx") %>%
  mutate(village_key = str_squish(str_to_upper(village))) %>%   # normalize
  distinct(village_key, .keep_all = TRUE)                       # one row per village name

## 2. Join: keep all EHCMV groups rows, attach ADM4 variables
ehcvm_groups <- ehcvm2021_groups %>%
  left_join(adm4_codes, by = "village_key")

# # Export the joined data
# 
# write_xlsx(
#   ehcvm_adm4_joined,
#   path = "ehcvm_adm4_joined_for_manual_fixes.xlsx"
# )

######################

# Step 2: Figure out which treatment/control villages from PFR and ProPFR are also in the EHCVM data

ehcvm_adm4_final <- read_excel("data/processed/1_ehcvm_adm4_final.xlsx")

PFR_raw <- read_dta("data/raw_data/WB_PFR_Baseline_2011/BEN_2011_PFRIE-BL_v01_M_Stata8/PRESELECTION.dta")

PFR_all <- PFR_raw %>%
  filter(treated == 1 | control == 1) %>%
  mutate(wb_survey = ifelse(edsb1 > 1, 1, 0)) %>%
  select(codevil, depart, commune, arrond, treated, emicov2006, emicov2010, wb_survey)

  
pfr_ehcvm <- PFR_all %>%
  inner_join(ehcvm_adm4_final %>% select(grappe, menage, vague, PanelHH,type, codevil, 20), by = "codevil") %>%
  rename(village = 14)

nrow(pfr_ehcvm %>%filter(wb_survey == 1)) # 48 villages in Goldstein & EHCVM

save(pfr_ehcvm, file = "data/processed/1_pfr_ehcvm.RData")

