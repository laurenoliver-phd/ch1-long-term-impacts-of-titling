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
library(readr)
library(writexl)

## Goals: 
# 1. Add village names to all the EHCVM villages using the standard ADM4 codes
# 2. Figure out which treatment/control villages from PFR and ProPFR are also in the EHCVM data

### Step 1: Add village names to EHCVM data

# Read in EHCVM data
ehcvm2021 <- read_csv("~/YSE Dropbox/Lauren Oliver/Benin_land_titles/Lauren/data/00_manually-downloaded/BEN_2021_EHCVM/ehcvm_villages_stata.csv")

ehcvm2021_groups <- ehcvm2021 %>%
  group_by(grappe) %>%
  slice(1) %>%    # or slice_head(n = 1)
  ungroup() %>%
  select(1:10) %>%
  mutate(village_key = str_squish(str_to_upper(s00q05)))

# Read in ADM4 codes with village names
adm4_codes <- read_excel("~/YSE Dropbox/Lauren Oliver/Benin_land_titles/Lauren/data/00_manually-downloaded/adm4_codes_fill.xlsx") %>%
  mutate(village_key = str_squish(str_to_upper(village))) %>%   # normalize
  distinct(village_key, .keep_all = TRUE)                       # one row per village name

## 2. Join: keep all EHCMV groups rows, attach ADM4 variables
ehcvm_groups <- ehcvm2021_groups %>%
  left_join(adm4_codes, by = "village_key")

# Export the joined data

write_xlsx(
  ehcvm_adm4_joined,
  path = "ehcvm_adm4_joined_for_manual_fixes.xlsx"
)

######################

# Step 2: Figure out which treatment/control villages from PFR and ProPFR are also in the EHCVM data

ehcvm_adm4_final <- read_excel("~/YSE Dropbox/Lauren Oliver/Benin_land_titles/Lauren/data/00_manually-downloaded/BEN_2021_EHCVM/EHCVM2021_CSVs/ehcvm_adm4_final.xlsx")
PFR_all <- read_csv("~/YSE Dropbox/Lauren Oliver/Benin_land_titles/Lauren/data/00_manually-downloaded/BEN_2021_EHCVM/EHCVM2021_CSVs/PFR_all.csv") %>%
  filter(treated == 1 | control == 1) %>%
  mutate(wb_survey = ifelse(edsb1 > 1, 1, 0)) %>%
  select(codevil, depart, commune, arrond, treated, emicov2006, emicov2010, wb_survey)

  
pfr_ehcvm <- PFR_all %>%
  inner_join(ehcvm_adm4_final %>% select(grappe, menage, vague, PanelHH,type, codevil, 20), by = "codevil") %>%
  rename(village = 14)

nrow(pfr_ehcvm %>%filter(wb_survey == 1)) # 48 villages in Goldstein & EHCVM

saveRDS(pfr_ehcvm, file = "pfr_ehcvm.rds")

