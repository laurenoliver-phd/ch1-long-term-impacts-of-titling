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

setwd("~/YSE Dropbox/Lauren Oliver/Benin_land_titles/Lauren/LongTermEffects")

## Reading in village data

load("~/chapter-1/data/processed/1_pfr_ehcvm.RData")
load("~/chapter-1/data/processed/2_ht_weights.RData")
adm4_codes <- read_excel("data/raw_data/adm4_codes.xlsx") %>%
  select(codevil, iddept, idcomm, idarro, idvill)

pfr_ehcvm <- pfr_ehcvm %>%
  left_join(adm4_codes, by = "codevil") %>%
  select(grappe, vague, treated, iddept, idcomm, depart, commune, arrond, village, codevil, type, PanelHH)%>%
  mutate(across(c(depart, commune, arrond, village), str_to_title))

pfr_ehcvm_sample <- pfr_ehcvm


## Reading in Data (2018, 2021)
s01_me_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s01_me_ben2021.dta")
s16a_me_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s16a_me_ben2021.dta")
s16c_me_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s16c_me_ben2021.dta")
s16d_me_ben2021 <- read_dta("data/raw_data/EHCVM_2021_STATA/s16d_me_ben2021.dta")

ehcvm_welfare_ben2021 <- read_dta("data/00_manually-downloaded/EHCVM_2021_STATA/ehcvm_welfare_ben2021.dta")

crop_info <- read_excel("documents/EHCVM_Replication.xlsx", sheet = "Crops")[1:17,] %>%
  rename(crop = Food,
         price_usd_per_tonne = Final_Value)


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
  select(grappe, menage, vague, hage, heduc, hhsize, hreligion, hmstat)

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
  select(grappe, menage, vague, hage, gender, educ_years,
         religion_head, married, hhsize, num_men, num_women, num_children,fem_head)


# Agriculture data

agri21 <- s16a_me_ben2021 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, s16aq02, s16aq03, s16aq15, s16aq14a__0, s16aq14b__0, s16aq10, s16aq09a, s16aq09b, s16aq00,
         s16aq33b_1,s16aq35b_1,s16aq37b_1,
         s16aq39b_1, s16aq39b_2, s16aq39b_3, s16aq39b_4, s16aq41b_1, s16aq41b_2, s16aq41b_3,
         s16aq41b_4, s16aq43b_1, s16aq43b_2, s16aq43b_3, s16aq43b_4, s16aq22, s16aq25, s16aq27, s16aq44) %>%
  rename(farm_no = s16aq02,
         plot_no = s16aq03,
         tillage = s16aq44) %>%
  mutate(fear_conflict = case_when(
    s16aq15 == 1 ~ 1,
    s16aq15 == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(can_sell = if_else(is.na(s16aq14a__0), 0, 1),
         can_beq = if_else(is.na(s16aq14b__0), 0, 1),
         owns_land = if_else(s16aq10 == 1, 1, 0),
         rents_land = case_when(
           s16aq10 %in% c(2, 3,4) ~ 1,
           s16aq10 %in% c(1, 5,  6, 7,8) ~ 0,
           TRUE ~ NA_real_
         ),
         plot_size_ha = case_when(
           s16aq09b == 1 ~ s16aq09a,          # already hectares
           s16aq09b == 2 ~ s16aq09a / 10000,  # m² to hectares
           TRUE ~ NA_real_
         ),
         labor_days = case_when(
           if_all(c(s16aq33b_1, s16aq35b_1, s16aq37b_1), is.na) ~ NA_real_,
           TRUE ~ rowSums(cbind(s16aq33b_1, s16aq35b_1, s16aq37b_1), na.rm = TRUE)
         ),
         fert_dummy = case_when(
           s16aq22 == 1 |
             s16aq25 == 1 |
             s16aq27 == 1  ~ 1,
           TRUE ~ 0
         )) %>%
  select(grappe, menage, farm_no, plot_no, fear_conflict, can_sell, can_beq, owns_land, rents_land, fert_dummy, plot_size_ha,
         labor_days)



# Crop Data - Planting 
crop21 <- s16c_me_ben2021 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, s16cq02, s16cq03, s16cq08, s16cq04,s16cq09, s16cq16c, s16cq07) %>%
  rename(farm_no = s16cq02,
         plot_no = s16cq03,
         crop_pct = s16cq08,
         crop_code = s16cq04,
         seed_type = s16cq09,
         output_kg = s16cq16c) %>%
  mutate(
    crop_group = case_when(
      crop_code %in% c(1,2,3,4) ~ 1,           # Cereals
      crop_code %in% c(8,10,64) ~ 2,           # Pulses
      crop_code %in% c(11,24,29) ~ 3,          # Vegetables
      is.na(crop_code) ~ NA_real_,             # Preserve missing
      TRUE ~ 0                                 # Other
    ),
    crop_group = factor(
      crop_group,
      levels = c(0,1,2,3),
      labels = c("Other","Cereals","Pulses","Vegetables")
    ), 
    annual_cash_crop = case_when(
      crop_code %in% c(13, 43, 56) ~ 1,
      is.na(crop_code) ~ NA_real_,
      TRUE ~ 0
    ),
    per_cash_crop = case_when(
      crop_code %in% c(51, 55) ~ 1,
      is.na(crop_code) ~ NA_real_,
      TRUE ~ 0
    ),
    crop = factor(
      crop_code,
      levels = crop_info$crop_code,
      labels = crop_info$crop
    ),
    method = factor(
      s16cq07,
      levels = c(1, 2),
      labels = c("Mono", "Inter")
    )) %>%
  left_join(crop_info %>% select(crop_code, price_usd_per_tonne), by = "crop_code")


### Join Crops & Ag dfs

ag_crop2021 <- agri21 %>%
  left_join(crop21, by = c("grappe", "menage", "farm_no", "plot_no")) %>%
  select(grappe, menage, farm_no, plot_no, crop_code,crop, method, crop_pct, plot_size_ha,
         fear_conflict, can_sell, can_beq, owns_land, rents_land, labor_days, seed_type, fert_dummy, 
         crop_group, annual_cash_crop, per_cash_crop) %>%
  mutate(crop_frac = if_else(is.na(crop_pct), 1, crop_pct/100),
         plot_crop_area_ha = plot_size_ha * crop_frac)


# Crop Data -- Production

# function to convert to kg
to_kg <- function(qty, unit){
  case_when(
    unit == 10 ~ qty,          # kg
    unit == 11 ~ qty * 1000,   # Tonne, 1000 kg
    unit == 16 ~ qty * 2.5,   # Tongolo, ~2.5 kg
    unit == 22 ~ qty * 25,     # Bassine Grande, ~25kg
    unit == 37 ~ qty * 25,     # bag 25kg
    unit == 38 ~ qty * 50,     # bag 50kg
    unit == 39 ~ qty * 100,    # bag 100kg
    TRUE ~ NA_real_
  )
}


prod21_raw <- s16d_me_ben2021 %>%
  filter(grappe %in% pfr_ehcvm_sample$grappe) %>%
  select(grappe, menage, s16dq01, s16dq02a, s16dq02b, s16dq03a, s16dq03b,
         s16dq05a, s16dq05b, s16dq05c, s16dq13a, s16dq13b) %>%
  rename(crop_code = s16dq01,
         cons_qty = s16dq02a,
         cons_unit = s16dq02b,
         gift_qty = s16dq03a,
         gift_unit = s16dq03b,
         sale_qty = s16dq05a,
         sale_unit = s16dq05b,
         sale_kg_est = s16dq05c,
         stock_qty = s16dq13a,
         stock_unit = s16dq13b) %>%
  left_join(crop_info %>% select(crop_code, crop, price_usd_per_tonne), by = "crop_code") %>%
  mutate(
    cons_kg  = to_kg(cons_qty, cons_unit),
    gift_kg  = to_kg(gift_qty, gift_unit),
    sale_kg  = to_kg(sale_qty, sale_unit),
    stock_kg = to_kg(stock_qty, stock_unit)
  ) %>%
  mutate(
    total_kg = case_when(
      if_all(c(cons_kg, gift_kg, sale_kg, stock_kg), is.na) ~ NA_real_,
      TRUE ~ rowSums(cbind(cons_kg, gift_kg, sale_kg, stock_kg), na.rm = TRUE)
    ), price_kg = price_usd_per_tonne / 1000,
    total_usd = total_kg * price_kg
  )

prod21 <- prod21_raw %>%
  select(grappe, menage,crop_code, crop, cons_kg, gift_kg, sale_kg, stock_kg, total_kg, price_kg, total_usd, price_usd_per_tonne) 

# Joining Ag & Crop data with Production data

plot2021<- ag_crop2021 %>%
  right_join(prod21, by = c("grappe", "menage", "crop_code", "crop")) %>%
  distinct(grappe, menage, farm_no, plot_no, crop_code, .keep_all = TRUE)

# Fixing totals so that it's total per plot for HHs that grow the same crop on multiple plots

plot2021 <- plot2021 %>%
  group_by(grappe, menage, crop_code) %>%
  mutate(
    total_area_crop = sum(plot_crop_area_ha, na.rm = TRUE),
    yield_kg_ha = if_else(total_area_crop > 0, total_kg / total_area_crop, NA_real_),
    plot_output_kg = yield_kg_ha * plot_size_ha,
    plot_output_usd = plot_output_kg * price_usd_per_tonne/1000
  ) %>%
  ungroup() %>%
  mutate(input_dummy = case_when(
    fert_dummy == 1 | seed_type %in% c(2,3) ~ 1,
    TRUE ~ 0
  ))

## Farm data 

farm2021 <- plot2021 %>%
  group_by(grappe, menage, farm_no) %>%
  summarise(
    total_farm_area_ha = sum(plot_crop_area_ha, na.rm = TRUE),
    can_sell_farm = as.integer(any(can_sell == 1, na.rm = TRUE)),
    can_beq_farm = as.integer(any(can_beq == 1, na.rm = TRUE)),
    owns_land_farm = as.integer(any(owns_land == 1, na.rm = TRUE)),
    rents_land_farm = as.integer(any(rents_land == 1, na.rm = TRUE)),
    labor_days_farm = sum(labor_days, na.rm = TRUE),
    # imp_seeds = as.integer(any(seed_type %in% c(2,3), na.rm = TRUE)),
    # fertilizer = as.integer(any(fert_dummy == 1, na.rm = TRUE)),
    input_dummy_farm = as.integer(any(input_dummy == 1, na.rm = TRUE)),
    annual_cash_crop = as.integer(any(annual_cash_crop == 1, na.rm = TRUE)),
    per_cash_crop = as.integer(any(per_cash_crop == 1, na.rm = TRUE)),
    cereals = as.integer(any(crop_group == "Cereals", na.rm = TRUE)),
    pulses = as.integer(any(crop_group == "Pulses", na.rm = TRUE)),
    vegetables = as.integer(any(crop_group == "Vegetables", na.rm = TRUE)),
    other_crops = as.integer(any(crop_group == "Other", na.rm = TRUE)),
    farm_yield_kg = sum(plot_output_kg, na.rm = TRUE),
    farm_yield_usd = sum(plot_output_usd, na.rm = TRUE),
    farm_yield_kg_ha = if_else(
      total_farm_area_ha > 0,
      farm_yield_kg / total_farm_area_ha,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(across(c(farm_yield_kg, farm_yield_usd, farm_yield_kg_ha),
                ~ na_if(., 0)))

## Adding treatment status to hh data

agg_hh_data2021 <- hh_data2021 %>%
  left_join(pfr_ehcvm_sample %>% select(grappe, treated, idcomm, iddept, depart, commune, arrond, village, codevil, PanelHH), by = "grappe") %>%
  select(grappe, menage, vague, treated, iddept, idcomm, depart, commune, arrond, village, codevil, PanelHH, fem_head, 
         hage, educ_years, religion_head, married, hhsize, num_men, num_women, num_children)

## Add in HT weights to hh data

agg_hh_data2021 <- agg_hh_data2021 %>%
  left_join(ht_probs %>% select(iddept, idcomm, selection_prob, ht_weight), by = c("iddept", "idcomm"))

## Adding hh data to plot and farm data

agg_plot2021 <- plot2021 %>%
  left_join(agg_hh_data2021, by = c("grappe", "menage")) %>%
  select(grappe, menage, vague, treated, depart, commune, arrond, village, ht_weight, PanelHH, fem_head, 
         hage, educ_years, religion_head, married, hhsize, num_men, num_women, num_children,
         farm_no, plot_no, plot_size_ha, can_sell, can_beq, owns_land, rents_land, fear_conflict,
         crop_code, crop, method, labor_days, input_dummy, crop_group, annual_cash_crop, per_cash_crop,
         plot_output_kg, plot_output_usd)

agg_farm2021 <- farm2021 %>%
  left_join(agg_hh_data2021, by = c("grappe", "menage")) %>%
  select(grappe, menage, vague, treated, depart, commune, arrond, village, ht_weight, PanelHH, fem_head, 
         hage, educ_years, religion_head, married, hhsize, num_men, num_women, num_children, everything())





# Controls (by HH)
# - Gender
# - Age
# - Years Education
# - Number of adult male hh members
# - Number of adult female hh members
# - Number of children hh members
# - Religion of HH head
# - Marital status of HH head

# Treatment Effects to Investigate (by plot and by crop)
# - Land conflict in the last year
# - HH head can pledge land
# - HH head can sell land
# - HH farms on rented land (hh rents in)
# - Total land size (ha)
# - Share of land size cultivated
# - Type of crop cultivated: Cereals* -- Millet, Sorghum, Rice, Maize
# - Type of crop cultivated: Pulses* -- Black-eyed Pea, Peanut, Soy
# - Type of crop cultivated: Roots and Tubers* - Manioc, Yams 
# - Type of crop cultivated: Vegetables* - Okra, Pepper, Tomato
# - Cash Crops - Annual* - Cotton, Pineapple, Sesame
# - Cash Crops - Perennial* - Cashew, Oil Palm
# - Farm labor supply (person-days/ha)
# - Fertilizer/high-yield seeds*
# - Total output (Log USD)
# - Yield (Log USD of total output/ha)
# *dummy variable



####

# save as rdata for use in analysis script
save(agg_plot2021, file = "data/03_clean/plot2021.RData")
save(agg_hh_data2021, file = "data/03_clean/hh_data2021.RData")
save(agg_farm2021, file = "data/03_clean/farm2021.RData")



