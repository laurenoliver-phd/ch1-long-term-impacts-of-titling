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

# Loading HH Data, Plot Data
load("data/processed/3.1_household_2018.RData")
hh_vars <- names(hh_data2018)
groups <- unique(hh_data2018$grappe)


s16a_me_ben2018 <- read_dta("data/raw_data/EHCVM_2018_STATA/s16a_me_ben2018.dta")
s16c_me_ben2018 <- read_dta("data/raw_data/EHCVM_2018_STATA/s16c_me_ben2018.dta")

# Loading Codebook Data
crop_info <-read_excel("docs/EHCVM_Codebook_Final.xlsx", sheet = "Crop Codes, Groups, Prices")[1:17,] %>% 
  rename(price_usd_per_tonne = Final_Value)

# Agriculture data

agri18 <- s16a_me_ben2018 %>%
  filter(grappe %in% groups) %>%
  select(grappe, menage, s16aq02,
         s16aq03, s16aq15, s16aq10,
         s16aq09a, s16aq09b, s16Aq00,
         s16aq33b_1,s16aq35b_1, s16aq37b_1,
         s16aq22, s16aq25, s16aq27) %>%
  rename(farm_no = s16aq02,
         plot_no = s16aq03,
         plot_size = s16aq09a,
         plot_size_unit = s16aq09b) %>%
  mutate(fear_conflict = case_when(
    s16aq15 == 1 ~ 1,
    s16aq15 == 2 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(
    owns_land = if_else(s16aq10 == 1, 1, 0),
    rents_land = case_when(
      s16aq10 %in% c(2, 3,4) ~ 1,
      s16aq10 %in% c(1, 5,  6, 7,8) ~ 0,
      TRUE ~ NA_real_
    ),
    plot_size_ha = case_when(
      plot_size_unit == 1 ~ plot_size,          # already hectares
      plot_size_unit == 2 ~ plot_size / 10000,  # m² to hectares
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
  select(grappe, menage, farm_no, plot_no,
         fear_conflict, owns_land, rents_land,
         fert_dummy, 
         plot_size, plot_size_unit,
         plot_size_ha, labor_days)

##### PLOT PRODUCTION DATA ####

to_kg <- function(qty, unit) {
  case_when(
    unit %in% c(19, 100) ~ qty,        # kg
    unit %in% c(25, 128) ~ qty,        # panier / basket
    unit == 134 ~ qty * 10,            # 10kg bag
    unit %in% c(5, 105, 34, 136) ~ qty * 25,
    unit %in% c(36, 138) ~ qty * 50,
    unit %in% c(33, 133, 135) ~ qty * 100,
    TRUE ~ NA_real_
  )
}

any_one <- function(x) as.integer(any(x == 1, na.rm = TRUE))


plot_prod18 <- s16c_me_ben2018 %>%
  filter(grappe %in% groups) %>%
  select(
    grappe, menage,
    farm_no   = s16cq02,
    plot_no   = s16cq03,
    crop_code = s16cq04,
    method    = s16cq07,
    crop_pct  = s16cq08,
    seed_type = s16cq09,
    
    cons_qty   = s16cq13a,
    cons_unit  = s16cq13b,
    gift_qty   = s16cq14a,
    gift_unit  = s16cq14b,
    sale_qty   = s16cq16a,
    sale_unit  = s16cq16b,
    stock_qty  = s16cq22a,
    stock_unit = s16cq22b
  ) %>%
  left_join(
    crop_info %>%
      select(crop_code, crop, price_usd_per_tonne),
    by = "crop_code"
  ) %>%
  mutate(
    crop_group = case_when(
      crop_code %in% c(1, 2, 3, 4) ~ "Cereals",
      crop_code %in% c(8, 10, 64) ~ "Pulses",
      crop_code %in% c(11, 24, 29) ~ "Vegetables",
      is.na(crop_code) ~ NA_character_,
      TRUE ~ "Other"
    ),
    crop_group = factor(
      crop_group,
      levels = c("Other", "Cereals", "Pulses", "Vegetables")
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
    
    method = factor(
      method,
      levels = c(1, 2),
      labels = c("Mono", "Inter")
    ),
    
    cons_kg  = to_kg(cons_qty, cons_unit),
    gift_kg  = to_kg(gift_qty, gift_unit),
    sale_kg  = to_kg(sale_qty, sale_unit),
    stock_kg = to_kg(stock_qty, stock_unit),
    
    total_kg = case_when(
      if_all(c(cons_kg, gift_kg, sale_kg, stock_kg), is.na) ~ NA_real_,
      TRUE ~ rowSums(across(c(cons_kg, gift_kg, sale_kg, stock_kg)), na.rm = TRUE)
    ),
    
    price_kg = price_usd_per_tonne / 1000,
    total_usd = total_kg * price_kg
  ) %>%
  right_join(
   agri18,
    by = c("grappe", "menage", "farm_no", "plot_no")
  ) %>%
  mutate(
    crop_frac = if_else(is.na(crop_pct), 1, crop_pct / 100),
    plot_crop_area_ha = plot_size_ha * crop_frac,
    input_dummy = as.integer(fert_dummy == 1 | seed_type %in% c(2, 3))
  ) 
# %>%
#   group_by(grappe, menage, crop_code) %>%
#   
#   mutate(
#     total_area_crop = sum(plot_crop_area_ha, na.rm = TRUE),
#     yield_kg_ha = if_else(total_area_crop > 0, total_kg / total_area_crop, NA_real_),
#     plot_output_kg = yield_kg_ha * plot_crop_area_ha,
#     total_usd = plot_output_kg * price_kg
#   ) %>%
#   
#   ungroup()

# -----------------------------
# Collapse to Parcel Level
# -----------------------------

plot2018 <- plot_prod18 %>%
  group_by(grappe, menage, farm_no, plot_no) %>%
  summarise(
    total_plot_area_ha = sum(plot_crop_area_ha, na.rm = TRUE),
    
    fear_conflict = any_one(fear_conflict),
    owns_land = any_one(owns_land),
    rents_land = any_one(rents_land),
    input_dummy = any_one(input_dummy),
    annual_cash_crop = any_one(annual_cash_crop),
    per_cash_crop = any_one(per_cash_crop),
    
    labor_days_plot = sum(labor_days, na.rm = TRUE),
    
    cereals = as.integer(any(crop_group == "Cereals", na.rm = TRUE)),
    pulses = as.integer(any(crop_group == "Pulses", na.rm = TRUE)),
    vegetables = as.integer(any(crop_group == "Vegetables", na.rm = TRUE)),
    other_crops = as.integer(any(crop_group == "Other", na.rm = TRUE)),
    
    plot_total_usd = sum(total_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    plot_yield_usd_ha = if_else(
      total_plot_area_ha > 0,
      plot_total_usd / total_plot_area_ha,
      NA_real_
    ),
    labor_rate_plot = if_else(
      total_plot_area_ha > 0,
      labor_days_plot / total_plot_area_ha,
      NA_real_
    )
  ) %>%
  left_join(hh_data2018, by = c("grappe", "menage")) %>%
  select(
    all_of(hh_vars),
    everything()
  ) %>%
  filter(plot_total_usd < max(plot_total_usd, na.rm = TRUE))



# save as rdata for use in analysis script
save(plot2018, file = "data/processed/3.3_plot_2018.RData")






