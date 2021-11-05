library(data.table)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(glue)
library(knitr)
source("functions/check_for_code.R")
list_of_drugs <- fread("data/UKB_f20003_antidep_psy_lithi.txt")

ukb_baseline <- "/nfs/AGE/UKB/data/180427/r/ukb21472.tab"
mhq_fp <- "/nfs/AGE/UKB/data/180524/r/ukb22140.tab"
updated_hes <- "/nfs/AGE/UKB/data/200725/r/ukb42733.tab"

cols_baseline <- fread(ukb_baseline, nrows= 0) %>% colnames() %>% tibble()
cols_mhq <- fread(mhq_fp, nrows= 0) %>% colnames() %>% tibble()
cols_updated_baseline <- fread(updated_hes, nrows= 0) %>% colnames() %>% tibble()


# UKB data was stored
df20003="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_20003_SELF_MED.tsv"
df20002="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_SELFREP_20002.tsv"
df20126="/proj/sens2017519/nobackup/b2016326_nobackup/private/arvhar/data/UKB_RAW_DATA/ukb_data/UKB_20126.tsv"
df2090_2010="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_2090_2010.tsv"
dfcidi="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_CIDI_SF.tsv"
df20544="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_SELFREP_20544.tsv"


# Get all the colnames for datafield 41270

cols_41270 <- cols_updated_baseline %>%  filter( . == "f.eid" | str_detect(., "f.41270")) %>% .$.
df41270 <- fread(updated_hes, select = cols_41270)

# Get all the colnames for datafield 20002
cols_20002 <- cols_baseline %>% filter(. == "f.eid" | str_detect(., "f.20002")) %>% .$.
df20002 <- fread(ukb_baseline, select = cols_20002)

# field 20126
cols_20126 <- cols_baseline %>% filter(. == "f.eid"| str_detect(.,"f.20126")) %>% .$.
df20126 <- fread(ukb_baseline, select = cols_20126)

# df2090 and 2090
cols2010_2090 <- cols_baseline %>% filter(. == "f.eid" | str_detect(., "f.2090") | . %in% c("f.2010.0.0", "f.2010.1.0", "f.2010.2.0")) %>% .$.
df2090_2010 <- fread(ukb_baseline, select = cols2010_2090)

# field 20003
col20003 <- cols_baseline %>% filter(. == "f.eid" | str_detect(., "f.20003")) %>% .$.
df20003 <- fread(ukb_baseline, select = col20003)

# field 20544
col20544 <- cols_mhq %>% filter(. == "f.eid" | str_detect(., "f.20544")) %>% .$.
df20544 <- fread(mhq_fp, select = col20544) %>% tibble()

df_cardinal <- fread(mhq_fp, select = c("f.eid","f.20441.0.0", "f.20446.0.0"))



icd_data <- df41270
meds_data <- df20003
selfrep_data <- df20002
extended_baseline <- df20126
cardinal <- df_cardinal
nerves_or_anx <- df2090_2010
selfrep_mhq <- df20544



# Definition of exclusion criteria.

scz <- check_for_code(codes = c("F20", "F25"), data = icd_data, col_name = "schizophrenia")
bipolar <- check_for_code(codes = c("F30", "F31"), data = icd_data, col_name = "bipolar")

psychosis <- check_for_code(
  codes = c("F21", "F22", "F23","F24","F26", "F27", "F28","F29"), data = icd_data, col_name = "psychosis"
)
suds <- check_for_code(
  codes = c("F10", "F11", "F12","F13","F14", "F15", "F16","F18", "F19"), 
  data = icd_data, col_name = "suds"
)

# Medication data used to derive self-reported prescriptions of lithium and antipsychotics

antipsy <- list_of_drugs %>% filter(class == 2) %>% .$drugid
lithium <- list_of_drugs %>% filter(class == 3) %>% .$drugid
antipsy_use <- check_for_code(codes = antipsy, data = meds_data, col_name = "antipsychotics")
lithium_use <- check_for_code(codes = lithium, data = meds_data, col_name = "lithium")


# Extract cases of probable bipolar type 1 & 2 

prob_bip <- extended_baseline %>% 
  mutate(prob_bip_1 = case_when(f.20126.0.0 == 1 ~ 1, !f.20126.0.0 == 1 ~ 0),
         prob_bip_2 = case_when(f.20126.0.0 == 2 ~ 1, !f.20126.0.0 == 2 ~ 0),
  ) %>% 
  select(1,3,4)

# Join all the different exclusion criteria together - broad definition

exclusion <- 
  full_join(antipsy_use, lithium_use, by = "f.eid") %>%  
  full_join(prob_bip, by = "f.eid") %>% 
  full_join(scz, by = "f.eid") %>%
  full_join(bipolar, by = "f.eid") %>%
  full_join(psychosis, by = "f.eid") %>% 
  filter(if_any(-1,~.x == 1))

# For the narrow definition, substance abuse is an additional exclusion criteria.

exclusion_narrow <- 
  exclusion %>% 
  full_join(suds, by = "f.eid") %>% 
  filter(if_any(-1,~.x == 1))

write_tsv(exclusion_narrow, "data/exclusion_criteria_narrow.tsv")
write_tsv(exclusion, "data/exclusion_criteria_broad.tsv")



# PRS population - controls
# Find individuals with ICD codes

icd_md <- check_for_code(codes = c("F32", "F33", "F34", "F38", "F30"), data = icd_data, col_name = "MDD")

cardinal_md <- # Ever depressed for 2 weeks or ever lost interest for two weeks.
  cardinal %>% 
  filter(!is.na(f.20441.0.0) | !is.na(f.20446.0.0)) %>% tibble() %>% 
  filter(if_any(-1, ~. == 1)) %>% 
  mutate(cardinal = 1) %>% 
  select(f.eid, cardinal)

probable_MD <- 
  extended_baseline %>% # probable depression or probable bipolar - remove from controls
  filter(f.20126.0.0 %in% c(1,2,3,4,5))

help_seeking <- nerves_or_anx %>% 
  filter(if_any(-1, ~.x == 1)) %>% 
  mutate(help_seeking = 1) %>% 
  select(f.eid, help_seeking)

antidep <- list_of_drugs %>% filter(class == 1) %>% .$drugid 
antidep_use <- check_for_code(codes = antidep, data = meds_data, col_name = "antidep")

# Selfreported MD from baseline and MHQ

selfrep_baseline <- check_for_code(codes = "1286", data = selfrep_data, col_name = "baseline_selfrep") 
md_mhq_selfrep <- check_for_code(codes = "11", data = selfrep_mhq, col_name = "mhq_selfrep") 


# Join all the different definitions together

all_md_cases <- 
  full_join(cardinal_md, icd_md, by = "f.eid") %>% 
  full_join(., probable_MD, by = "f.eid") %>% 
  full_join(., help_seeking, by = "f.eid") %>% 
  full_join(., antidep_use, by = "f.eid") %>% 
  full_join(., selfrep_baseline, by = "f.eid") %>% 
  full_join(., md_mhq_selfrep, by = "f.eid")

control_exclusion <- 
  all_md_cases %>% mutate(not_control = 1) %>% 
  select(1,not_control)


controls <- # Controls are those participants not in exclusion criteria, or in any indication of MD.
  control_exclusion %>% 
  full_join(cardinal) %>%  # join with a random dataframe to get all UKB individuals
  full_join(.,exclusion,by = "f.eid") %>% 
  mutate(control_md = if_else(is.na(not_control),1,0)) %>% 
  select(f.eid, control_md)



# PRS outcomes
# The PRS outcomes are based on the same criteria as the exclusion critieria. 
# 1. Probable schizophrenia is any ICD code for scz or selfreported antipsychotics prescription 
# 2. Probable bipolar is any ICD code for bipolar, or selfreported lithium prescription


prs_outcomes <- 
  exclusion %>% 
  mutate(
    probable_schizophrenia = case_when(antipsychotics ==  1 ~ 1, schizophrenia == 1 ~1),
    probable_bipolar = case_when(lithium ==  1 ~ 1, bipolar == 1 ~1)) %>% 
  select(f.eid,probable_schizophrenia, probable_bipolar, prob_bip_1, prob_bip_2) %>% 
  full_join(.,extended_baseline, by = "f.eid") %>% 
  mutate( 
    mild_md = case_when(f.20126.0.0 == 5 ~1),
    moderate_md = case_when(f.20126.0.0 == 4 ~1),
    severe_md = case_when(f.20126.0.0 == 3 ~1)
  ) %>% 
  select(-f.20126.0.0) %>% 
  # semi_join(., qc_cohort, by = "f.eid") %>% 
  left_join(., controls, by = "f.eid") %>% 
  mutate(across(1, ~case_when(control_md == 1 ~ 0, .x == 1 ~ 1))) # use control columns to code the controls

# Count the number of cases for each phenotype
num_outcomes <- prs_outcomes %>% 
  select(-f.eid) %>% 
  map_df(sum,na.rm = T) %>% 
  pivot_longer(everything()) %>% 
  rename(n_cases = value)


write_tsv(prs_outcomes, "data/prs_outcomes.tsv")
