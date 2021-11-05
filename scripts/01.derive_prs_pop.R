library(tidyverse)
library(data.table)

source("functions/check_for_code.R")
list_of_drugs <- fread("data/UKB_f20003_antidep_psy_lithi.txt")
qc_cohort <- read_tsv("data/ignore/qc_cohort.tsv")


# Filepaths and reading the required data
# ============================================================================================

df41202_41204="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_ICD_41202_41204.tsv"
df20003="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_20003_SELF_MED.tsv"
df20002="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_SELFREP_20002.tsv"
df20126="/proj/sens2017519/nobackup/b2016326_nobackup/private/arvhar/data/UKB_RAW_DATA/ukb_data/UKB_20126.tsv"
df2090_2010="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_2090_2010.tsv"
dfcidi="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_CIDI_SF.tsv"
df20544="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_SELFREP_20544.tsv"

# ============================================================================================



icd_data <- fread(df41202_41204) %>% tibble()
meds_data <- fread(df20003) %>% tibble()
selfrep_data <- fread(df20002) %>% tibble()
extended_baseline <- fread(df20126) %>% tibble()
cardinal <- fread(dfcidi) %>% tibble()
nerves_or_anx <- fread(df2090_2010) %>% tibble()
selfrep_mhq <- fread(df20544) %>% tibble()



# ========================================================================================
# Derive the psychiatric disorders to be excluded

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

# Join all the different exclusion criteria together - broad definition

exclusion_narrow <- 
  full_join(antipsy_use, lithium_use, by = "f.eid") %>%  
  full_join(prob_bip, by = "f.eid") %>% 
  full_join(scz, by = "f.eid") %>%
  full_join(bipolar, by = "f.eid") %>%
  full_join(psychosis, by = "f.eid") %>% 
  full_join(suds, by = "f.eid") %>% 
  filter(if_any(-1,~.x == 1))

# Save the exclusion criteria. They are used in 02.derive_phenotypes.R

write_tsv(exclusion_narrow, "exclusion_criteria_narrow.tsv")
write_tsv(exclusion, "exclusion_criteria_broad.tsv")

# =========================================================================================
# Derive the controls for the PRS population

icd_md <- check_for_code(codes = c("F32", "F33", "F34", "F38", "F30"), data = icd_data, col_name = "MDD")

cardinal_md <- # Cardinal symptoms from MHQ
  cardinal %>% 
  filter(f.20441.0.0 == 1 | f.20446.0.0 == 1) %>% 
  mutate(cardinal_md = 1) %>% 
  select(1, cardinal_md)

probable_MD <- extended_baseline %>% # Probable depression 
  filter(f.20126.0.0 %in% c(3,4,5))

help_seeking <- nerves_or_anx %>% 
  filter(if_any(-1, ~.x == 1)) %>% 
  mutate(help_seeking = 1) %>% 
  select(f.eid, help_seeking)

antidep <- list_of_drugs %>% filter(class == 1) %>% .$drugid 
antidep_use <- check_for_code(codes = antidep, data = meds_data, col_name = "antidep")

# Selfreported MD from baseline and MHQ
selfrep_baseline <- check_for_code(codes = "1286", data = selfrep_data, col_name = "baseline_selfrep") 
md_mhq_selfrep <- check_for_code(codes = "11", data = selfrep_mhq, col_name = "mhq_selfrep") 


all_md_cases <- # Join all the different definitions together
  full_join(cardinal_md, icd_md, by = "f.eid") %>% 
  full_join(., probable_MD, by = "f.eid") %>% 
  full_join(., help_seeking, by = "f.eid") %>% 
  full_join(., antidep_use, by = "f.eid") %>% 
  full_join(., selfrep_baseline, by = "f.eid") %>% 
  full_join(., md_mhq_selfrep, by = "f.eid")

  
  
  
controls <- # Controls are those participants not in exclusion criteria, or in any indication of MD.
  all_md_cases %>% mutate(not_control = 1) %>% 
  select(1,not_control) %>% 
  full_join(.,exclusion,by = "f.eid") %>% 
  mutate(not_control = 1) %>%
  full_join(.,nerves_or_anx,by = "f.eid") %>% 
  mutate(control_md = case_when(is.na(not_control) ~ 1)) %>% 
  select(f.eid, control_md)



                             
# ===========================================================================================
# Derive the PRS outcomes - They are based on the same criteria as exclusions

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
  semi_join(., qc_cohort, by = "f.eid") %>%
  left_join(., controls, by = "f.eid") %>% 
  mutate(across(1, ~case_when(control_md == 1 ~ 0, .x == 1 ~ 1))) # use control columns to code the controls

# ===========================================================================================

# Count the number of cases for each phenotype

num_outcomes <- 
  prs_outcomes %>% 
  map_df(sum,na.rm = T) %>% 
  pivot_longer(everything()) %>% 
  rename(n_cases = value)


write_tsv(prs_outcomes, "prs_outcomes.tsv")
write_tsv(num_outcomes, "num_outcomes.tsv")