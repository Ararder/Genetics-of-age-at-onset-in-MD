library(tidyverse)
library(data.table)

# Filepaths for reading in data
# ======================================================================================
df20002="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_SELFREP_20002.tsv"
df20009="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/ukb_df_20009_age_at_diagnosis.tsv"
df20433="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/ukb22140_180524_f20433.csv"
dfcidi="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_CIDI_SF.tsv"
df21022="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_21022_AGE.tsv"
df31="/home/arvhar/data/data/UKB_RAW_DATA/ukb_data/UKB_DF31_SEX.tsv"
# ======================================================================================


selfrep_diag <- fread(df20002) %>% tibble()
age_at_diag <- fread(df20009) %>% tibble()  
df20433 <- fread(df20433) %>% tibble()
cidi <- fread(dfcidi) %>% tibble()
age <- fread(df21022)
sex <- fread(df31)

# GWAS phenotypes - these covariates are regressed out.

covariates <- inner_join(age, sex, by = "f.eid") %>% 
  mutate(age_sex = age*sex, age2 = age^2, age2_sex = age^2*sex)

# Derived in 00.quality_control_cohort.Rmd

qc_cohort <- read_tsv("data/ignore/qc_cohort.tsv")

# These files are derived in 01.derive_prs_pop

excl <- read_tsv("exclusion_criteria_broad.tsv") 
excl_narrow <- read_tsv("exclusion_criteria_narrow.tsv")

# ======================================================================================
# Define age at symptoms (broad and narrow) and age at diagnosis

join_pairwise <- function(index,data1, data2, code){
  # Takes as input two dataframes with the same number of columns. (In this case, data field 20002 and 20009)
  # At each column position, joins the column from each dataset into one.
  # Lastly, retains only rows where the code is matched - ie a specific diagnosis
  dat1 <- data1 %>% 
    select(1,{{ index }} )
  
  dat2 <- data2 %>% 
    select(1, {{ index  }} )
  
  dat1 %>% 
    filter(if_any(-1, ~str_detect(string = .x, pattern = code))) %>% 
    inner_join(., dat2, by = "f.eid") %>% 
    select(1,3) %>% 
    set_names(c("f.eid", "age_at_diagnosis"))
}

age_at_diagnosis <-
  seq(from = 2, to = 88) %>% # for each column, join together with corresponding age at diagnosis column.
  map(join_pairwise, data1 = selfrep_diag, data2 = age_at_diag, code = "1286") %>% # retain only individuals with code 1286 - MDD
  reduce(bind_rows) %>% 
  distinct(f.eid,.keep_all = TRUE)# Bind together all cases found.





# Apply QC and exclusion criteria for age at diagnosis

age_at_diagnosis_qc <- 
  age_at_diagnosis %>% 
  filter(age_at_diagnosis > 1) %>% # remove -1,  "Do now know" and "Prefer not to answer" 
  semi_join(., qc_cohort, by = "f.eid") %>% 
  anti_join(., excl, by = "f.eid") 


age_at_symptoms <-
  df20433 %>%
  tibble() %>%  
  rename(f.eid = 1, aao = 2)

qc_aao_broad <- 
  age_at_symptoms %>% 
  filter(aao > 0) %>% # remove -818, -121 - "Do now know" and "Prefer not to answer" 
  semi_join(., qc_cohort, by = "f.eid") %>%  
  anti_join(., excl, by = "f.eid")



# Narrow definition of age at symptoms

cidi_score <- 
  # Recode columns to just 1/0 (the ones we use to derive total score)
  cidi %>% 
    mutate(across(c(-f.eid,-f.20440.0.0,-f.20536.0.0), ~case_when(. == 1 ~ 1, . != 1 ~ 0))) %>% 
  # Weightgain has several levels. Recode everything above 1 to 0.
  mutate(f.20536.0.0 = case_when(
    f.20536.0.0 == 1 ~ 1, f.20536.0.0 == 2 ~ 1,
    f.20536.0.0 == 3 ~ 1, f.20536.0.0 < 1 ~ 0))

# Derive the total number of symptoms

cidi_score$tot_score <- 
  cidi_score %>% 
  select(-f.eid, -f.20440.0.0) %>%  # remove ID, and item regarding impact on normal roles
  mutate(tot_score = rowSums(., na.rm = TRUE)) %>% 
  select(tot_score)

age_at_symptoms_narrow <- 
  cidi_score %>% 
  filter(tot_score >= 5) %>% 
  filter(f.20440.0.0 >= 3) %>% # Impact on normal roles has to be "high"
  select(f.eid) %>% 
  anti_join(., excl_narrow, by = "f.eid") %>% 
  inner_join(., qc_aao_broad)



write_tsv(age_at_diagnosis_qc, "age_at_diagnosis.tsv")
write_tsv(age_at_symptoms_narrow, "age_at_symptoms_narrow.tsv")
write_tsv(qc_aao_broad, "age_at_symptoms_broad.tsv")





kinship <- # Kinships between age at diagnosis and age at symptoms, estimated with KING
  fread("data/ignore/relatedness_between_cohorts.id") %>% tibble()

#overlap between cohorts.
overlap1 <- kinship %>%   # 1)  nurse cohort individual is in column ID1 and is related to MHQ individual in column ID2
  filter(ID1 %in% age_at_diagnosis$f.eid & ID2 %in% qc_aao_broad$f.eid) %>% rename(f.eid = ID1)
overlap2 <- kinship %>% # 2) nurse cohort individual in ID column 2, related to MHQ individual in ID column1
  filter(ID2 %in% age_at_diagnosis$f.eid & ID1 %in% qc_aao_broad$f.eid) %>% rename(f.eid = ID2)
related_samples <-  bind_rows(overlap1,overlap2)



rint_transform <- function(covariates, phenotype){
  
  data <- phenotype %>% 
    inner_join(., covariates, by = "f.eid") %>% 
    rename(aao = 2)
  model <- lm(aao ~ age + sex + age_sex + age2 + age2_sex, data = data)
  data$resid <- model$residuals
  data$qnorm <- qnorm((rank(data$resid, na.last = "keep")- 0.5)/sum(!is.na(data$resid)))
  
  data %>%
    mutate(iid = f.eid) %>% select(f.eid,iid, qnorm)
}

rint_transform(covariates, qc_aao_broad) %>% 
  write_tsv(., "age_at_symptoms_broad_GWAS", col_names = F)
rint_transform(covariates, age_at_symptoms_narrow) %>% 
  write_tsv(., "age_at_symptoms_narrow_GWAS", col_names = F)

age_at_diagnosis_qc %>% # For age at diagnosis, we first need to remove overlap and relatedness 
  anti_join(., qc_aao_broad, by = "f.eid") %>% # Overlap
  anti_join(., related_samples, by = "f.eid") %>% # Related samples
  rint_transform(covariates, .) %>% 
  write_tsv(., "age_at_diagnosis_GWAS", col_names = F)


age_at_diagnosis_qc %>% # For the estimation of h2 with GCTA-GREML overlap is not removed
  rint_transform(covariates, .) %>% 
  write_tsv(., "age_at_diagnosis_GREML", col_names = F)
