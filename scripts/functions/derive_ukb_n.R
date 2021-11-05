derive_ukb_n <- function(icd_codes = NULL, selfrep_codes = NULL, medicine_codes = NULL, merge = TRUE, col_name){
  source("functions/check_for_code.R")
  data <- list()
  icd_data <- fread("~/RAW_DATA_UKB/UKB_41270") %>% tibble()
  meds_data <- fread("~/RAW_DATA_UKB/UKB_20003_SELF_MED.tsv") %>% tibble()
  selfrep_data <- fread("~/RAW_DATA_UKB/UKB_SELFREP_20002.tsv") %>% tibble()
  
  
  if(!is.null(icd_codes)){
    icd_phenotypes <- check_for_code(codes = icd_codes, col_name = col_name, data =  icd_data)
    names(icd_phenotypes)[[2]] <- paste0(names(icd_phenotypes)[[2]], "_icd")
  }
  
  if(!is.null(selfrep_codes)){
    selfrep_phenotypes <- check_for_code(codes = selfrep_codes, col_name = col_name, data =  selfrep_data)
    names(selfrep_phenotypes)[[2]] <- paste0(names(selfrep_phenotypes)[[2]], "_sr")
  }
  
  if(!is.null(medicine_codes)){
    selfrep_med <- check_for_code(codes = medicine_codes, col_name = col_name, data =  meds_data)
    names(selfrep_med)[[2]] <- paste0(names(selfrep_med)[[2]], "_med")
  }

  full_join(icd_phenotypes,selfrep_phenotypes, by="f.eid") %>%
    full_join(.,selfrep_med, by = "f.eid")
    
}









