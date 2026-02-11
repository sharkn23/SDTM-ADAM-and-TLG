################# Question 1 #################
#Create an SDTM Disposition (DS) domain dataset from raw clinical trial data using the {sdtm.oak}. 

library(pharmaverseraw)
library(pharmaversesdtm)
library(sdtm.oak)
library(dplyr)
library(logr)
library(readr)

### create a log file
options("logr.autolog" = TRUE)

log_open("question_1/log/question_1.log")

### read data

ds_raw <- pharmaverseraw::ds_raw

### read DM domain (for study day calculation)

dm <- pharmaversesdtm::dm

### create oak_id_vars

ds_raw <- ds_raw %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "ds_raw"
  )

### read controlled terminology

study_ct <- read.csv("question_1/metadata/sdtm_ct.csv")

### map topic variable

ds <-
  # Map DSTERM using assign_no_ct
  # if OTHERSP is not null then DSTERM <- OTHERSP, else DSTERM <- IT.DSTERM
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM"
  ) %>% 
  assign_no_ct(
    raw_dat = ds_raw,
    raw_var = "IT.DSTERM",
    tgt_var = "DSTERM"
  )

### map the rest of variables

ds <- ds %>%
  ## Map DSDECOD using assign_ct
  # if OTHERSP is null then DSDECOD <- IT.DSDECOD
  assign_ct(
    raw_dat = condition_add(ds_raw, is.na(OTHERSP)),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>% 
  # if OTHERSP is not null then DSDECOD <- OTHERSP
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>% 
  ## Map DSCAT using assign_ct
  # if OTHERSP is not null then DSCAT <- "OTHER EVENT"
  hardcode_ct(
    raw_dat = condition_add(ds_raw, !is.na(OTHERSP)),
    raw_var = "OTHERSP",
    tgt_var = "DSCAT",
    tgt_val = "OTHER EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # if IT.DSDECOD = "Randomized" then DSCAT <- "PROTOCOL MILESTONE"
  hardcode_ct(
    raw_dat = condition_add(ds_raw, IT.DSDECOD == "Randomized"),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "PROTOCOL MILESTONE",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  # else DSCAT <- "DISPOSITION EVENT"
  hardcode_ct(
    raw_dat = condition_add(ds_raw),
    raw_var = "IT.DSDECOD",
    tgt_var = "DSCAT",
    tgt_val = "DISPOSITION EVENT",
    ct_spec = study_ct,
    ct_clst = "C74558",
    id_vars = oak_id_vars()
  ) %>%
  ## Map DSSTDTC using assign_datetime
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = "IT.DSSTDAT",
    tgt_var = "DSSTDTC",
    raw_fmt = c("m-d-y")
  ) %>%
  ## Map DSDTC using assign_datetime
  assign_datetime(
    raw_dat = ds_raw,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    tgt_var = "DSDTC",
    raw_fmt = c("m-d-y", "H:M")
  ) %>%
  ## Map VISITNUM using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  ) %>%
  ## Map VISIT using assign_ct
  assign_ct(
    raw_dat = ds_raw,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  )


### create SDTM derived variables

ds <- ds %>%
  dplyr::mutate(
    STUDYID = ds_raw$STUDY,
    DOMAIN = "DS",
    USUBJID = paste0("01-", ds_raw$PATNUM)
  ) %>%
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSTERM")
  ) %>%
  derive_study_day(
    sdtm_in = .,
    dm_domain = dm,
    tgdt = "DSSTDTC",
    refdt = "RFXSTDTC",
    study_day_var = "DSSTDY"
  ) %>%
  select(
    "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "VISITNUM", "VISIT", "DSDTC", "DSSTDTC", "DSSTDY"
  )

### write output to csv

write_csv(ds, "question_1/output/ds.csv")

### close log
log_close()