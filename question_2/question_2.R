################# Question 2 #################
#Create an ADSL (Subject Level) dataset using SDTM source data, the {admiral} family of packages, and tidyverse tools.  

library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(pharmaversesdtm)
library(lubridate)
library(stringr)
library(logr)
library(readr)

### create a log file
options("logr.autolog" = TRUE)

log_open("question_2/log/question_2.log")

### read data

dm <- convert_blanks_to_na(pharmaversesdtm::dm)
vs <- convert_blanks_to_na(pharmaversesdtm::vs)
ex <- convert_blanks_to_na(pharmaversesdtm::ex)
ds <- convert_blanks_to_na(pharmaversesdtm::ds)
ae <- convert_blanks_to_na(pharmaversesdtm::ae)

### assign DM domain as basis fro ADSL

adsl <- dm %>%
  select(-DOMAIN)

### derive age groups (AGEGR9, AGEGR9N)

agegr9_lookup <- exprs(
  ~condition,            ~AGEGR9, ~AGEGR9N,
  AGE < 18,                "<18",        1,
  between(AGE, 18, 50),  "18-50",        2,
  AGE > 50,                ">50",        3
)

adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr9_lookup
)

### derive numeric treatment date/time (TRTSDTM) and imputation flag (TRTSTMF)

# Impute start and end time of exposure to first and last respectively,
# Do not impute date
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",
    time_imputation = "00:00:00",
    ignore_seconds_flag = TRUE,
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN"
  )

# derive treatment variables in ADSL  
adsl <- adsl %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  )


### derive population flag (ITTFL)
adsl <- adsl %>%
  derive_var_merged_exist_flag(
    dataset_add = dm,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = ITTFL,
    false_value = "N",
    condition = !is.na(ARM)
  )

### derive Last Date Known Alive (LSTALVDT)
adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(convert_dtc_to_dt(VSDTC)) & (!is.na(VSSTRESN) | !is.na(VSSTRESC)),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(VSDTC),
          LALVSEQ = VSSEQ,
          LALVDOM = "VS",
          LALVVAR = "VSDTC"
        ),
      ),
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(convert_dtc_to_dt(AESTDTC)),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(AESTDTC),
          LALVSEQ = AESEQ,
          LALVDOM = "AE",
          LALVVAR = "AESTDTC"
        ),
      ),
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(convert_dtc_to_dt(DSSTDTC)),
        set_values_to = exprs(
          LSTALVDT = convert_dtc_to_dt(DSSTDTC),
          LALVSEQ = DSSEQ,
          LALVDOM = "DS",
          LALVVAR = "DSSTDTC"
        ),
      ),
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDTM),
        set_values_to = exprs(LSTALVDT = TRTEDTM, LALVSEQ = NA_integer_, LALVDOM = "ADSL", LALVVAR = "TRTEDTM"),
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTALVDT, LALVSEQ, event_nr),
    mode = "last",
    new_vars = exprs(LSTALVDT, LALVSEQ, LALVDOM, LALVVAR)
  )

### write output to csv

write_csv(adsl, "question_2/output/adsl.csv") 

### close log
log_close()
