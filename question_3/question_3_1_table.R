################# Question 3.1 #################
#Create outputs for adverse events summary using the ADAE dataset and {gtsummary}.
#Summary Table using {gtsummary}

library(dplyr)
library(gtsummary)
library(logr)

### create a log file
options("logr.autolog" = TRUE)

log_open("question_3/log/question_3_1_table.log")

### read data

adsl <- pharmaverseadam::adsl
adae <- pharmaverseadam::adae

### preprocessing adae to select treatment-emergent AE records
adae <- adae %>% 
  filter(
    TRTEMFL == "Y"
  )

### create and export table

tbl <- adae %>% 
  tbl_hierarchical(
    variables = c(AESOC, AETERM),
    by = ACTARM,
    id = USUBJID,
    denominator = adsl,
    overall_row = TRUE,
    label = "..ard_hierarchical_overall.." ~ "Treatment Emergent AEs"
  ) %>%
  # add column for all subjects
  add_overall(
    last = TRUE, 
    col_label = "**All Subjects**<br>N = {N}"
  ) %>% 
  # sort in descending order
  sort_hierarchical() %>% 
  # export the table as docx
  as_gt() %>% 
  gt::gtsave(filename = "question_3/output/ae_summary_table.docx")


### close log
log_close()
