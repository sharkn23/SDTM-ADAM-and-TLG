# Coding assessment

## Overview
This repository contains solutions for the ADS Coding Assessment. 

## Repository Structure
As shown in the repository organization, each question is isolated into its own directory with a consistent sub-folder structure to ensure professional standards of traceability and reproducibility:

```text
├── question_n/
│   ├── log/       # Console output and execution logs (Evidence of error-free runs)
│   ├── metadata/  # Supporting files like study controlled terminology (study_ct)
│   ├── output/    # Generated datasets (.csv) and reporting files (.docx, .png)
│   └── question_n.R # Primary source code
```
## Project Structure

### 🧬 Question 1: SDTM DS Domain Creation
**Objective:** Transform raw clinical trial data (`ds_raw`) into a standardized SDTM Disposition (DS) domain.

* **Tools:** `{sdtm.oak}`, `pharmaverseraw`.
* **Key Implementation:** Utilized modular mapping functions and Study Controlled Terminology (CT) to map collected values to preferred terms.
* **Variables Derived:** STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM, VISIT, DSDTC, DSSTDTC, DSSTDY.

### 📊 Question 2: ADaM ADSL Dataset Creation
**Objective:** Create a Subject-Level Analysis Dataset (ADSL) using SDTM source data, the `{admiral}` family of packages, and tidyverse tools.

* **Tools:** `{admiral}`, `pharmaversesdtm`, and `{dplyr}`.
* **Key Implementation:**
    * **TRTSDTM & TRTSTMF:** Derived the treatment start datetime using the first exposure record for each participant. Implemented specific imputation logic for missing hours and minutes (setting to 00:00:00) while handling the imputation flag for seconds as required.
    * **LSTAVLDT:** Identified the last known alive date by aggregating and calculating the maximum date across Vital Signs (`VS`), Adverse Events (`AE`), Disposition (`DS`), and Exposure (`EX`) records.
    * **Categorical Grouping:** Created age groupings (**AGEGR9**) into categories of "<18", "18 - 50", and ">50", including the corresponding numeric variable **AGEGR9N**.
    * **ITT Flag:** Implemented the **ITTFL** variable to identify randomized patients where `ARM` is populated in the `DM` domain.
* **Variables Derived:** AGEGR9, AGEGR9N, TRTSDTM, TRTSTMF, ITTFL, and LSTAVLDT.

### 📈 Question 3: TLG - Adverse Events Reporting
**Objective:** Create outputs for adverse events summary using the `ADAE` dataset and `{gtsummary}` to produce regulatory-compliant clinical reports.

* **Tools:** `{gtsummary}`, `{ggplot2}`, and `pharmaverseadam`.
* **Key Implementation:**
    * **Summary Table:** Developed a treatment-emergent adverse event (TEAE) summary table using `{gtsummary}`, sorted by descending frequency.
    * **Visualization:** Used `{ggplot2}` to create 1) AE severity distribution by treatment group. 2) Top 10 most frequent AEs including 95% Confidence Intervals.
    * **Incidence Analysis:** Produced a visualization of the Top 10 most frequent AEs (`AETERM`) including 95% Confidence Intervals for incidence rates.

## Video Walkthrough
A brief 2-minute video (code_walkthrough.mp4) is included in the root directory.
