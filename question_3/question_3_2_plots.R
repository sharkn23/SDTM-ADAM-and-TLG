################# Question 3.2 #################
#Create outputs for adverse events summary using the ADAE dataset and {gtsummary}.
#Visualizations using {ggplot2)

library(dplyr)
library(ggplot2)
library(pharmaverseadam)
library(binom)
library(logr)

### create a log file
options("logr.autolog" = TRUE)

log_open("question_3/log/question_3_2_plots.log")

### read data

adae <- pharmaverseadam::adae

### plot1: AE severity distribution by treatment

# create plot1
plot1 <- ggplot(adae) +
  # add bars and fill
  geom_bar(aes(x = ACTARM, fill = AESEV))+
  # add title and labels
  labs(
    title = "AE severity distributiom by treatment",
    x = "Treatment Arm",
    y = "Count of AEs"
  )

# save plot1 as png
ggsave(
  filename = "question_3/output/plot1.png", 
  plot = plot1
)

### plot2: Top 10 most frequent AEs (with 95% CI for incidence rates)

# calculate total number of subjects
n_total <- n_distinct(adae$USUBJID)

# calculate incidence and Clopper-Pearson CIs
ae_summary <- adae %>%
  group_by(AETERM) %>%
  summarise(n_ae = n_distinct(USUBJID)) %>%
  mutate(
    percent = (n_ae / n_total) * 100,
    # calculate Clopper-Pearson 95% CI
    ci_low = binom.confint(n_ae, n_total, methods = "exact")$lower * 100,
    ci_high = binom.confint(n_ae, n_total, methods = "exact")$upper * 100
  ) %>%
  arrange(desc(percent)) %>%
  # select top 10 rows
  slice_head(n = 10)

# create plot2
plot2 <- ggplot(ae_summary, aes(x = percent, y = reorder(AETERM, percent))) +
  # add and format error bar
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.2, orientation = "y") +
  # add point
  geom_point(size = 4) +
  # add titles and labels
  labs(
    title = "Top 10 Most Frequent Adverse Events",
    subtitle = paste0("n = ", n_total, " subjects; 95% Clopper-Pearson CIs"),
    x = "Percentage of Patients (%)",
    y = NULL
  ) +
  # adjust x-axis to add % sign
  scale_x_continuous(labels = function(x) paste0(x, "%"))

# save plot1 as png
ggsave(
  filename = "question_3/output/plot2.png", 
  plot = plot2
)

### close log
log_close()