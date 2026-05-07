library(readxl)
library(meta)
library(dplyr)

# Paths
path <- "C:/Users/user/Downloads/final_Speckle_2026.xlsx"
outcome_sheet <- "GLS"
demo_sheet <- "Demographic data"

# Read outcome data
df_gls <- read_excel(path, sheet = outcome_sheet)
# Read demographic data
df_demo <- read_excel(path, sheet = demo_sheet)

# Clean and Map GLS
mapped_gls <- data.frame(
  Study = tolower(trimws(df_gls[[grep("author", colnames(df_gls), ignore.case = TRUE)[1]]])),
  n_e = as.numeric(df_gls[[grep("totalintervention", colnames(df_gls), ignore.case = TRUE)[1]]]),
  mean_e = as.numeric(df_gls[[grep("meanintervention", colnames(df_gls), ignore.case = TRUE)[1]]]),
  sd_e = as.numeric(df_gls[[grep("sdintervention", colnames(df_gls), ignore.case = TRUE)[1]]]),
  n_c = as.numeric(df_gls[[grep("totalcontrol", colnames(df_gls), ignore.case = TRUE)[1]]]),
  mean_c = as.numeric(df_gls[[grep("meancontrol", colnames(df_gls), ignore.case = TRUE)[1]]]),
  sd_c = as.numeric(df_gls[[grep("sdcontrol", colnames(df_gls), ignore.case = TRUE)[1]]])
) %>% filter(!is.na(Study) & !is.na(mean_e))

# Clean and Map Demo
# Study names might need fuzzy matching or normalization
mapped_demo <- data.frame(
  Study = tolower(trimws(df_demo$Study)),
  Age = as.numeric(df_demo$`Age SLE`),
  Duration = as.numeric(df_demo$`Duration (yrs)`),
  SLEDAI = as.numeric(df_demo$`SLEDAI (mean)`),
  LVEF = as.numeric(df_demo$`LVEF (%)`),
  Renal = as.numeric(df_demo$`Renal / LN (%)`)
) %>% filter(!is.na(Study))

# Join
df_merged <- inner_join(mapped_gls, mapped_demo, by = "Study")

cat(paste("Matched", nrow(df_merged), "studies for meta-regression.\n"))

if (nrow(df_merged) >= 10) {
  # Perform Meta-analysis
  m <- metacont(n.e = n_e, mean.e = mean_e, sd.e = sd_e,
                n.c = n_c, mean.c = mean_c, sd.c = sd_c,
                data = df_merged, studlab = Study, sm = "SMD", method.tau = "REML")
  
  # Meta-regression: Age
  cat("\n--- Meta-regression: Age ---\n")
  reg_age <- metareg(m, ~ Age)
  print(reg_age)
  
  # Meta-regression: Duration
  cat("\n--- Meta-regression: Disease Duration ---\n")
  reg_dur <- metareg(m, ~ Duration)
  print(reg_dur)
  
  # Meta-regression: SLEDAI
  cat("\n--- Meta-regression: SLEDAI (Disease Activity) ---\n")
  reg_sledai <- metareg(m, ~ SLEDAI)
  print(reg_sledai)
} else {
  cat("Insufficient matched studies for robust meta-regression.\n")
}
