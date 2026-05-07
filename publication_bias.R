library(readxl)
library(meta)

# Paths
path <- "C:/Users/user/Downloads/final_Speckle_2026.xlsx"
outcome_sheet <- "GLS"

# Read outcome data
df_gls <- read_excel(path, sheet = outcome_sheet)

# Map columns
mapped_df <- data.frame(
  author = df_gls[[grep("author", colnames(df_gls), ignore.case = TRUE)[1]]],
  n_e = as.numeric(df_gls[[grep("totalintervention", colnames(df_gls), ignore.case = TRUE)[1]]]),
  mean_e = as.numeric(df_gls[[grep("meanintervention", colnames(df_gls), ignore.case = TRUE)[1]]]),
  sd_e = as.numeric(df_gls[[grep("sdintervention", colnames(df_gls), ignore.case = TRUE)[1]]]),
  n_c = as.numeric(df_gls[[grep("totalcontrol", colnames(df_gls), ignore.case = TRUE)[1]]]),
  mean_c = as.numeric(df_gls[[grep("meancontrol", colnames(df_gls), ignore.case = TRUE)[1]]]),
  sd_c = as.numeric(df_gls[[grep("sdcontrol", colnames(df_gls), ignore.case = TRUE)[1]]])
)
mapped_df <- subset(mapped_df, !is.na(author) & !is.na(mean_e))

# Perform Meta-analysis
m <- metacont(n.e = n_e, mean.e = mean_e, sd.e = sd_e,
              n.c = n_c, mean.c = mean_c, sd.c = sd_c,
              data = mapped_df, studlab = author, sm = "SMD", method.tau = "REML")

# Funnel Plot
png("results/funnel_GLS.png", width = 800, height = 600)
funnel(m, main = "Funnel Plot for GLS")
dev.off()

# Egger's Test
cat("\n--- Egger's Test for GLS ---\n")
print(metabias(m, method = "linreg"))

# Trim and Fill
cat("\n--- Trim and Fill for GLS ---\n")
tf <- trimfill(m)
print(tf)
png("results/funnel_TF_GLS.png", width = 800, height = 600)
funnel(tf, main = "Funnel Plot (Trim & Fill) for GLS")
dev.off()
