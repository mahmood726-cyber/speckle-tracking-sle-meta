library(readxl)
library(meta)
library(ggplot2)

# Set paths
path <- "C:/Users/user/Downloads/final_Speckle_2026.xlsx"
outcomes <- c("GLS", "GCS", "GRS", "PSD", "LAVI", "LVMI", "Torsion", "LVEDV")
results_dir <- "results"

if (!dir.exists(results_dir)) dir.create(results_dir)

summary_list <- list()

for (outcome in outcomes) {
  cat(paste("\nProcessing outcome:", outcome, "...\n"))

  # Read data
  df <- tryCatch({
    read_excel(path, sheet = outcome)
  }, error = function(e) {
    cat(paste("  Error reading sheet", outcome, ":", e$message, "\n"))
    return(NULL)
  })

  if (is.null(df)) next

  # Map columns
  col_names <- tolower(colnames(df))
  mapped_df <- data.frame(
    author = df[[grep("author", colnames(df), ignore.case = TRUE)[1]]],
    n_e = as.numeric(df[[grep("totalintervention", colnames(df), ignore.case = TRUE)[1]]]),
    mean_e = as.numeric(df[[grep("meanintervention", colnames(df), ignore.case = TRUE)[1]]]),
    sd_e = as.numeric(df[[grep("sdintervention", colnames(df), ignore.case = TRUE)[1]]]),
    n_c = as.numeric(df[[grep("totalcontrol", colnames(df), ignore.case = TRUE)[1]]]),
    mean_c = as.numeric(df[[grep("meancontrol", colnames(df), ignore.case = TRUE)[1]]]),
    sd_c = as.numeric(df[[grep("sdcontrol", colnames(df), ignore.case = TRUE)[1]]])
  )

  # Filter out rows with missing data
  mapped_df <- mapped_df[complete.cases(mapped_df), ]

  if (nrow(mapped_df) < 2) {
    cat(paste("  Insufficient data for", outcome, "(", nrow(mapped_df), "studies)\n"))
    next
  }
  
  # Perform Meta-analysis (SMD) - Hedges' g is default for SMD in metacont
  m_smd <- metacont(n.e = n_e, mean.e = mean_e, sd.e = sd_e,
                   n.c = n_c, mean.c = mean_c, sd.c = sd_c,
                   data = mapped_df, studlab = author,
                   sm = "SMD", method.tau = "REML", title = outcome)

  # Save Forest Plot (SMD) - Improved aesthetics
  png(file.path(results_dir, paste0("forest_SMD_", outcome, ".png")), width = 1200, height = 800, res = 120)
  forest(m_smd, 
         leftcols = c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c"),
         leftlabs = c("Study", "N (SLE)", "Mean", "SD", "N (Ctrl)", "Mean", "SD"),
         prediction = TRUE, # Clinical Statistician: Show prediction interval
         col.predict = "red",
         print.tau2 = TRUE,
         print.I2 = TRUE)
  dev.off()

  # Leave-one-out sensitivity analysis
  inf <- metainf(m_smd)
  png(file.path(results_dir, paste0("sensitivity_LOO_", outcome, ".png")), width = 1000, height = 800, res = 120)
  forest(inf, main = paste("Leave-one-out Sensitivity:", outcome))
  dev.off()

  # Store summary
  summary_list[[outcome]] <- data.frame(
    Outcome = outcome,
    Studies = m_smd$k,
    SMD = m_smd$TE.random,
    SMD_Lower = m_smd$lower.random,
    SMD_Upper = m_smd$upper.random,
    SMD_Pval = m_smd$pval.random,
    I2 = m_smd$I2 * 100,
    Tau2 = m_smd$tau2,
    PI_Lower = m_smd$lower.predict, # Prediction Interval
    PI_Upper = m_smd$upper.predict
  )
}

if (length(summary_list) > 0) {
  final_summary <- do.call(rbind, summary_list)
  write.csv(final_summary, "results/final_summary_comprehensive.csv", row.names = FALSE)
  print(final_summary)
}
