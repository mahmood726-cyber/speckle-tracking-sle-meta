# Speckle Tracking Echocardiography in SLE: Meta-analysis (2026)

This repository contains the data, R scripts, and results for a comprehensive meta-analysis of speckle tracking echocardiography (STE) parameters in patients with Systemic Lupus Erythematosus (SLE) compared to healthy controls.

## Outcomes Analyzed
- Global Longitudinal Strain (GLS)
- Global Circmferential Strain (GCS)
- Global Radial Strain (GRS)
- Peak Systolic Dispersion (PSD)
- Left Atrial Volume Index (LAVI)
- Left Ventricular Mass Index (LVMI)
- LV Torsion
- Left Ventricular End-Diastolic Volume (LVEDV)

## Key Findings
- **GLS Impairment**: Significant impairment in GLS (SMD 1.09, p < 0.001) in SLE patients.
- **Moderator Analysis**: SLEDAI (Disease Activity) is a significant moderator of GLS impairment ($p=0.0115$).
- **Publication Bias**: No significant publication bias detected (Egger's p = 0.65).

## Repository Structure
- `main_analysis.R`: Script for primary pooled estimates and forest plots.
- `meta_regression.R`: Script for moderator analysis using demographic data.
- `publication_bias.R`: Script for assessment of publication bias.
- `results/`: Directory containing all forest plots, funnel plots, and numerical summaries.

## Software Requirements
- R 4.5.2+
- Packages: `meta`, `readxl`, `dplyr`, `ggplot2`
