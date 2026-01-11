library(gemini.R)
library(depmixS4)
library(readr)
library(dplyr)
library(tidyr)

# Set workspace to repo root to ensure data paths work
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()
setwd(workspace)

# Load wide format data and the trained model
df_raw  <- read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# Identify the High Sentiment state index
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(nstates(fit)), function(i) {
    getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)

# Ensure date sorting and fill quarterly data down to monthly
df_prep <- df_raw %>%
  arrange(date) %>%
  tidyr::fill(gdp_real, pcepi, FYFSD, .direction = "down") %>%
  filter(!is.na(cons_sent)) %>%
  filter(date >= as.Date("1987-01-01")) %>%
  mutate(
    real_GDP_L1 = lag(gdp_real, 1),
    FYFSD_L1    = lag(FYFSD, 1),
    PCEPI_L0    = pcepi
  ) %>%
  drop_na()

# Z-Score Scaling
df_scaled <- df_prep %>%
  mutate(across(c(real_GDP_L1, FYFSD_L1, PCEPI_L0, cons_sent), ~scale(.)[,1]))

# Prepare the model structure
cov_vars <- c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")
n_st     <- nstates(hmm_mod)
frm      <- as.formula(paste("~", paste(cov_vars, collapse = " + ")))

mod_new <- depmix(
  response = cons_sent ~ 1,
  data = df_scaled,
  nstates = n_st,
  family = gaussian(),
  transition = frm
)

# Inject trained parameters and run posterior decoding
mod_applied <- setpars(mod_new, getpars(hmm_mod))
post_probs  <- posterior(mod_applied, type = "smoothing")

df_scaled$state_idx <- post_probs$state
df_scaled$p_high    <- post_probs[, HI_STATE_IDX + 1]

# Assign Regime Labels
df_scaled <- df_scaled %>%
  mutate(
    regime = ifelse(p_high >= 0.5, "High Sentiment", "Low Sentiment")
  )

# Current Status
latest <- tail(df_scaled, 1)
curr_regime <- latest$regime
curr_prob   <- latest$p_high
curr_csi    <- tail(df_prep$cons_sent, 1)

# Regime Length and Previous Streak
runs <- rle(df_scaled$regime)
curr_streak <- tail(runs$lengths, 1)
prev_streak <- if(length(runs$lengths) > 1) tail(runs$lengths, 2)[1] else 0

# Historical Match and Anomaly Scores
indicators <- cov_vars
means_by_regime <- df_scaled %>%
  group_by(regime) %>%
  summarise(across(all_of(indicators), \(x) mean(x, na.rm = TRUE)))

mean_curr_vec <- means_by_regime %>% 
  filter(regime == curr_regime) %>% 
  select(all_of(indicators)) %>% 
  as.numeric()

mean_other_vec <- means_by_regime %>% 
  filter(regime != curr_regime) %>% 
  select(all_of(indicators)) %>% 
  as.numeric()

# Compare Latest Values to Baselines
vals_current <- as.numeric(latest[indicators])

# Distance calculation
dist_to_curr  <- abs(vals_current - mean_curr_vec)
dist_to_other <- abs(vals_current - mean_other_vec)

# Historical Match Percent
n_aligned   <- sum(dist_to_curr <= 1.0)
pct_aligned <- round((n_aligned / length(indicators)) * 100, 0)

# Average Anomaly Score
avg_anomaly <- round(mean(dist_to_curr), 2)

# Regime Expansion
n_leaning_other   <- sum(dist_to_other < dist_to_curr)
pct_leaning_other <- (n_leaning_other / length(indicators)) * 100

expansion_status <- if (curr_regime == "Low Sentiment") {
  if (pct_leaning_other > 50) "Recovery Detected (Drivers shifting to Growth)" else "Deep Contraction (Reinforcing Low State)"
} else {
  if (pct_leaning_other > 50) "Warning Signal (Drivers shifting to Volatility)" else "Strong Expansion (Reinforcing High State)"
}

# Construct Gemini Prompt
prompt <- paste0(
  "You are an expert macroeconomic analyst. Interpret the latest results from our proprietary Hidden Markov Model (HMM) for US Consumer Sentiment.\n\n",
  
  "### MODEL STATUS\n",
  "- **Current Regime:** ", curr_regime, "\n",
  "- **Regime Probability:** ", round(curr_prob * 100, 1), "%\n",
  "- **Current Streak:** ", curr_streak, " Months\n",
  "- **Previous Regime Length:** ", prev_streak, " Months\n",
  "- **Consumer Sentiment Index (Raw):** ", curr_csi, "\n\n",
  
  "### DRIVER HEALTH (Z-SCORES)\n",
  "Analysis of the 3 key model drivers vs their historical baselines for this regime:\n",
  "- **Real GDP (Lagged):** Current Z: ", round(latest$real_GDP_L1, 2), " (Baseline: ", round(mean_curr_vec[1], 2), ")\n",
  "- **PCE Inflation:** Current Z: ", round(latest$PCEPI_L0, 2), " (Baseline: ", round(mean_curr_vec[2], 2), ")\n",
  "- **Fed Surplus/Deficit:** Current Z: ", round(latest$FYFSD_L1, 2), " (Baseline: ", round(mean_curr_vec[3], 2), ")\n\n",
  
  "### DIAGNOSTICS\n",
  "- **Historical Match:** ", pct_aligned, "% (Percentage of drivers behaving 'normally' for this regime)\n",
  "- **Avg Anomaly Score:** ", avg_anomaly, " (Avg standard deviations from baseline)\n",
  "- **Expansion Status:** ", expansion_status, "\n\n",
  
  "### TASK\n",
  "Write a sophisticated, 1-paragraph executive summary (approx 100 words) for a portfolio manager. ",
  "Do not just list the numbers. Explain the NARRATIVE. Is the current regime stable or fragile? ",
  "Which specific indicator is causing the most drag or lift? ",
  "Reference the 'Expansion Status' to determine if a pivot is imminent."
)

# Call API and save result
setAPIKey(Sys.getenv("GEMINI_API_KEY"))

tryCatch({
  analysis <- gemini(prompt)
  write_lines(analysis, "data/ai_analysis.md")
  print("AI Analysis successfully generated.")
}, error = function(e) {
  print(paste("Error calling Gemini API:", e$message))
  write_lines("AI Analysis unavailable at this time due to API connection issues.", "data/ai_analysis.md")
})
