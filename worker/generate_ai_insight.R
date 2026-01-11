library(gemini.R)
library(depmixS4)
library(readr)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. SETUP & PATHS
# ==============================================================================
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()
setwd(workspace)

# Constants from your App (To ensure identical behavior)
TRAIN_END <- as.Date("2024-12-01")

if (!file.exists("data/fred_raw_wide.csv")) stop("ERROR: fred_raw_wide.csv missing")
if (!file.exists("data/m4_monthly_fit.rds")) stop("ERROR: m4_monthly_fit.rds missing")

# ==============================================================================
# 2. DATA LOADING & PREP (Matching app.R logic)
# ==============================================================================
df_raw  <- read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# Identify High State
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(nstates(fit)), function(i) {
    getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)

# Clean Data
df_prep <- df_raw %>%
  arrange(date) %>%
  select(date, gdp_real, pcepi, FYFSD, cons_sent) %>%
  tidyr::fill(gdp_real, pcepi, FYFSD, .direction = "down") %>%
  filter(!is.na(cons_sent)) %>%
  filter(date >= as.Date("1987-01-01")) %>%
  mutate(
    real_GDP_L1 = lag(gdp_real, 1),
    FYFSD_L1    = lag(FYFSD, 1),
    PCEPI_L0    = pcepi
  ) %>%
  drop_na()

# ==============================================================================
# 3. SCALING (The Fix: Lock Mean/SD to Training Period)
# ==============================================================================
# In the previous version, 'scale()' shifted the baseline as new data arrived.
# Here, we calculate Mean/SD only on data <= TRAIN_END, matching app.R.

cov_vars <- c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")
df_scaled <- df_prep # Start with prep data

# Calculate stats on training set only
train_mask <- df_scaled$date <= TRAIN_END

for (var in cov_vars) {
  # Calc mean/sd from history
  mu <- mean(df_scaled[[var]][train_mask], na.rm = TRUE)
  sd <- sd(df_scaled[[var]][train_mask], na.rm = TRUE)
  
  # Apply to ALL data (Historical + New)
  # This ensures 2025 data is judged against the 1987-2024 baseline
  df_scaled[[var]] <- (df_scaled[[var]] - mu) / sd
}

# Convert to base dataframe (depmixS4 requirement)
df_scaled <- as.data.frame(df_scaled)

# ==============================================================================
# 4. MODEL INFERENCE
# ==============================================================================

# Rebuild Model Structure
n_st <- nstates(hmm_mod)
frm  <- as.formula(paste("~", paste(cov_vars, collapse = " + ")))

mod_new <- depmix(
  response = cons_sent ~ 1,
  data = df_scaled,
  nstates = n_st,
  family = gaussian(),
  transition = frm
)

# Inject Parameters
mod_applied <- setpars(mod_new, getpars(hmm_mod))

# Run Posterior (With Fallback)
post_probs <- tryCatch({
  posterior(mod_applied, type = "smoothing")
}, error = function(e) {
  message(paste("Inference Failed:", e$message))
  return(NULL)
})

# ==============================================================================
# 5. GENERATE METRICS & REPORT
# ==============================================================================

if (is.data.frame(post_probs)) {
  
  # Append Results
  df_scaled$state_idx <- post_probs$state
  df_scaled$p_high    <- post_probs[, HI_STATE_IDX + 1]
  df_scaled$regime    <- ifelse(df_scaled$p_high >= 0.5, "High Sentiment", "Low Sentiment")
  
  # Get Latest Data
  latest      <- tail(df_scaled, 1)
  curr_regime <- latest$regime
  curr_prob   <- latest$p_high
  curr_csi    <- tail(df_prep$cons_sent, 1) # Raw CSI
  
  # Calculate Streaks
  runs        <- rle(df_scaled$regime)
  curr_streak <- tail(runs$lengths, 1)
  prev_streak <- if(length(runs$lengths) > 1) tail(runs$lengths, 2)[1] else 0
  
  # Calculate Anomalies (Z-Score Deviation)
  indicators <- cov_vars
  means_by_regime <- df_scaled %>%
    group_by(regime) %>%
    summarise(across(all_of(indicators), \(x) mean(x, na.rm = TRUE)))
  
  mean_curr_vec <- means_by_regime %>% 
    filter(regime == curr_regime) %>% 
    select(all_of(indicators)) %>% 
    as.numeric()
  
  vals_current  <- as.numeric(latest[indicators])
  dist_to_curr  <- abs(vals_current - mean_curr_vec)
  avg_anomaly   <- round(mean(dist_to_curr), 2)
  
  # Expansion Status
  mean_other_vec <- means_by_regime %>% 
    filter(regime != curr_regime) %>% 
    select(all_of(indicators)) %>% 
    as.numeric()
  dist_to_other <- abs(vals_current - mean_other_vec)
  n_leaning_other   <- sum(dist_to_other < dist_to_curr)
  pct_leaning_other <- (n_leaning_other / length(indicators)) * 100

  expansion_status <- if (curr_regime == "Low Sentiment") {
    if (pct_leaning_other > 50) "Recovery Detected" else "Deep Contraction"
  } else {
    if (pct_leaning_other > 50) "Warning Signal" else "Strong Expansion"
  }

  # Construct Prompt
  prompt <- paste0(
    "You are an expert macroeconomic analyst. Interpret these HMM results:\n",
    "- **Regime:** ", curr_regime, " (", round(curr_prob * 100, 1), "% Conf)\n",
    "- **Streak:** ", curr_streak, " Months\n",
    "- **Raw Sentiment:** ", curr_csi, "\n",
    "- **Anomaly Score:** ", avg_anomaly, " std devs\n",
    "- **Status:** ", expansion_status, "\n\n",
    "Write a 1-paragraph executive summary of the current economic narrative."
  )
  
  # Call API
  setAPIKey(Sys.getenv("GEMINI_API_KEY"))
  
  tryCatch({
    analysis <- gemini(prompt)
    write_lines(analysis, "data/ai_analysis.md")
    message("SUCCESS: AI Analysis generated.")
  }, error = function(e) {
    message(paste("API Error:", e$message))
    write_lines("AI Analysis unavailable (API Error).", "data/ai_analysis.md")
  })

} else {
  message("WARNING: Posterior failed (likely scaling mismatch).")
  write_lines("### ⚠️ AI Analysis Unavailable\n\nModel inference failed.", "data/ai_analysis.md")
}
