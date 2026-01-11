library(gemini.R)
library(depmixS4)
library(readr)
library(dplyr)
library(tidyr)

# ==============================================================================
# 1. ROBUST WORKSPACE SETUP
# ==============================================================================
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()
setwd(workspace)

# Check for required files
if (!file.exists("data/fred_raw_wide.csv")) stop("ERROR: data/fred_raw_wide.csv not found.")
if (!file.exists("data/m4_monthly_fit.rds")) stop("ERROR: data/m4_monthly_fit.rds not found.")

# ==============================================================================
# 2. DATA LOADING & PREPROCESSING
# ==============================================================================

# Load data and model
df_raw  <- read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# Identify the "High Sentiment" state index
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(nstates(fit)), function(i) {
    getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)

# Clean and Feature Engineer
# Select ONLY used columns to prevent daily NAs from dropping monthly rows
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

# Scale data (Z-scores)
df_scaled <- df_prep %>%
  mutate(across(c(real_GDP_L1, FYFSD_L1, PCEPI_L0, cons_sent), ~scale(.)[,1]))

# CRITICAL FIX: Convert to base data.frame for depmixS4 compatibility
df_scaled <- as.data.frame(df_scaled)

# ==============================================================================
# 3. RUN MODEL INFERENCE (With Error Handling)
# ==============================================================================

# Reconstruct model structure
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

# FIX: Use length(getpars) instead of npars() to avoid "function not found" error
if (length(getpars(hmm_mod)) != length(getpars(mod_new))) {
  stop("Parameter mismatch between trained model and new data structure.")
}

# Inject trained parameters
mod_applied <- setpars(mod_new, getpars(hmm_mod))

# Run Posterior Decoding with Fallback
# This prevents the "$ operator is invalid for atomic vectors" error
post_probs <- tryCatch({
  posterior(mod_applied, type = "smoothing")
}, error = function(e) {
  message(paste("Model Inference Failed:", e$message))
  return(NULL)
})

# ==============================================================================
# 4. GENERATE REPORT (Only if model succeeded)
# ==============================================================================

if (is.data.frame(post_probs)) {
  
  # --- SUCCESS PATH ---
  df_scaled$state_idx <- post_probs$state
  df_scaled$p_high    <- post_probs[, HI_STATE_IDX + 1]
  
  df_scaled$regime <- ifelse(df_scaled$p_high >= 0.5, "High Sentiment", "Low Sentiment")
  
  latest <- tail(df_scaled, 1)
  curr_regime <- latest$regime
  curr_prob   <- latest$p_high
  curr_csi    <- tail(df_prep$cons_sent, 1)
  
  runs <- rle(df_scaled$regime)
  curr_streak <- tail(runs$lengths, 1)
  prev_streak <- if(length(runs$lengths) > 1) tail(runs$lengths, 2)[1] else 0
  
  # Metrics
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
  n_aligned     <- sum(dist_to_curr <= 1.0)
  pct_aligned   <- round((n_aligned / length(indicators)) * 100, 0)
  avg_anomaly   <- round(mean(dist_to_curr), 2)
  
  prompt <- paste0(
    "You are an expert macroeconomic analyst. Interpret these HMM results for Consumer Sentiment:\n",
    "- **Regime:** ", curr_regime, " (", round(curr_prob * 100, 1), "% Conf)\n",
    "- **Streak:** ", curr_streak, " Months\n",
    "- **Raw Sentiment:** ", curr_csi, "\n",
    "- **Anomaly Score:** ", avg_anomaly, " std devs\n\n",
    "Write a 1-paragraph executive summary of the current economic narrative."
  )
  
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
  
  # --- FAILURE PATH ---
  message("WARNING: Could not calculate posterior probabilities. Writing fallback message.")
  write_lines(
    "### ⚠️ AI Analysis Unavailable\n\nThe economic model detected a data anomaly and could not generate a forecast confidence score this month.", 
    "data/ai_analysis.md"
  )
}
