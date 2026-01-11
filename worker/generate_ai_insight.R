library(gemini.R)
library(depmixS4)
library(readr)
library(dplyr)
library(tidyr)

print("DEBUG: Starting AI Insight Generator (Robust Version)...")

# 1. WORKSPACE SETUP
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()
setwd(workspace)

# 2. LOAD DATA
print("DEBUG: Loading data...")
if (!file.exists("data/fred_raw_wide.csv")) stop("ERROR: fred_raw_wide.csv missing")
if (!file.exists("data/m4_monthly_fit.rds")) stop("ERROR: m4_monthly_fit.rds missing")

df_raw  <- read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# 3. IDENTIFY HIGH STATE
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(nstates(fit)), function(i) {
    getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)

# 4. PREPROCESS & FEATURE ENGINEER
print("DEBUG: Preprocessing...")
df_prep <- df_raw %>%
  arrange(date) %>%
  # Fix: Select only used columns to avoid NAs from unused daily series
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

print(paste("DEBUG: Rows after cleaning:", nrow(df_prep)))

# 5. SCALE AND FORMAT
# Scale using current history (Approximation of training scale)
df_scaled <- df_prep %>%
  mutate(across(c(real_GDP_L1, FYFSD_L1, PCEPI_L0, cons_sent), ~scale(.)[,1]))

# Fix: Convert Tibble to standard Data Frame for depmixS4
df_scaled <- as.data.frame(df_scaled)

# 6. RUN MODEL INFERENCE
print("DEBUG: Setting up model...")
cov_vars <- c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")
n_st     <- nstates(hmm_mod)
frm      <- as.formula(paste("~", paste(cov_vars, collapse = " + ")))

# Create fresh model structure
mod_new <- depmix(
  response = cons_sent ~ 1,
  data = df_scaled,
  nstates = n_st,
  family = gaussian(),
  transition = frm
)

# Inject trained parameters
print("DEBUG: Injecting parameters...")
# Safety check on parameter length
if (length(getpars(hmm_mod)) != npars(mod_new)) {
  stop(paste("ERROR: Parameter mismatch! Model expects", npars(mod_new), "but loaded", length(getpars(hmm_mod))))
}
mod_applied <- setpars(mod_new, getpars(hmm_mod))

# Safety check on Likelihood
ll <- logLik(mod_applied)
print(paste("DEBUG: Log-Likelihood of new data:", ll))

if (is.na(ll) || ll == -Inf) {
  print("WARNING: Model parameters are invalid for this data (Likelihood is NA/-Inf).")
  # Fallback to avoid crash
  post_probs <- NA
} else {
  print("DEBUG: Calculating Posterior...")
  post_probs <- tryCatch({
    posterior(mod_applied, type = "smoothing")
  }, error = function(e) {
    print(paste("ERROR in posterior():", e$message))
    return(NA)
  })
}

# 7. GENERATE METRICS (If Posterior Succeeded)
if (is.data.frame(post_probs)) {
  print("DEBUG: Posterior success. Computing metrics...")
  
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
  
  # Calculate Anomaly Scores
  indicators <- cov_vars
  means_by_regime <- df_scaled %>%
    group_by(regime) %>%
    summarise(across(all_of(indicators), \(x) mean(x, na.rm = TRUE)))
  
  mean_curr_vec <- means_by_regime %>% 
    filter(regime == curr_regime) %>% 
    select(all_of(indicators)) %>% 
    as.numeric()
  
  vals_current <- as.numeric(latest[indicators])
  dist_to_curr  <- abs(vals_current - mean_curr_vec)
  avg_anomaly <- round(mean(dist_to_curr), 2)
  
  # Prepare Prompt
  prompt <- paste0(
    "You are an expert macroeconomic analyst. Interpret these HMM results:\n",
    "- **Regime:** ", curr_regime, " (", round(curr_prob * 100, 1), "% Conf)\n",
    "- **Streak:** ", curr_streak, " Months\n",
    "- **Raw Sentiment:** ", curr_csi, "\n",
    "- **Anomaly Score:** ", avg_anomaly, " std devs\n\n",
    "Write a 1-paragraph executive summary of the current economic sentiment narrative."
  )
  
  print("DEBUG: Calling Gemini API...")
  setAPIKey(Sys.getenv("GEMINI_API_KEY"))
  
  tryCatch({
    analysis <- gemini(prompt)
    write_lines(analysis, "data/ai_analysis.md")
    print("SUCCESS: AI Report Generated.")
  }, error = function(e) {
    print(paste("API ERROR:", e$message))
    write_lines("AI Analysis unavailable (API Error).", "data/ai_analysis.md")
  })
  
} else {
  print("ERROR: Could not calculate posterior probabilities. Model mismatch likely.")
  write_lines("### ⚠️ AI Analysis Unavailable\n\nThe economic model detected a data anomaly and could not generate a forecast confidence score this month.", "data/ai_analysis.md")
}
