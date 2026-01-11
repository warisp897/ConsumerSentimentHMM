library(gemini.R)
library(depmixS4)
library(readr)
library(dplyr)
library(tidyr)

print("DEBUG: Starting AI Insight Generator...")

# 1. WORKSPACE SETUP
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()
setwd(workspace)
print(paste("DEBUG: Working Directory:", getwd()))

# 2. LOAD DATA
print("DEBUG: Loading data/fred_raw_wide.csv...")
if (!file.exists("data/fred_raw_wide.csv")) stop("ERROR: fred_raw_wide.csv not found!")

df_raw  <- read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
print(paste("DEBUG: Data loaded. Rows:", nrow(df_raw)))

print("DEBUG: Loading data/m4_monthly_fit.rds...")
if (!file.exists("data/m4_monthly_fit.rds")) stop("ERROR: m4_monthly_fit.rds not found!")
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# 3. IDENTIFY STATE INDEX
print("DEBUG: Identifying High Sentiment State...")
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(nstates(fit)), function(i) {
    getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)
print(paste("DEBUG: High State Index detected:", HI_STATE_IDX))

# 4. PREPROCESS & FEATURE ENGINEER
print("DEBUG: Preprocessing data (lags and fills)...")
df_prep <- df_raw %>%
  arrange(date) %>%
  # FIX: Select ONLY the columns used in the model.
  # This prevents NAs in unused daily columns (like 10yr Treasury) from killing your monthly rows.
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

print(paste("DEBUG: Preprocessing complete. Rows:", nrow(df_prep)))

print(paste("DEBUG: Preprocessing complete. Rows:", nrow(df_prep)))

# 5. SCALE AND FORMAT
print("DEBUG: Scaling data...")
df_scaled <- df_prep %>%
  mutate(across(c(real_GDP_L1, FYFSD_L1, PCEPI_L0, cons_sent), ~scale(.)[,1]))

# --- THE FIX & THE DEBUG CHECK ---
print("DEBUG: Checking Data Class before conversion...")
print(class(df_scaled))

print("DEBUG: converting Tibble to DataFrame (The Fix)...")
df_scaled <- as.data.frame(df_scaled) # <--- THIS IS THE CRITICAL LINE
print("DEBUG: Conversion complete. New Class:")
print(class(df_scaled))

# Check for NAs just in case
na_count <- sum(is.na(df_scaled[, c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")]))
print(paste("DEBUG: NA count in model columns:", na_count))
if (na_count > 0) stop("ERROR: NAs found in scaled data!")

# 6. RUN MODEL
print("DEBUG: Setting up depmix model structure...")
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

print("DEBUG: Injecting parameters and running Posterior...")
mod_applied <- setpars(mod_new, getpars(hmm_mod))

# This is where it crashed before
post_probs  <- posterior(mod_applied, type = "smoothing")
print("DEBUG: Posterior calculation successful!")

df_scaled$state_idx <- post_probs$state
df_scaled$p_high    <- post_probs[, HI_STATE_IDX + 1]

# 7. GENERATE METRICS
print("DEBUG: Calculating regimes and metrics...")
df_scaled <- df_scaled %>%
  mutate(
    regime = ifelse(p_high >= 0.5, "High Sentiment", "Low Sentiment")
  )

latest <- tail(df_scaled, 1)
curr_regime <- latest$regime
curr_prob   <- latest$p_high
curr_csi    <- tail(df_prep$cons_sent, 1)

runs <- rle(df_scaled$regime)
curr_streak <- tail(runs$lengths, 1)
prev_streak <- if(length(runs$lengths) > 1) tail(runs$lengths, 2)[1] else 0

print(paste("DEBUG: Current Regime:", curr_regime, "| Streak:", curr_streak))

# Anomaly Scores
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

vals_current <- as.numeric(latest[indicators])
dist_to_curr  <- abs(vals_current - mean_curr_vec)
dist_to_other <- abs(vals_current - mean_other_vec)

n_aligned   <- sum(dist_to_curr <= 1.0)
pct_aligned <- round((n_aligned / length(indicators)) * 100, 0)
avg_anomaly <- round(mean(dist_to_curr), 2)

n_leaning_other   <- sum(dist_to_other < dist_to_curr)
pct_leaning_other <- (n_leaning_other / length(indicators)) * 100

expansion_status <- if (curr_regime == "Low Sentiment") {
  if (pct_leaning_other > 50) "Recovery Detected (Drivers shifting to Growth)" else "Deep Contraction (Reinforcing Low State)"
} else {
  if (pct_leaning_other > 50) "Warning Signal (Drivers shifting to Volatility)" else "Strong Expansion (Reinforcing High State)"
}

# 8. CALL GEMINI
print("DEBUG: Constructing Prompt...")
prompt <- paste0(
  "You are an expert macroeconomic analyst. Interpret the latest results from our proprietary Hidden Markov Model (HMM) for US Consumer Sentiment.\n\n",
  "### MODEL STATUS\n",
  "- **Current Regime:** ", curr_regime, "\n",
  "- **Regime Probability:** ", round(curr_prob * 100, 1), "%\n",
  "- **Current Streak:** ", curr_streak, " Months\n",
  "- **Previous Regime Length:** ", prev_streak, " Months\n",
  "- **Consumer Sentiment Index (Raw):** ", curr_csi, "\n\n",
  "### DRIVER HEALTH (Z-SCORES)\n",
  "- **Real GDP (Lagged):** Current Z: ", round(latest$real_GDP_L1, 2), " (Baseline: ", round(mean_curr_vec[1], 2), ")\n",
  "- **PCE Inflation:** Current Z: ", round(latest$PCEPI_L0, 2), " (Baseline: ", round(mean_curr_vec[2], 2), ")\n",
  "- **Fed Surplus/Deficit:** Current Z: ", round(latest$FYFSD_L1, 2), " (Baseline: ", round(mean_curr_vec[3], 2), ")\n\n",
  "### DIAGNOSTICS\n",
  "- **Historical Match:** ", pct_aligned, "%\n",
  "- **Avg Anomaly Score:** ", avg_anomaly, "\n",
  "- **Expansion Status:** ", expansion_status, "\n\n",
  "### TASK\n",
  "Write a sophisticated, 1-paragraph executive summary (approx 100 words). Explain the NARRATIVE."
)

print("DEBUG: Calling Gemini API...")
setAPIKey(Sys.getenv("GEMINI_API_KEY"))

tryCatch({
  analysis <- gemini(prompt)
  write_lines(analysis, "data/ai_analysis.md")
  print("SUCCESS: AI Analysis generated and saved to data/ai_analysis.md")
}, error = function(e) {
  print(paste("ERROR: Gemini API Failed:", e$message))
  write_lines("AI Analysis unavailable due to API error.", "data/ai_analysis.md")
})
