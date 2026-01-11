library(depmixS4)
library(readr)
library(dplyr)
library(tidyr)
library(httr2)
library(jsonlite)

# Set workspace to repo root
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()
setwd(workspace)

# Constants
TRAIN_END  <- as.Date("2024-12-01")
MODEL_NAME <- "gemini-2.5-flash" 

# Helper to write errors
write_error_report <- function(title, details) {
  short_details <- substr(as.character(details), 1, 200)
  msg <- paste0("### AI Analysis Failed\n\n**Reason:** ", title, "\n\n**Details:**\n", short_details)
  readr::write_lines(msg, "data/ai_analysis.md")
  message(paste("FAILED:", title, "-", short_details))
  quit(save = "no", status = 0) 
}

# Verify files
if (!file.exists("data/fred_raw_wide.csv")) write_error_report("Missing File", "fred_raw_wide.csv not found")
if (!file.exists("data/m4_monthly_fit.rds")) write_error_report("Missing Model", "m4_monthly_fit.rds not found")

# Load data
df_raw  <- readr::read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# Identify High Sentiment State
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(depmixS4::nstates(fit)), function(i) {
    depmixS4::getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)

# Clean data 
df_prep <- df_raw %>%
  dplyr::arrange(date) %>%
  dplyr::select(date, gdp_real, pcepi, FYFSD, y = cons_sent) %>%
  tidyr::fill(gdp_real, pcepi, FYFSD, .direction = "down") %>%
  dplyr::filter(!is.na(y)) %>%
  dplyr::filter(date >= as.Date("1987-01-01")) %>%
  dplyr::mutate(
    real_GDP_L1 = dplyr::lag(gdp_real, 1),
    FYFSD_L1    = dplyr::lag(FYFSD, 1),
    PCEPI_L0    = pcepi
  ) %>%
  tidyr::drop_na()

if (nrow(df_prep) < 10) write_error_report("Insufficient Data", paste("Only", nrow(df_prep), "rows remained."))

# Scale variables
cov_vars <- c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")
df_scaled <- df_prep 
train_mask <- df_scaled$date <= TRAIN_END

stats_list <- lapply(cov_vars, function(var) {
  list(
    mu = mean(df_scaled[[var]][train_mask], na.rm = TRUE),
    sd = sd(df_scaled[[var]][train_mask], na.rm = TRUE)
  )
})
names(stats_list) <- cov_vars

for (var in cov_vars) {
  mu <- stats_list[[var]]$mu
  s  <- stats_list[[var]]$sd
  if (is.na(s) || s == 0) s <- 1 
  df_scaled[[var]] <- (df_scaled[[var]] - mu) / s
}

df_scaled <- as.data.frame(df_scaled) 

# Reconstruct model
n_st <- depmixS4::nstates(hmm_mod)
frm  <- as.formula(paste("~", paste(cov_vars, collapse = " + ")))

mod_new <- depmixS4::depmix(
  response = y ~ 1,
  data = df_scaled,
  nstates = n_st,
  family = gaussian(),
  transition = frm
)

mod_applied <- depmixS4::setpars(mod_new, depmixS4::getpars(hmm_mod))

# Inference
post_probs <- tryCatch({
  depmixS4::posterior(mod_applied, type = "smoothing")
}, error = function(e) {
  return(e$message) 
})

if (is.character(post_probs)) write_error_report("Inference Crashed", post_probs)
post_probs <- as.data.frame(post_probs)

# --- METRICS & REPORTING ---

df_scaled$state_idx <- post_probs$state
df_scaled$p_high    <- post_probs[, HI_STATE_IDX + 1]
df_scaled$regime    <- ifelse(df_scaled$p_high >= 0.5, "Low Sentiment", "High Sentiment")

latest      <- utils::tail(df_scaled, 1)
curr_regime <- latest$regime
curr_prob   <- if(curr_regime == "Low Sentiment") latest$p_high else (1 - latest$p_high)
curr_csi    <- utils::tail(df_prep$y, 1)

# Anomaly Scores
indicators <- cov_vars
means_by_regime <- df_scaled %>%
  dplyr::group_by(regime) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(indicators), \(x) mean(x, na.rm = TRUE)))

mean_curr_vec <- means_by_regime %>% 
  dplyr::filter(regime == curr_regime) %>% 
  dplyr::select(dplyr::all_of(indicators)) %>% 
  as.numeric()

vals_current  <- as.numeric(latest[indicators])
diffs <- vals_current - mean_curr_vec
avg_anomaly   <- round(mean(abs(diffs)), 2)
runs          <- rle(df_scaled$regime)
curr_streak   <- utils::tail(runs$lengths, 1)

# Map names to readable labels
driver_map <- c("real_GDP_L1" = "GDP Growth", "PCEPI_L0" = "Inflation (PCE)", "FYFSD_L1" = "Fiscal Deficit")

# Construct "Evidence Block"
evidence <- paste(vapply(seq_along(diffs), function(i) {
  val <- diffs[i]
  direction <- if(val > 0) "above" else "below"
  paste0("- ", driver_map[indicators[i]], ": ", abs(round(val, 2)), " SD ", direction, " baseline")
}, character(1)), collapse = "\n")

# --- FINAL TUNED PROMPT ---
prompt <- paste0(
  "You are a Senior Portfolio Manager at a macro hedge fund. Provide a strategic assessment of the current US Consumer Sentiment Regime.\n\n",
  
  "### MARKET DATA\n",
  "- **Current Regime:** ", curr_regime, " (", round(curr_prob * 100, 1), "% Confidence)\n",
  "- **Duration:** ", curr_streak, " months\n",
  "- **Anomaly Score:** ", avg_anomaly, " deviations from norm\n\n",
  
  "### DRIVER DEVIATIONS (vs Regime Baseline)\n",
  evidence, "\n\n",
  
  "### TASK\n",
  "Write a concise 3-4 sentence thesis. Connect the dots between the drivers.\n",
  "1. Acknowledge the regime status.\n",
  "2. Analyze the conflict: specifically weigh the impact of GDP vs Inflation/Deficit.\n",
  "3. Conclude: Is the regime stabilizing or fragile? (e.g. 'Despite strong growth, inflation remains the structural cap on sentiment.')\n",
  "**Tone:** Professional, analytical, decisive. No flowery adjectives."
)

# API Call
api_key <- Sys.getenv("GEMINI_API_KEY")
url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/", MODEL_NAME, ":generateContent?key=", api_key)

tryCatch({
  resp <- httr2::request(url) %>%
    httr2::req_headers("Content-Type" = "application/json") %>%
    httr2::req_body_json(list(contents = list(list(parts = list(list(text = prompt)))))) %>%
    httr2::req_perform()
  
  result <- resp %>% httr2::resp_body_json()
  text_out <- result$candidates[[1]]$content$parts[[1]]$text
  readr::write_lines(text_out, "data/ai_analysis.md")
  message("SUCCESS: AI Analysis generated.")
  
}, error = function(e) {
  write_error_report("API Error", e$message)
})
