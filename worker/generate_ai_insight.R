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
MODEL_NAME <- "gemini-1.5-flash" 

# Helper to write errors to markdown without crashing workflow
write_error_report <- function(title, details) {
  msg <- paste0("### AI Analysis Failed\n\n**Reason:** ", title, "\n\n**Details:**\n", details)
  readr::write_lines(msg, "data/ai_analysis.md")
  message(paste("FAILED:", title, "-", details))
  quit(save = "no", status = 0) 
}

# Check existence of critical files
if (!file.exists("data/fred_raw_wide.csv")) write_error_report("Missing File", "fred_raw_wide.csv not found")
if (!file.exists("data/m4_monthly_fit.rds")) write_error_report("Missing Model", "m4_monthly_fit.rds not found")

# Load data using explicit readr call
df_raw  <- readr::read_csv("data/fred_raw_wide.csv", show_col_types = FALSE)
hmm_mod <- readRDS("data/m4_monthly_fit.rds")

# Identify the index of the state with the highest mean sentiment
get_hi_state <- function(fit) {
  mus <- vapply(seq_len(depmixS4::nstates(fit)), function(i) {
    depmixS4::getpars(fit@response[[i]][[1]])[1] 
  }, numeric(1))
  which.max(mus)
}
HI_STATE_IDX <- get_hi_state(hmm_mod)

# Clean data using explicit dplyr calls to prevent namespace collisions
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

# Safety check for empty data
if (nrow(df_prep) < 10) write_error_report("Insufficient Data", paste("Only", nrow(df_prep), "rows remained."))

# Scale variables using statistics from training period only
cov_vars <- c("real_GDP_L1", "PCEPI_L0", "FYFSD_L1")
df_scaled <- df_prep 
train_mask <- df_scaled$date <= TRAIN_END

# Calculate Mean and SD based on history
stats_list <- lapply(cov_vars, function(var) {
  list(
    mu = mean(df_scaled[[var]][train_mask], na.rm = TRUE),
    sd = sd(df_scaled[[var]][train_mask], na.rm = TRUE)
  )
})
names(stats_list) <- cov_vars

# Apply scaling to all rows
for (var in cov_vars) {
  mu <- stats_list[[var]]$mu
  s  <- stats_list[[var]]$sd
  if (is.na(s) || s == 0) s <- 1 
  df_scaled[[var]] <- (df_scaled[[var]] - mu) / s
}

# Convert to base dataframe to prevent depmixS4 crashes with tibbles
df_scaled <- as.data.frame(df_scaled) 

# Reconstruct model structure matching the trained object
n_st <- depmixS4::nstates(hmm_mod)
frm  <- as.formula(paste("~", paste(cov_vars, collapse = " + ")))

mod_new <- depmixS4::depmix(
  response = y ~ 1,
  data = df_scaled,
  nstates = n_st,
  family = gaussian(),
  transition = frm
)

# Inject trained parameters into the new container
mod_applied <- depmixS4::setpars(mod_new, depmixS4::getpars(hmm_mod))

# Run posterior decoding with error catching
post_probs <- tryCatch({
  depmixS4::posterior(mod_applied, type = "smoothing")
}, error = function(e) {
  return(e$message)
})

# Check if inference succeeded
if (!is.data.frame(post_probs)) write_error_report("Inference Crashed", paste("Error:", post_probs))

# Calculate metrics for the prompt
df_scaled$state_idx <- post_probs$state
df_scaled$p_high    <- post_probs[, HI_STATE_IDX + 1]

# Logic Swapped as requested: If p_high > 0.5, label as Low (and vice versa)
df_scaled$regime    <- ifelse(df_scaled$p_high >= 0.5, "Low Sentiment", "High Sentiment")

latest      <- utils::tail(df_scaled, 1)
curr_regime <- latest$regime
# We track confidence of the ASSIGNED regime
curr_prob   <- if(curr_regime == "Low Sentiment") latest$p_high else (1 - latest$p_high)
curr_csi    <- utils::tail(df_prep$y, 1)

# Calculate anomaly metrics
indicators <- cov_vars
means_by_regime <- df_scaled %>%
  dplyr::group_by(regime) %>%
  dplyr::summarise(dplyr::across(dplyr::all_of(indicators), \(x) mean(x, na.rm = TRUE)))

mean_curr_vec <- means_by_regime %>% 
  dplyr::filter(regime == curr_regime) %>% 
  dplyr::select(dplyr::all_of(indicators)) %>% 
  as.numeric()

vals_current  <- as.numeric(latest[indicators])
# Anomaly is the distance from the expected mean of the current regime
avg_anomaly   <- round(mean(abs(vals_current - mean_curr_vec)), 2)
runs          <- rle(df_scaled$regime)
curr_streak   <- utils::tail(runs$lengths, 1)

# Construct the enhanced prompt
prompt <- paste0(
  "You are an expert macroeconomic analyst. Interpret these HMM results:\n",
  "- **Regime:** ", curr_regime, " (", round(curr_prob * 100, 1), "% Conf)\n",
  "- **Streak:** ", curr_streak, " Months\n",
  "- **Raw Sentiment:** ", curr_csi, "\n",
  "- **Anomaly Score:** ", avg_anomaly, " std devs (Distance from regime baseline)\n\n",
  "Write a 1-paragraph executive summary of the current economic narrative. ",
  "IMPORTANT: Reference specific drivers if relevant. ",
  "For example, mention if GDP, Inflation, or Fiscal Deficit is deviating from the norm."
)

# Call API using direct httr2 request
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
