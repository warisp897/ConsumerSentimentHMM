library(fredr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(lubridate)

# ---- force outputs into the checked-out repo ----
workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()

message("GITHUB_WORKSPACE: ", workspace)
setwd(workspace)
message("Working directory now: ", getwd())

out_dir <- file.path(workspace, "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
message("Output dir: ", normalizePath(out_dir, winslash = "/", mustWork = TRUE))


api_key <- Sys.getenv("FRED_API_KEY", unset = "")
if (api_key == "") {
  stop("Missing FRED_API_KEY environment variable. Set it locally or as a GitHub Actions secret.")
}

fredr_set_key(api_key)

start_date <- Sys.getenv("FRED_START", unset = "1980-01-01") |> as.Date()
end_env <- Sys.getenv("FRED_END", unset = "")
end_date <- if (end_env == "") Sys.Date() else as.Date(end_env)

out_dir <- Sys.getenv("OUT_DIR", unset = "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- Series map ----------
series_map <- c(
gdp_nominal = "GDP",
gdp_real = "GDPC1",

urate = "UNRATE",
part_rate = "CIVPART",
initial_claims= "ICSA",

cpi = "CPIAUCSL",
pcepi = "PCEPI",
pcepi_robust= "PCEPILFE",
ppi = "PPIACO",

ffr = "FEDFUNDS",
short_treasury= "TB3MS",
long_treasury = "DGS10",
m2_val= "M2SL",
fed_budget= "FYFSD",
fed_debt= "GFDEBTN",

new_private_houses= "HOUST",
new_permits = "PERMIT",
new_house_sales = "HSN1F",
case_schiller = "CSUSHPINSA",

cons_sent = "UMCSENT",

exports = "EXPGSA",
imports = "IMPGSA",
acc_balance = "NETFI"
)

# ---------- Pull helper ----------
pull_one <- function(var_name, series_id, start_date, end_date) {
  # fredr throws on bad IDs or rate limiting; let it error fast so Actions fails clearly.
  obs <- fredr(
    series_id = series_id,
    observation_start = start_date,
    observation_end   = end_date
  )
  
  # fredr returns columns: date, series_id, value, realtime_start, realtime_end
  # FRED uses "." for missing in some contexts; fredr typically gives numeric with NAs.
  obs %>%
    transmute(
      series_var = var_name,
      series_id  = series_id,
      date       = as.Date(date),
      value      = as.numeric(value)
    )
}

# ---------- Pull all ----------
raw_long <- imap_dfr(series_map, ~pull_one(.y, .x, start_date, end_date))

# Basic sanity checks
if (nrow(raw_long) == 0) stop("No rows returned from FRED. Check API key, series IDs, or date range.")
missing_series <- setdiff(names(series_map), unique(raw_long$series_var))
if (length(missing_series) > 0) {
  stop(paste0("Missing series in pull (unexpected): ", paste(missing_series, collapse = ", ")))
}

# ---------- Write outputs ----------
long_path <- file.path(out_dir, "fred_raw_long.csv")
wide_path <- file.path(out_dir, "fred_raw_wide.csv")

readr::write_csv(fred_long, long_path)
readr::write_csv(fred_wide, wide_path)

# prove it
stopifnot(file.exists(long_path), file.exists(wide_path))
message("Wrote: ", normalizePath(long_path, winslash="/", mustWork=TRUE))
message("Wrote: ", normalizePath(wide_path, winslash="/", mustWork=TRUE))
message("Sizes: ",
        file.info(long_path)$size, " bytes | ",
        file.info(wide_path)$size, " bytes")

# ---------- Print small summary ----------
summary_tbl <- raw_long %>%
  group_by(series_var, series_id) %>%
  summarise(
    n = n(),
    first_date = min(date, na.rm = TRUE),
    last_date  = max(date, na.rm = TRUE),
    na_count   = sum(is.na(value)),
    .groups = "drop"
  ) %>%
  arrange(series_var)

message("Pulled ", nrow(raw_long), " rows across ", n_distinct(raw_long$series_var), " series.")
print(summary_tbl, n = Inf)

message("Wrote: ", raw_long_path)
message("Wrote: ", raw_wide_path)
