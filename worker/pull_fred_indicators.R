library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)

workspace <- Sys.getenv("GITHUB_WORKSPACE")
if (workspace == "") workspace <- getwd()

message("------------------------------------------------------------")
message("GITHUB_WORKSPACE: ", workspace)
message("Initial getwd():   ", getwd())

# Force working directory to the repo root
setwd(workspace)

message("Working directory now: ", getwd())

out_dir <- file.path(workspace, "data")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# normalizePath(mustWork=TRUE) fails if directory doesn't exist;
# we just created it, so it's safe.
message("Output dir: ", normalizePath(out_dir, winslash = "/", mustWork = TRUE))

message("Listing repo root:")
print(list.files(".", all.files = TRUE))

message("Listing ./data before pull:")
if (dir.exists("data")) {
  print(list.files("data", all.files = TRUE))
} else {
  message("No ./data dir exists (unexpected).")
}

# ============================================================
# 1) FRED series list
# ============================================================
series <- c(
  # Output
  gdp_nominal         = "GDP",
  gdp_real            = "GDPC1",

  # Labor market
  urate               = "UNRATE",
  part_rate           = "CIVPART",
  initial_claims      = "ICSA",

  # Prices / inflation
  cpi                 = "CPIAUCSL",
  pcepi               = "PCEPI",
  pcepi_robust        = "PCEPILFE",
  ppi                 = "PPIACO",

  # Monetary / financial
  ffr                 = "FEDFUNDS",
  short_treasury      = "TB3MS",
  long_treasury       = "DGS10",
  m2_val              = "M2SL",

  # Fiscal
  fed_budget          = "FYFSD",
  fed_debt            = "GFDEBTN",

  # Housing / construction
  new_private_houses  = "HOUST",
  new_permits         = "PERMIT",
  new_house_sales     = "HSN1F",
  case_schiller       = "CSUSHPINSA",

  # Consumer sentiment
  cons_sent           = "UMCSENT",

  # Trade
  exports             = "EXPGSA",
  imports             = "IMPGSA",
  acc_balance         = "NETFI"
)

# ============================================================
# 2) FRED fetch helpers
# ============================================================
api_key <- Sys.getenv("FRED_API_KEY")
if (api_key == "") {
  stop("FRED_API_KEY is missing. Add it in GitHub repo Settings -> Secrets -> Actions.")
}

fetch_fred_series <- function(series_id, api_key) {
  # Using fred.stlouisfed.org JSON endpoint
  url <- paste0(
    "https://api.stlouisfed.org/fred/series/observations?",
    "series_id=", series_id,
    "&api_key=", api_key,
    "&file_type=json"
  )

  # NOTE: jsonlite can read from URL directly.
  # If you ever see intermittent network issues, you can wrap in tryCatch.
  x <- jsonlite::fromJSON(url)

  if (!"observations" %in% names(x)) {
    stop("No 'observations' in response for series_id=", series_id)
  }

  obs <- x$observations

  # obs$value is character; "." appears for missing.
  df <- tibble::tibble(
    date  = as.Date(obs$date),
    value = suppressWarnings(as.numeric(obs$value))
  )

  df
}

# ============================================================
# 3) Pull all series into a long dataset
# ============================================================
message("------------------------------------------------------------")
message("Starting FRED pull for ", length(series), " series...")

pulled <- vector("list", length(series))
names(pulled) <- names(series)

for (nm in names(series)) {
  id <- series[[nm]]
  message("Pulling ", nm, " (", id, ") ...")

  df <- fetch_fred_series(id, api_key) %>%
    mutate(
      series_name = nm,
      series_id   = id
    )

  # Basic diagnostics
  message("  Rows: ", nrow(df),
          " | Date range: ",
          format(min(df$date, na.rm = TRUE)), " -> ", format(max(df$date, na.rm = TRUE)),
          " | NA values: ", sum(is.na(df$value))
  )

  pulled[[nm]] <- df
}

fred_long <- bind_rows(pulled) %>%
  select(series_name, series_id, date, value) %>%
  arrange(series_name, date)

# ============================================================
# 4) Build a wide dataset (one column per series_name)
# ============================================================
fred_wide <- fred_long %>%
  select(series_name, date, value) %>%
  tidyr::pivot_wider(
    names_from  = series_name,
    values_from = value
  ) %>%
  arrange(date)

# ============================================================
# 5) Write outputs (ABSOLUTE PATHS) + hard verification
# ============================================================
long_path <- file.path(out_dir, "fred_raw_long.csv")
wide_path <- file.path(out_dir, "fred_raw_wide.csv")

message("------------------------------------------------------------")
message("Writing outputs...")

# Use readr for consistent formatting
readr::write_csv(fred_long, long_path, na = "")
readr::write_csv(fred_wide, wide_path, na = "")

# Hard proof (fail fast if something is wrong)
if (!file.exists(long_path)) stop("Expected output missing: ", long_path)
if (!file.exists(wide_path)) stop("Expected output missing: ", wide_path)

long_size <- file.info(long_path)$size
wide_size <- file.info(wide_path)$size

message("Wrote: ", normalizePath(long_path, winslash = "/", mustWork = TRUE))
message("  Size: ", long_size, " bytes | Rows: ", nrow(fred_long))
message("Wrote: ", normalizePath(wide_path, winslash = "/", mustWork = TRUE))
message("  Size: ", wide_size, " bytes | Rows: ", nrow(fred_wide))

# Extra: show a quick preview so logs confirm content
message("------------------------------------------------------------")
message("Preview (head) of long:")
print(utils::head(fred_long, 5))

message("Preview (tail) of long:")
print(utils::tail(fred_long, 5))

message("Preview (tail) of wide (last 3 dates):")
print(utils::tail(fred_wide, 3))

message("------------------------------------------------------------")
message("Listing ./data after pull:")
print(list.files("data", all.files = TRUE))

message("Done.")
message("------------------------------------------------------------")
