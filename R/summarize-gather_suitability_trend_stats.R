#' Merge state-level suitability and hot-spot statistics
#'
#' Reads two per-state CSV files for a species and merges them by
#' two-letter state abbreviation to produce a concise summary table of
#' suitability and hotspot metrics.
#'
#' @details
#' This function is part of the rENM processing pipeline and
#' operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' Combines state-level suitability trend outputs with hotspot statistics
#' into a unified summary table for downstream reporting and analysis.
#'
#' \strong{Inputs}
#' Input files must exist at:
#'
#' \code{<project_dir>/runs/<alpha_code>/Trends/suitability/}
#' \code{<alpha_code>-Suitability-Trend-State-Analysis.csv}
#'
#' \code{<project_dir>/runs/<alpha_code>/Trends/suitability/}
#' \code{<alpha_code>-Suitability-Trend-State-Analysis-Hotspots-Stats.csv}
#'
#' \strong{Processing steps}
#' \itemize{
#'   \item Reads both input CSV files using \code{readr::read_csv}.
#'   \item Verifies required columns are present in each dataset.
#'   \item Sorts both inputs alphabetically by state abbreviation.
#'   \item Merges datasets using a left join on state abbreviation.
#'   \item Converts relevant columns to numeric values.
#'   \item Assembles a standardized summary table with key metrics.
#' }
#'
#' \strong{Outputs}
#' Output file is written to:
#'
#' \code{<project_dir>/runs/<alpha_code>/Trends/suitability/}
#' \code{<alpha_code>-Suitability-Trend-State-Analysis-Summary.csv}
#'
#' \strong{Log behavior}
#' Appends a processing summary to:
#'
#' \code{<project_dir>/runs/<alpha_code>/_log.txt}
#'
#' using the eBird-standard format:
#' \itemize{
#'   \item Always skips one blank line before the 72-dash separator.
#'   \item Does not include an ending separator line.
#'   \item Omits all raster-related fields.
#'   \item Includes: Timestamp, Alpha code, Outputs saved,
#'   Total elapsed, and Output file.
#' }
#'
#' \strong{Data requirements}
#' \itemize{
#'   \item First file must contain:
#'   STATE, GAP.RANGE.AREA, GAP.RANGE.PCT, GAP.RANGE.POS.PCT,
#'   and either GAPP.RANGE.NEG.PCT or GAP.RANGE.NEG.PCT.
#'   \item Second file must contain:
#'   abbr, state_area_km2, hotspot_area_km2, hotspot_pct_of_state.
#' }
#'
#' @param alpha_code Character. Four-letter species code.
#'
#' @return
#' Invisibly returns a data.frame with the following columns:
#' \itemize{
#'   \item state: Character. Two-letter state abbreviation.
#'   \item state_area: Numeric. State area in square kilometers.
#'   \item range_area: Numeric. Area of species range.
#'   \item range_pct: Numeric. Percent of state occupied by range.
#'   \item pos_pct: Numeric. Percent of positive trend area.
#'   \item neg_pct: Numeric. Percent of negative trend area.
#'   \item hotspot_area: Numeric. Area classified as hotspot.
#'   \item hotspot_pct: Numeric. Percent of state as hotspot.
#' }
#'
#' Side effects:
#' \itemize{
#'   \item Writes a summary CSV file to the suitability directory.
#'   \item Appends a processing summary block to the run log file.
#' }
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr arrange left_join tibble %>%
#'
#' @examples
#' \dontrun{
#' gather_suitability_trend_stats("CASP")
#' }
#'
#' @export
gather_suitability_trend_stats <- function(alpha_code) {
  # ---- Dependencies ---------------------------------------------------------
  if (!requireNamespace("readr", quietly = TRUE))
    stop("Package 'readr' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required.")
  `%>%` <- dplyr::`%>%`

  # Helper: format elapsed seconds as HH:MM:SS
  .fmt_elapsed <- function(elapsed_secs) {
    secs <- as.numeric(elapsed_secs, units = "secs")
    hrs  <- floor(secs / 3600)
    mins <- floor((secs %% 3600) / 60)
    sec  <- round(secs %% 60)
    sprintf("%02d:%02d:%02d", hrs, mins, sec)
  }

  # Helper: append concise eBird-standard log block
  .append_log <- function(code, out_file, elapsed_secs, project_dir) {
    runs_dir <- file.path(project_dir, "runs", code)
    log_file <- file.path(runs_dir, "_log.txt")
    if (!dir.exists(runs_dir)) {
      dir.create(runs_dir, recursive = TRUE, showWarnings = FALSE)
    }

    sep_line <- paste0(strrep("-", 72))
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

    block <- paste0(
      "\n",  # always skip one blank line before separator
      sep_line, "\n",
      "Processing summary (gather_suitability_trend_stats)\n",
      sprintf("%-16s %s\n", "Timestamp:",     ts),
      sprintf("%-16s %s\n", "Alpha code:",    code),
      sprintf("%-16s %s\n", "Outputs saved:", "1 file"),
      sprintf("%-16s %s\n", "Total elapsed:", .fmt_elapsed(elapsed_secs)),
      sprintf("%-16s %s\n", "Output file:",   out_file)
    )

    cat(block, file = log_file, append = TRUE)
    invisible(log_file)
  }

  # ---- Start timer ----------------------------------------------------------
  t0 <- Sys.time()

  # ---- Project directory ----------------------------------------------------
  project_dir <- rENM_project_dir()

  # ---- Paths ---------------------------------------------------------------
  code <- toupper(alpha_code)
  base_dir <- file.path(project_dir, "runs", code, "Trends", "suitability")
  main_file <- file.path(base_dir, sprintf("%s-Suitability-Trend-State-Analysis.csv", code))
  hot_file  <- file.path(base_dir, sprintf("%s-Suitability-Trend-State-Analysis-Hotspots-Stats.csv", code))
  out_file  <- file.path(base_dir, sprintf("%s-Suitability-Trend-State-Analysis-Summary.csv", code))

  if (!file.exists(main_file)) stop("Missing main suitability file: ", main_file)
  if (!file.exists(hot_file))  stop("Missing hotspot stats file: ", hot_file)

  # ---- Read ---------------------------------------------------------------
  main <- readr::read_csv(main_file, show_col_types = FALSE)
  hot  <- readr::read_csv(hot_file,  show_col_types = FALSE)

  # ---- Verify required columns -------------------------------------------
  req_main <- c("STATE", "GAP.RANGE.AREA", "GAP.RANGE.PCT", "GAP.RANGE.POS.PCT")
  missing_main <- setdiff(req_main, names(main))
  if (length(missing_main)) {
    stop(
      "First file is missing required column(s): ",
      paste(missing_main, collapse = ", "),
      "\n  File: ",
      main_file
    )
  }

  neg_col <- if ("GAPP.RANGE.NEG.PCT" %in% names(main)) {
    "GAPP.RANGE.NEG.PCT"
  } else if ("GAP.RANGE.NEG.PCT" %in% names(main)) {
    "GAP.RANGE.NEG.PCT"
  } else {
    stop(
      "First file must contain 'GAPP.RANGE.NEG.PCT' (preferred) or 'GAP.RANGE.NEG.PCT'.\n  File: ",
      main_file
    )
  }

  req_hot <- c("abbr", "state_area_km2", "hotspot_area_km2", "hotspot_pct_of_state")
  missing_hot <- setdiff(req_hot, names(hot))
  if (length(missing_hot)) {
    stop(
      "Second file is missing required column(s): ",
      paste(missing_hot, collapse = ", "),
      "\n  File: ",
      hot_file
    )
  }

  # ---- Sort each input alphabetically -------------------------------------
  main <- main %>% dplyr::arrange(.data$STATE)
  hot  <- hot  %>% dplyr::arrange(.data$abbr)

  # ---- Merge --------------------------------------------------------------
  merged <- dplyr::left_join(main, hot, by = c("STATE" = "abbr"))
  as_num <- function(x) suppressWarnings(as.numeric(x))

  out <- dplyr::tibble(
    state        = merged$STATE,
    state_area   = as_num(merged$state_area_km2),
    range_area   = as_num(merged$`GAP.RANGE.AREA`),
    range_pct    = as_num(merged$`GAP.RANGE.PCT`),
    pos_pct      = as_num(merged$`GAP.RANGE.POS.PCT`),
    neg_pct      = as_num(merged[[neg_col]]),
    hotspot_area = as_num(merged$hotspot_area_km2),
    hotspot_pct  = as_num(merged$hotspot_pct_of_state)
  ) %>%
    dplyr::arrange(.data$state)

  # ---- Write CSV ----------------------------------------------------------
  readr::write_csv(out, out_file)
  message("Merged suitability summary written: ", out_file)

  # ---- Log summary --------------------------------------------------------
  elapsed <- difftime(Sys.time(), t0, units = "secs")
  .append_log(code, out_file, elapsed, project_dir)

  invisible(out)
}
