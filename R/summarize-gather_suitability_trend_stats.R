#' Merge state-level suitability and hot-spot statistics
#'
#' Reads two per-state CSV files for a species and merges them by
#' two-letter state abbreviation to produce a concise summary table of
#' suitability and hotspot metrics.
#'
#' @details
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
#'   \item Checks that no row reports a hot spot area larger than its range
#'     area, or a range area larger than its extent area.
#' }
#'
#' \strong{Area checks}
#' A hot spot lies within the range, and the range lies within the modeled
#' extent, so neither area can exceed the one containing it. Rows breaking
#' either relation are named in a warning and recorded in the run log; the
#' table is still written, so a single suspect row does not cost the run.
#' The extent comparison allows a 1\% tolerance because a cell-summed area
#' and a vector area measure the same region on different bases.
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
#'   STATE, GAP.RANGE.PCT, GAP.RANGE.POS.PCT,
#'   and either GAPP.RANGE.NEG.PCT or GAP.RANGE.NEG.PCT.
#'   \item Second file must contain:
#'   abbr, state_area_km2, range_area_km2, hotspot_area_km2.
#' }
#'
#' @param alpha_code Character. Four-letter species code.
#'
#' @return
#' Invisibly returns a data.frame with the following columns:
#' \itemize{
#'   \item state: Character. Two-letter state abbreviation.
#'   \item extent_area_state: Numeric. Portion of the state's area falling
#'     within the species' modeled extent (not the state's true area).
#'   \item range_area: Numeric. Area of species range within the state, as
#'     measured on the model grid rather than from the source polygon, so
#'     that it shares a measurement basis with hotspot_area.
#'   \item range_pct: Numeric. Percent of state occupied by range.
#'   \item pos_pct: Numeric. Percent of positive trend area.
#'   \item neg_pct: Numeric. Percent of negative trend area.
#'   \item hotspot_area: Numeric. Area classified as hotspot.
#'   \item hotspot_pct: Numeric. Hotspot area as a percent of range area.
#' }
#'
#' Side effects:
#' \itemize{
#'   \item Writes a summary CSV file to the suitability directory.
#'   \item Appends a processing summary block to the run log file.
#' }
#'
#' @importFrom readr read_csv write_csv
#' @importFrom dplyr arrange left_join tibble mutate if_else %>%
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
  .append_log <- function(code, out_file, elapsed_secs, project_dir, flags = NULL) {
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
      sprintf("%-16s %s\n", "Output file:",   out_file),
      sprintf("%-16s %s\n", "Area checks:",
              if (length(flags)) sprintf("%d FAILED", length(flags)) else "passed"),
      if (length(flags)) paste0("  - ", flags, "\n", collapse = "") else ""
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
  req_main <- c("STATE", "GAP.RANGE.PCT", "GAP.RANGE.POS.PCT")
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

  req_hot <- c("abbr", "state_area_km2", "range_area_km2", "hotspot_area_km2")
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
    state             = merged$STATE,
    extent_area_state = as_num(merged$state_area_km2),
    range_area        = as_num(merged$range_area_km2),
    range_pct         = as_num(merged$`GAP.RANGE.PCT`),
    pos_pct           = as_num(merged$`GAP.RANGE.POS.PCT`),
    neg_pct           = as_num(merged[[neg_col]]),
    hotspot_area      = as_num(merged$hotspot_area_km2)
  ) %>%
    dplyr::mutate(
      # Both areas come from the same coverage-weighted cell sums upstream,
      # so this ratio cannot exceed 100%. Using the vector polygon area
      # (GAP.RANGE.AREA) as the denominator would reintroduce that
      # possibility, since it is measured on a different basis than the
      # hot-spot numerator.
      hotspot_pct = dplyr::if_else(
        .data$range_area > 0,
        100 * .data$hotspot_area / .data$range_area,
        NA_real_
      )
    ) %>%
    dplyr::arrange(.data$state)

  # ---- Check area invariants ----------------------------------------------
  # A hot spot is a subset of range, and range is a subset of the modeled
  # extent, so neither area can exceed the one containing it.
  #
  # The hot-spot test holds by construction: both areas are coverage-weighted
  # sums over the same cells. It is kept as a regression guard because the
  # invariant has broken twice, once when hot spots were masked to the whole
  # state rather than to GAP range, and once when cells straddling the range
  # boundary were counted at their full area.
  #
  # The range test is a real comparison, between a cell sum and a vector
  # area. Those measure the same region on different bases and agree to well
  # under a percent, so the tolerance keeps measurement noise from firing it.
  extent_tol <- 0.01

  bad_hot <- which(out$hotspot_area > out$range_area)
  bad_rng <- which(out$range_area > out$extent_area_state * (1 + extent_tol))

  flags <- c(
    if (length(bad_hot)) sprintf(
      "%s: hot spot area %.3f exceeds range area %.3f",
      out$state[bad_hot], out$hotspot_area[bad_hot], out$range_area[bad_hot]
    ),
    if (length(bad_rng)) sprintf(
      "%s: range area %.3f exceeds extent area %.3f",
      out$state[bad_rng], out$range_area[bad_rng], out$extent_area_state[bad_rng]
    )
  )

  if (length(flags)) {
    warning(
      "Area invariants violated for ", code, ":\n  ",
      paste(flags, collapse = "\n  "),
      call. = FALSE
    )
  }

  # ---- Write CSV ----------------------------------------------------------
  readr::write_csv(out, out_file)
  message("Merged suitability summary written: ", out_file)

  # ---- Log summary --------------------------------------------------------
  elapsed <- difftime(Sys.time(), t0, units = "secs")
  .append_log(code, out_file, elapsed, project_dir, flags)

  invisible(out)
}
