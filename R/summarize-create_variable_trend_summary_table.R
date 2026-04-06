#' Create a variable trend summary table
#'
#' Produces a publication-ready summary table from variable-trend
#' statistics with significance marks (*, **, ***) and bolding by PD.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline 
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' Converts variable-level Bayesian trend statistics into a compact,
#' formatted summary table suitable for reporting and publication.
#'
#' \strong{Inputs}
#' Input file must exist at:
#'
#' \code{<project_dir>/runs/<alpha_code>/Trends/variables/}
#' \code{<alpha_code>-Variable-Contributions-BR-Stats.csv}
#'
#' \strong{Processing steps}
#' \itemize{
#'   \item Reads the variable-trend CSV using \code{readr::read_csv}.
#'   \item Verifies required columns are present.
#'   \item Resolves CI column names:
#'   slope_ci_lower or slope_ci_low, and slope_ci_upper or slope_ci_high.
#'   \item Preserves row order exactly as in the input CSV.
#'   \item Assigns significance marks based on pd_slope:
#'   \itemize{
#'     \item *** if pd_slope >= 95
#'     \item **  if 90 <= pd_slope < 95
#'     \item *   if 85 <= pd_slope < 90
#'   }
#'   \item Appends significance marks to variable names.
#'   \item Converts numeric fields safely.
#'   \item Builds a formatted Excel workbook using \code{openxlsx}.
#'   \item Creates a styled table using \code{gt}.
#'   \item Optionally exports PNG using webshot2 if available.
#'   \item Optionally exports PDF using pagedown if available.
#' }
#'
#' \strong{Outputs}
#' Output files are written to:
#'
#' \code{<project_dir>/runs/<alpha_code>/Summaries/tables}
#'
#' \itemize{
#'   \item Excel (.xlsx)
#'   \item PNG (.png) if webshot2 is available
#'   \item PDF (.pdf) if pagedown is available
#' }
#'
#' \strong{Column mapping}
#' Output columns correspond to input columns:
#' \itemize{
#'   \item Variable <- Variable
#'   \item Points <- n_points
#'   \item Slope <- slope_mean
#'   \item CI Low <- slope_ci_lower or slope_ci_low
#'   \item CI High <- slope_ci_upper or slope_ci_high
#'   \item PD <- pd_slope
#'   \item ROPE % <- rope_slope_pct
#' }
#'
#' \strong{Formatting rules}
#' \itemize{
#'   \item Row order is preserved exactly as in the input CSV.
#'   \item Variables with pd_slope >= 85 are bolded.
#'   \item Significance marks are appended to variable names.
#' }
#'
#' \strong{Log behavior}
#' Appends a processing summary to:
#'
#' \code{<project_dir>/runs/<alpha_code>/_log.txt}
#'
#' using the eBird-standard format:
#' \itemize{
#'   \item One blank line before the 72-dash separator.
#'   \item No ending separator line.
#'   \item Reports all output files written.
#' }
#'
#' \strong{Data requirements}
#' Input CSV must contain:
#' Variable, n_points, slope_mean, pd_slope, rope_slope_pct,
#' and CI columns (slope_ci_lower or slope_ci_low,
#' slope_ci_upper or slope_ci_high).
#'
#' @param alpha_code Character. Species alpha code.
#'
#' @return
#' A named list returned invisibly with the following elements:
#' \itemize{
#'   \item xlsx: Character. Absolute path to Excel output file.
#'   \item png: Character. Absolute path to PNG output or NA if skipped.
#'   \item pdf: Character. Absolute path to PDF output or NA if skipped.
#' }
#'
#' Side effects:
#' \itemize{
#'   \item Writes Excel, PNG, and PDF files to disk.
#'   \item Appends a processing summary block to the run log file.
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr %>% mutate case_when transmute
#' @importFrom openxlsx createWorkbook addWorksheet createStyle writeData
#'   mergeCells addStyle setRowHeights freezePane setColWidths saveWorkbook
#' @importFrom gt gt tab_header md cols_align fmt_number tab_style
#'   cell_text cells_body opt_row_striping tab_options gtsave px
#'
#' @examples
#' \dontrun{
#' create_variable_trend_summary_table("CASP")
#' }
#'
#' @export
create_variable_trend_summary_table <- function(alpha_code) {
  # ---- Dependencies ---------------------------------------------------------
  req <- c("readr", "dplyr", "openxlsx", "gt")
  missing <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing))
    stop("Missing required packages: ", paste(missing, collapse = ", "), ".")
  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)
  has_pagedown <- requireNamespace("pagedown", quietly = TRUE)
  `%>%` <- dplyr::`%>%`

  # ---- Helpers --------------------------------------------------------------
  .fmt_elapsed <- function(elapsed_secs) {
    secs <- as.numeric(elapsed_secs, units = "secs")
    sprintf("%02d:%02d:%02d",
            floor(secs / 3600),
            floor((secs %% 3600) / 60),
            round(secs %% 60))
  }

  .append_log <- function(code, outputs, elapsed_secs, project_dir) {
    runs_dir <- file.path(project_dir, "runs", code)
    log_file <- file.path(runs_dir, "_log.txt")
    if (!dir.exists(runs_dir)) dir.create(runs_dir, recursive = TRUE, showWarnings = FALSE)

    sep_line <- paste0(strrep("-", 72))
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    existing <- unlist(outputs[!is.na(outputs)], use.names = FALSE)
    existing <- existing[file.exists(existing)]
    n_saved  <- length(existing)
    block <- paste0(
      "\n", sep_line, "\n",
      "Processing summary (create_variable_trend_summary_table)\n",
      sprintf("%-16s %s\n", "Timestamp:", ts),
      sprintf("%-16s %s\n", "Alpha code:", code),
      sprintf("%-16s %s\n", "Outputs saved:",
              sprintf("%d file%s", n_saved, ifelse(n_saved == 1, "", "s"))),
      sprintf("%-16s %s\n", "Total elapsed:", .fmt_elapsed(elapsed_secs)),
      "Output files:\n",
      paste0("  - ", existing, collapse = "\n"),
      if (n_saved > 0) "\n" else ""
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
  csv_in  <- file.path(project_dir, "runs", code, "Trends", "variables",
                       sprintf("%s-Variable-Contributions-BR-Stats.csv", code))
  out_dir <- file.path(project_dir, "runs", code, "Summaries", "tables")
  if (!file.exists(csv_in)) stop("Input CSV not found: ", csv_in)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_xlsx <- file.path(out_dir, sprintf("%s-Variable-Trend-Summary.xlsx", code))
  out_png  <- file.path(out_dir, sprintf("%s-Variable-Trend-Summary.png",  code))
  out_pdf  <- file.path(out_dir, sprintf("%s-Variable-Trend-Summary.pdf",  code))

  # ---- Read & prepare -------------------------------------------------------
  df_raw <- readr::read_csv(csv_in, show_col_types = FALSE)
  req_cols <- c("Variable", "n_points", "slope_mean", "pd_slope", "rope_slope_pct")
  miss <- setdiff(req_cols, names(df_raw))
  if (length(miss)) stop("CSV missing expected column(s): ", paste(miss, collapse = ", "))

  ci_low_col  <- if ("slope_ci_lower" %in% names(df_raw)) "slope_ci_lower" else "slope_ci_low"
  ci_high_col <- if ("slope_ci_upper" %in% names(df_raw)) "slope_ci_upper" else "slope_ci_high"
  if (!(ci_low_col %in% names(df_raw)))  stop("Missing slope_ci_lower or slope_ci_low.")
  if (!(ci_high_col %in% names(df_raw))) stop("Missing slope_ci_upper or slope_ci_high.")

  df <- df_raw %>%
    dplyr::mutate(
      sig_mark = dplyr::case_when(
        !is.na(pd_slope) & pd_slope >= 95 ~ "***",
        !is.na(pd_slope) & pd_slope >= 90 ~ "**",
        !is.na(pd_slope) & pd_slope >= 85 ~ "*",
        TRUE ~ ""
      ),
      Variable_out = paste0(Variable, sig_mark)
    ) %>%
    dplyr::transmute(
      Variable = Variable_out,
      Points   = suppressWarnings(as.numeric(.data$n_points)),
      Slope    = suppressWarnings(as.numeric(.data$slope_mean)),
      `CI Low` = suppressWarnings(as.numeric(.data[[ci_low_col]])),
      `CI High`= suppressWarnings(as.numeric(.data[[ci_high_col]])),
      PD       = suppressWarnings(as.numeric(.data$pd_slope)),
      `ROPE %` = suppressWarnings(as.numeric(.data$rope_slope_pct))
    )

  bold_rows <- which(!is.na(df$PD) & df$PD >= 85)

  # ---- Excel workbook -------------------------------------------------------
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Summary", gridLines = FALSE)
  headerStyle <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#5A5A5A",
                                       halign = "right", valign = "center", textDecoration = "bold")
  headerStyleVar <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#5A5A5A",
                                          halign = "left", valign = "center", textDecoration = "bold")
  bodyStyleNum <- openxlsx::createStyle(halign = "right", valign = "center")
  bodyStyleVar <- openxlsx::createStyle(halign = "left", valign = "center")
  zebraStyle   <- openxlsx::createStyle(fgFill = "#F5F5F5")
  boldVarCell  <- openxlsx::createStyle(textDecoration = "bold", halign = "left", valign = "center")

  fmt_points <- openxlsx::createStyle(numFmt = "#,##0", halign = "right")
  fmt_slope  <- openxlsx::createStyle(numFmt = "0.000", halign = "right")
  fmt_ci     <- openxlsx::createStyle(numFmt = "0.000", halign = "right")
  fmt_pd     <- openxlsx::createStyle(numFmt = "0.0", halign = "right")
  fmt_rope   <- openxlsx::createStyle(numFmt = "0.0", halign = "right")
  titleStyle <- openxlsx::createStyle(textDecoration = "bold", halign = "center", fontSize = 13)

  title <- sprintf("%s VARIABLE TREND SUMMARY", code)
  openxlsx::writeData(wb, "Summary", x = title, startCol = 1, startRow = 1)
  openxlsx::mergeCells(wb, "Summary", cols = 1:ncol(df), rows = 1)
  openxlsx::addStyle(wb, "Summary", style = titleStyle, rows = 1, cols = 1)

  start_row <- 3
  openxlsx::writeData(wb, "Summary", df, startRow = start_row, startCol = 1, headerStyle = headerStyle)
  openxlsx::addStyle(wb, "Summary", style = headerStyleVar, rows = start_row, cols = 1, stack = TRUE)

  if (nrow(df) > 1) {
    body_rows <- seq_len(nrow(df)) + start_row
    zebra_rows <- body_rows[(seq_along(body_rows) %% 2) == 0]
    if (length(zebra_rows))
      openxlsx::addStyle(wb, "Summary", style = zebraStyle,
                         rows = zebra_rows, cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE)
  }

  openxlsx::addStyle(wb, "Summary", style = bodyStyleVar,
                     rows = (start_row+1):(start_row+nrow(df)), cols = 1, gridExpand = TRUE)
  openxlsx::addStyle(wb, "Summary", style = bodyStyleNum,
                     rows = (start_row+1):(start_row+nrow(df)), cols = 2:ncol(df), gridExpand = TRUE)

  rng_rows <- (start_row+1):(start_row+nrow(df))
  idx <- function(nm) match(nm, names(df))
  if (!is.na(idx("Points")))  openxlsx::addStyle(wb, "Summary", fmt_points, rows = rng_rows, cols = idx("Points"), gridExpand = TRUE, stack = TRUE)
  if (!is.na(idx("Slope")))   openxlsx::addStyle(wb, "Summary", fmt_slope,  rows = rng_rows, cols = idx("Slope"), gridExpand = TRUE, stack = TRUE)
  if (!is.na(idx("CI Low")))  openxlsx::addStyle(wb, "Summary", fmt_ci,     rows = rng_rows, cols = idx("CI Low"), gridExpand = TRUE, stack = TRUE)
  if (!is.na(idx("CI High"))) openxlsx::addStyle(wb, "Summary", fmt_ci,     rows = rng_rows, cols = idx("CI High"), gridExpand = TRUE, stack = TRUE)
  if (!is.na(idx("PD")))      openxlsx::addStyle(wb, "Summary", fmt_pd,     rows = rng_rows, cols = idx("PD"), gridExpand = TRUE, stack = TRUE)
  if (!is.na(idx("ROPE %")))  openxlsx::addStyle(wb, "Summary", fmt_rope,   rows = rng_rows, cols = idx("ROPE %"), gridExpand = TRUE, stack = TRUE)

  if (length(bold_rows))
    openxlsx::addStyle(wb, "Summary", style = boldVarCell,
                       rows = start_row + bold_rows, cols = 1, gridExpand = FALSE, stack = TRUE)

  openxlsx::setRowHeights(wb, "Summary", rows = 1, heights = 20)
  openxlsx::setRowHeights(wb, "Summary", rows = (start_row):(start_row+nrow(df)+1), heights = 14)
  openxlsx::freezePane(wb, "Summary", firstRow = TRUE, firstActiveRow = start_row + 1)
  openxlsx::setColWidths(wb, "Summary", cols = 1:ncol(df), widths = "auto")
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  # ---- GT table (PNG + PDF) ------------------------------------------------
  gt_tbl <- df %>%
    gt::gt() %>%
    gt::tab_header(title = gt::md(sprintf("**%s VARIABLE TREND SUMMARY**", code))) %>%
    gt::cols_align("left",  columns = Variable) %>%
    gt::cols_align("right", columns = -Variable) %>%
    gt::fmt_number(columns = Points, decimals = 0, use_seps = TRUE) %>%
    gt::fmt_number(columns = c(Slope, `CI Low`, `CI High`), decimals = 3) %>%
    gt::fmt_number(columns = PD, decimals = 1) %>%
    gt::fmt_number(columns = `ROPE %`, decimals = 1) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(columns = Variable, rows = !is.na(PD) & PD >= 85)
    ) %>%
    gt::opt_row_striping() %>%
    gt::tab_options(
      table.font.size = gt::px(8),
      data_row.padding = gt::px(1),
      heading.title.font.size = gt::px(10),
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
      table_body.hlines.width = gt::px(0),
      table_body.vlines.width = gt::px(0)
    )

  wrote_png <- FALSE; wrote_pdf <- FALSE
  if (has_webshot2) { gt::gtsave(gt_tbl, out_png);  wrote_png <- file.exists(out_png) }
  if (has_pagedown) { gt::gtsave(gt_tbl, out_pdf); wrote_pdf <- file.exists(out_pdf) }

  # ---- Console summary + log -----------------------------------------------
  elapsed <- difftime(Sys.time(), t0, units = "secs")
  cat("Variable trend summary written for ", code, "\n",
      "  Input:   ", csv_in, "\n",
      "  Outputs: ", out_xlsx, "\n",
      "            ", if (wrote_png) out_png else "(PNG skipped)", "\n",
      "            ", if (wrote_pdf) out_pdf else "(PDF skipped)", "\n",
      "  Rows:    ", nrow(df), "\n",
      "  Elapsed: ", .fmt_elapsed(elapsed), "\n", sep = "")

  outputs <- list(
    xlsx = out_xlsx,
    png  = if (wrote_png) out_png else NA_character_,
    pdf  = if (wrote_pdf) out_pdf else NA_character_
  )
  .append_log(code, outputs, elapsed, project_dir)

  invisible(outputs)
}
