#' Create a state-level GAP range / hot spot summary table
#'
#' Produces a publication-ready summary table from state-level suitability
#' trend data, exporting to Excel and optionally to image and PDF formats.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline 
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' Converts state-level suitability trend summary outputs into formatted
#' tables suitable for reporting, publication, and visualization.
#'
#' \strong{Inputs}
#' Input CSV must exist at:
#'
#' \code{<project_dir>/runs/<alpha_code>/Trends/suitability/}
#' \code{<alpha_code>-Suitability-Trend-State-Analysis-Summary.csv}
#'
#' \strong{Processing steps}
#' \itemize{
#'   \item Reads the summary CSV using \code{readr::read_csv}.
#'   \item Verifies required columns are present.
#'   \item Converts numeric columns safely.
#'   \item Renames columns for presentation.
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
#' Table columns are presented as:
#' State, State Area, Range Area, Range \%, Positive \%, Negative \%,
#' Hot Spot Area, Hot Spot \%.
#'
#' \strong{Log behavior}
#' Appends a processing summary to:
#'
#' \code{<project_dir>/runs/<alpha_code>/_log.txt}
#'
#' using the eBird-standard format.
#'
#' \strong{Data requirements}
#' Input CSV must contain:
#' state, state_area, range_area, range_pct, pos_pct, neg_pct,
#' hotspot_area, hotspot_pct.
#'
#' @param alpha_code Character. Four-letter species code.
#'
#' @return
#' A named list returned invisibly with the following elements:
#' \itemize{
#'   \item xlsx: Character. Absolute path to Excel output file.
#'   \item png: Character. Absolute path to PNG file or NA if skipped.
#'   \item pdf: Character. Absolute path to PDF file or NA if skipped.
#' }
#'
#' Side effects:
#' \itemize{
#'   \item Writes formatted Excel, PNG, and PDF outputs.
#'   \item Appends a processing summary block to the run log file.
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr %>%
#' @importFrom openxlsx createWorkbook addWorksheet createStyle writeData
#'   mergeCells addStyle setRowHeights freezePane setColWidths saveWorkbook
#' @importFrom gt gt tab_header md cols_align fmt_number opt_row_striping
#'   tab_options gtsave px everything
#'
#' @examples
#' \dontrun{
#' create_suitability_trend_summary_table("CASP")
#' }
#'
#' @export
create_suitability_trend_summary_table <- function(alpha_code) {
  # ---- Dependencies ---------------------------------------------------------
  req <- c("readr", "dplyr", "openxlsx", "gt")
  missing <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages: ", paste(missing, collapse = ", "), ".")
  }
  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)
  has_pagedown <- requireNamespace("pagedown", quietly = TRUE)
  `%>%` <- dplyr::`%>%`

  # ---- Helpers --------------------------------------------------------------
  .fmt_elapsed <- function(elapsed_secs) {
    secs <- as.numeric(elapsed_secs, units = "secs")
    sprintf(
      "%02d:%02d:%02d",
      floor(secs / 3600),
      floor((secs %% 3600) / 60),
      round(secs %% 60)
    )
  }

  .append_log <- function(code, outputs, elapsed_secs, project_dir) {
    runs_dir <- file.path(project_dir, "runs", code)
    log_file <- file.path(runs_dir, "_log.txt")
    if (!dir.exists(runs_dir)) {
      dir.create(runs_dir, recursive = TRUE, showWarnings = FALSE)
    }

    sep_line <- strrep("-", 72)
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

    existing <- unlist(outputs[!is.na(outputs)], use.names = FALSE)
    existing <- existing[file.exists(existing)]
    n_saved  <- length(existing)

    block <- paste0(
      "\n",
      sep_line, "\n",
      "Processing summary (create_suitability_trend_summary_table)\n",
      sprintf("%-16s %s\n", "Timestamp:", ts),
      sprintf("%-16s %s\n", "Alpha code:", code),
      sprintf(
        "%-16s %s\n",
        "Outputs saved:",
        sprintf("%d file%s", n_saved, ifelse(n_saved == 1, "", "s"))
      ),
      sprintf("%-16s %s\n", "Total elapsed:", .fmt_elapsed(elapsed_secs)),
      "Output files:\n",
      paste0("  - ", existing, collapse = "\n"),
      if (n_saved > 0) "\n" else ""
    )
    cat(block, file = log_file, append = TRUE)
  }

  # ---- Start timer ----------------------------------------------------------
  t0 <- Sys.time()

  # ---- Project directory ----------------------------------------------------
  project_dir <- rENM_project_dir()

  # ---- Paths ---------------------------------------------------------------
  code <- toupper(alpha_code)
  csv_in <- file.path(
    project_dir, "runs", code, "Trends", "suitability",
    sprintf("%s-Suitability-Trend-State-Analysis-Summary.csv", code)
  )
  out_dir <- file.path(project_dir, "runs", code, "Summaries", "tables")
  if (!file.exists(csv_in)) {
    stop("Input CSV not found: ", csv_in)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  out_xlsx <- file.path(out_dir, sprintf("%s-Suitability-Trend-Summary.xlsx", code))
  out_png  <- file.path(out_dir, sprintf("%s-Suitability-Trend-Summary.png",  code))
  out_pdf  <- file.path(out_dir, sprintf("%s-Suitability-Trend-Summary.pdf",  code))

  # ---- Read and prepare -----------------------------------------------------
  df <- readr::read_csv(csv_in, show_col_types = FALSE)
  needed <- c(
    "state", "state_area", "range_area", "range_pct",
    "pos_pct", "neg_pct", "hotspot_area", "hotspot_pct"
  )
  miss <- setdiff(needed, names(df))
  if (length(miss)) {
    stop("CSV missing expected column(s): ", paste(miss, collapse = ", "))
  }
  df[needed[-1]] <- lapply(df[needed[-1]], function(x) suppressWarnings(as.numeric(x)))

  # Rename columns for presentation
  colnames(df) <- c(
    "State", "State Area", "Range Area", "Range %",
    "Positive %", "Negative %", "Hot Spot Area", "Hot Spot %"
  )

  # ---- Excel workbook -------------------------------------------------------
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Summary", gridLines = FALSE)

  headerStyle <- openxlsx::createStyle(
    fontColour = "#FFFFFF",
    fgFill     = "#5A5A5A",
    halign     = "right",
    valign     = "center",
    textDecoration = "bold"
  )
  bodyStyle <- openxlsx::createStyle(
    halign = "right",
    valign = "center"
  )
  zebraStyle <- openxlsx::createStyle(fgFill = "#F5F5F5")
  comma1 <- openxlsx::createStyle(numFmt = "#,##0.0", halign = "right")
  num1   <- openxlsx::createStyle(numFmt = "0.0",     halign = "right")
  titleStyle <- openxlsx::createStyle(
    textDecoration = "bold",
    halign         = "center",
    fontSize       = 13
  )

  title <- sprintf("%s STATE-LEVEL GAP RANGE / HOT SPOT SUMMARY", code)
  openxlsx::writeData(wb, "Summary", title, startCol = 1, startRow = 1)
  openxlsx::mergeCells(wb, "Summary", cols = 1:ncol(df), rows = 1)
  openxlsx::addStyle(wb, "Summary", titleStyle, rows = 1, cols = 1)

  start_row <- 3
  openxlsx::writeData(
    wb, "Summary", df,
    startRow   = start_row,
    startCol   = 1,
    headerStyle = headerStyle
  )

  if (nrow(df) > 1) {
    zebra_rows <- (seq_len(nrow(df)) + start_row)[(seq_len(nrow(df)) %% 2) == 0]
    if (length(zebra_rows)) {
      openxlsx::addStyle(
        wb, "Summary", zebraStyle,
        rows = zebra_rows,
        cols = 1:ncol(df),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  area_cols <- c("State Area", "Range Area", "Hot Spot Area")
  pct_cols  <- c("Range %", "Positive %", "Negative %", "Hot Spot %")
  idx_area <- match(area_cols, names(df))
  idx_pct  <- match(pct_cols,  names(df))

  if (length(idx_area)) {
    openxlsx::addStyle(
      wb, "Summary", comma1,
      rows = (start_row + 1):(start_row + nrow(df)),
      cols = idx_area,
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  if (length(idx_pct)) {
    openxlsx::addStyle(
      wb, "Summary", num1,
      rows = (start_row + 1):(start_row + nrow(df)),
      cols = idx_pct,
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::addStyle(
    wb, "Summary", bodyStyle,
    rows = (start_row + 1):(start_row + nrow(df)),
    cols = 1:ncol(df),
    gridExpand = TRUE
  )
  openxlsx::setRowHeights(wb, "Summary", rows = 1, heights = 20)
  openxlsx::setRowHeights(
    wb, "Summary",
    rows   = start_row:(start_row + nrow(df) + 1),
    heights = 14
  )
  openxlsx::freezePane(wb, "Summary", firstRow = TRUE, firstActiveRow = start_row + 1)
  openxlsx::setColWidths(wb, "Summary", cols = 1:ncol(df), widths = "auto")
  openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

  # ---- GT table (PNG + PDF) ------------------------------------------------
  gt_tbl <- df %>%
    gt::gt() %>%
    gt::tab_header(
      title = gt::md(sprintf("**%s STATE-LEVEL GAP RANGE / HOT SPOT SUMMARY**", code))
    ) %>%
    gt::cols_align("right", columns = gt::everything()) %>%
    gt::fmt_number(
      columns  = c("State Area", "Range Area", "Hot Spot Area"),
      decimals = 1,
      use_seps = TRUE
    ) %>%
    gt::fmt_number(
      columns  = c("Range %", "Positive %", "Negative %", "Hot Spot %"),
      decimals = 1
    ) %>%
    gt::opt_row_striping() %>%
    gt::tab_options(
      table.font.size            = gt::px(8),
      data_row.padding           = gt::px(1),
      table.border.top.width     = gt::px(0),
      table.border.bottom.width  = gt::px(0),
      column_labels.border.top.width    = gt::px(0),
      column_labels.border.bottom.width = gt::px(0),
      table_body.hlines.width    = gt::px(0),
      table_body.vlines.width    = gt::px(0),
      heading.title.font.size    = gt::px(10)
    )

  wrote_png <- FALSE
  wrote_pdf <- FALSE
  if (has_webshot2) {
    gt::gtsave(gt_tbl, out_png)
    wrote_png <- file.exists(out_png)
  }
  if (has_pagedown) {
    gt::gtsave(gt_tbl, out_pdf)
    wrote_pdf <- file.exists(out_pdf)
  }

  # ---- Console + log -------------------------------------------------------
  elapsed <- difftime(Sys.time(), t0, units = "secs")
  cat(
    "Suitability trend summary written for ", code, "\n",
    "  Input:   ", csv_in, "\n",
    "  Outputs: ", out_xlsx, "\n",
    "            ", if (wrote_png) out_png else "(PNG skipped: webshot2 not available)", "\n",
    "            ", if (wrote_pdf) out_pdf else "(PDF skipped: pagedown not available)", "\n",
    "  Rows:    ", nrow(df), "\n",
    "  Elapsed: ", .fmt_elapsed(elapsed), "\n",
    sep = ""
  )

  outputs <- list(
    xlsx = out_xlsx,
    png  = if (wrote_png) out_png else NA_character_,
    pdf  = if (wrote_pdf) out_pdf else NA_character_
  )
  .append_log(code, outputs, elapsed, project_dir)

  invisible(outputs)
}
