#' Create a state-level GAP range / hot spot summary table
#'
#' Produces a publication-ready summary table from state-level suitability
#' trend data, exporting to Excel and optionally to image and PDF formats.
#'
#' @details
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
#'   \item Sorts states by Range \% descending and, if \code{top_states} is
#'     set, keeps only the top \code{top_states} rows.
#'   \item Renames columns for presentation.
#'   \item Appends the boundary block, when boundary statistics exist.
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
#' State, Extent Area, Range Area, Range \%, Positive \%,
#' Negative \%, Hot Spot Area, Hot Spot \%.
#'
#' \strong{Boundary block}
#' When
#' \code{<alpha_code>-Suitability-Trend-Boundary-Statistics.csv} is present,
#' written by \code{rENM.analysis::find_boundary_trend_statistics()}, two
#' further rows are appended below the state rows, separated from them by a
#' rule: \code{Range interior} and \code{Buffer ring (250 km)}.
#'
#' These are range-wide figures, not states, and comparing them is the point:
#' a ring more positive than the interior indicates conditions improving where
#' the species would expand into, while a ring less positive indicates the
#' reverse. They are never to be summed with the state rows. Extent Area and
#' Range \% are blank for them, since Range \% is a state's share of the
#' species total range and has no analogue here.
#'
#' The block is omitted when the file is absent, so the table still builds
#' for runs predating that function.
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
#' state, extent_area_state, range_area, range_pct, pos_pct, neg_pct,
#' hotspot_area, hotspot_pct.
#'
#' @param alpha_code Character. Four-letter species code.
#' @param top_states Integer. If provided, restricts the table to the
#'   \code{top_states} states with the highest Range \% (gap range as a
#'   percentage of state area), sorted descending. Default \code{NULL}
#'   includes all states from the input CSV.
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
#' @importFrom openxlsx mergeCells addStyle setRowHeights freezePane setColWidths saveWorkbook
#' @importFrom gt gt tab_header md cols_align fmt_number opt_row_striping
#' @importFrom gt tab_options gtsave px everything
#' @importFrom gt tab_style cell_borders cells_body sub_missing
#'
#' @examples
#' \dontrun{
#' create_suitability_trend_summary_table("CASP")
#'
#' # Restrict to the 12 states with the highest Range %
#' create_suitability_trend_summary_table("CASP", top_states = 12)
#' }
#'
#' @export
create_suitability_trend_summary_table <- function(alpha_code, top_states = 12) {
  # ---- Dependencies ---------------------------------------------------------
  req <- c("readr", "dplyr", "openxlsx", "gt")
  missing_pkgs <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "), ".")
  }
  has_webshot2 <- requireNamespace("webshot2", quietly = TRUE)
  has_pagedown <- requireNamespace("pagedown", quietly = TRUE)
  `%>%` <- dplyr::`%>%`

  if (!is.null(top_states)) {
    if (!is.numeric(top_states) || length(top_states) != 1 || top_states < 1) {
      stop("`top_states` must be a single positive integer or NULL.")
    }
    top_states <- as.integer(top_states)
  }

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
    "state", "extent_area_state", "range_area", "range_pct",
    "pos_pct", "neg_pct", "hotspot_area", "hotspot_pct"
  )
  miss <- setdiff(needed, names(df))
  if (length(miss)) {
    stop("CSV missing expected column(s): ", paste(miss, collapse = ", "))
  }
  df[needed[-1]] <- lapply(df[needed[-1]], function(x) suppressWarnings(as.numeric(x)))

  # Sort by Range % descending; optionally keep only the top N states
  df <- df[order(-df$range_pct), ]
  if (!is.null(top_states)) {
    df <- utils::head(df, top_states)
  }

  # Rename columns for presentation
  colnames(df) <- c(
    "State", "Extent Area", "Range Area", "Range %",
    "Positive %", "Negative %", "Hot Spot Area", "Hot Spot %"
  )

  # ---- Append the boundary block --------------------------------------------
  # Range-wide interior and ring figures, appended after the state rows are
  # sorted and trimmed so they stay at the foot of the table. They answer a
  # different question than the state rows -- what is happening just outside
  # the range -- and must not be read as further states or summed with them.
  # Extent Area and Range % are left blank because neither applies: Range %
  # is a state's share of the species total range.
  #
  # Optional by design: the table still builds for a run predating
  # find_boundary_trend_statistics(), or when it is invoked on its own.
  n_states <- nrow(df)
  bnd_csv  <- file.path(
    project_dir, "runs", code, "Trends", "suitability",
    sprintf("%s-Suitability-Trend-Boundary-Statistics.csv", code)
  )
  if (file.exists(bnd_csv)) {
    bnd <- readr::read_csv(bnd_csv, show_col_types = FALSE)
    lab <- c(interior = "Range interior", ring = "Buffer ring (250 km)")
    bnd <- bnd[match(names(lab), bnd$zone), , drop = FALSE]
    bnd <- bnd[!is.na(bnd$zone), , drop = FALSE]
    if (nrow(bnd)) {
      df <- rbind(df, data.frame(
        "State"         = unname(lab[bnd$zone]),
        "Extent Area"   = NA_real_,
        "Range Area"    = bnd$area_km2,
        "Range %"       = NA_real_,
        "Positive %"    = bnd$pos_pct,
        "Negative %"    = bnd$neg_pct,
        "Hot Spot Area" = bnd$hotspot_area_km2,
        "Hot Spot %"    = bnd$hotspot_pct,
        check.names     = FALSE,
        stringsAsFactors = FALSE
      ))
    }
  }
  n_bnd <- nrow(df) - n_states

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

  area_cols <- c("Extent Area", "Range Area", "Hot Spot Area")
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

  # Set the boundary rows apart from the state rows above them.
  if (n_bnd > 0) {
    openxlsx::addStyle(
      wb, "Summary",
      openxlsx::createStyle(border = "top", borderStyle = "thin",
                            borderColour = "#D3D3D3"),
      rows = start_row + n_states + 1,
      cols = 1:ncol(df), gridExpand = TRUE, stack = TRUE
    )
  }

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
      columns  = c("Extent Area", "Range Area", "Hot Spot Area"),
      decimals = 1,
      use_seps = TRUE
    ) %>%
    gt::fmt_number(
      columns  = c("Range %", "Positive %", "Negative %", "Hot Spot %"),
      decimals = 1
    ) %>%
    # Extent Area and Range % are blank for the boundary rows because neither
    # applies to them; rendered as "NA" they read as an error instead.
    gt::sub_missing(columns = gt::everything(), missing_text = "") %>%
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

  if (n_bnd > 0) {
    gt_tbl <- gt_tbl %>%
      gt::tab_style(
        style     = gt::cell_borders(sides  = "top",
                                     color  = "#D3D3D3",
                                     weight = gt::px(1)),
        locations = gt::cells_body(rows = n_states + 1)
      )
  }

  wrote_png <- FALSE
  wrote_pdf <- FALSE
  if (has_webshot2) {
    .gt_save_with_timeout(gt_tbl, out_png)
    wrote_png <- file.exists(out_png)
  }
  if (has_pagedown) {
    .gt_save_with_timeout(gt_tbl, out_pdf)
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
    "  Rows:    ", nrow(df),
    if (!is.null(top_states)) sprintf(" (top %d by Range %%)", top_states) else "", "\n",
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
