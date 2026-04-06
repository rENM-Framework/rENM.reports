#' Create a centroid shift summary table
#'
#' Description
#'
#' Build a two-part, publication-ready centroid summary for a species
#' alpha_code. The output combines a shift block and regression block
#' into a standardized tabular format for reporting centroid trends.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline 
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' Builds a standardized centroid trend summary from precomputed
#' centroid velocity and regression outputs for a given species.
#'
#' \strong{Inputs}
#' Reads CSV files from:
#' \itemize{
#'   \item \code{<project_dir>/runs/<ALPHA>/Trends/centroids/}
#' }
#'
#' Expected inputs:
#' \describe{
#'   \item{Bioclimatic-Velocity}{
#'     \code{<project_dir>/runs/<ALPHA>/Trends/centroids/}
#'     \code{<ALPHA>-Bioclimatic-Velocity.csv}
#'     with columns \code{start_lon}, \code{start_lat},
#'     \code{end_lon}, \code{end_lat}, \code{distance_km},
#'     \code{bearing_deg}, \code{velocity_km_per_yr}.
#'   }
#'   \item{Longitude-Summary}{
#'     \code{<project_dir>/runs/<ALPHA>/Trends/centroids/}
#'     \code{<ALPHA>-Centroids-Longitude-Summary.csv}
#'     with columns \code{mean}, \code{ci_low}, \code{ci_high},
#'     \code{pd}, \code{rope_pct}.
#'   }
#'   \item{Latitude-Summary}{
#'     \code{<project_dir>/runs/<ALPHA>/Trends/centroids/}
#'     \code{<ALPHA>-Centroids-Latitude-Summary.csv}
#'     with the same columns as Longitude-Summary.
#'   }
#' }
#'
#' \strong{Outputs}
#' Writes outputs to:
#' \itemize{
#'   \item \code{<project_dir>/runs/<ALPHA>/Summaries/tables/}
#'         \code{<ALPHA>-Centroid-Trend-Summary.xlsx}
#'         (always written)
#'   \item \code{<project_dir>/runs/<ALPHA>/Summaries/tables/}
#'         \code{<ALPHA>-Centroid-Trend-Summary.png}
#'         (if optional graphics packages are available)
#'   \item \code{<project_dir>/runs/<ALPHA>/Summaries/tables/}
#'         \code{<ALPHA>-Centroid-Trend-Summary.pdf}
#'         (if optional graphics packages are available)
#' }
#'
#' \strong{Table structure}
#' \itemize{
#'   \item Shift block (one row): 1980/2020 centroid coordinates,
#'         great-circle distance (km), bearing (deg),
#'         velocity (km/yr).
#'   \item Regression block (two rows: Longitude, Latitude):
#'         Slope, CI Low, CI High, PD, Rope \%.
#' }
#'
#' \strong{Visual and typographic specification}
#' \itemize{
#'   \item Title centered and bold; default 10 pt (configurable).
#'   \item Table text 8 pt (Arial default).
#'   \item No top border or rule on any block.
#'   \item No vertical lines anywhere.
#'   \item No horizontal rules between body rows in the regression block.
#'   \item Column-label rows have a light-gray ("#BFBFBF") underline;
#'         each block capped with a light-gray bottom rule.
#'   \item PNG/PDF export uses \pkg{gt} plus \pkg{webshot2} or
#'         \pkg{chromote}, with both top and bottom tables centered
#'         using \code{table.width} and \code{table.align} and canvas
#'         padding via \pkg{magick}.
#'   \item PNG/PDF use 1 px data-row padding.
#' }
#'
#' \strong{Logging}
#' Prints progress to the console and appends an eBird-style
#' processing summary (72-dash ruler; Timestamp, Alpha code,
#' Outputs saved, Output file, Total elapsed) to:
#' \code{<project_dir>/runs/<ALPHA>/}
#' \code{_log.txt}.
#'
#' @param alpha_code Character. Species alpha code (case-insensitive).
#' @param decimals Integer. Number of decimals to display and format.
#' @param font_name Character. Table font family.
#' @param title_size Integer. Title font size in Excel and PNG/PDF.
#' @param target_px Integer. Target pixel width used to center PNG tables.
#'
#' @return
#' Invisibly returns a List with:
#' \itemize{
#'   \item \code{shift_df}: Data frame containing the one-row shift block.
#'   \item \code{reg_df}: Data frame containing the two-row regression block.
#'   \item \code{files}: Character vector of file paths written.
#' }
#' Side effects:
#' \itemize{
#'   \item Writes Excel, PNG, and PDF files to the project directory.
#'   \item Appends a processing summary to the run-specific log file.
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr %>%
#' @importFrom openxlsx createWorkbook addWorksheet writeData mergeCells
#' @importFrom openxlsx createStyle addStyle setRowHeights setColWidths
#' @importFrom openxlsx saveWorkbook
#' @importFrom gt gt tab_header opt_table_font tab_style fmt_number
#' @importFrom gt cols_align tab_options gtsave everything px google_font
#' @importFrom gt default_fonts cells_title cell_text
#' @importFrom magick image_read image_info image_extent image_blank
#' @importFrom magick image_append image_write
#' @importFrom grDevices pdf dev.off as.raster
#' @importFrom grid grid.newpage grid.raster unit
#'
#' @examples
#' \dontrun{
#' create_centroid_trend_summary_table("CASP")
#' create_centroid_trend_summary_table("GRRO",
#'   title_size = 11, font_name = "Helvetica")
#' }
#'
#' @export
create_centroid_trend_summary_table <- function(alpha_code,
                                                decimals = 2,
                                                font_name = "Arial",
                                                title_size = 10,
                                                target_px = 350) {
  
  start_time <- Sys.time()
  
  # ---- Dependencies ---------------------------------------------------------
  for (pkg in c("readr", "dplyr", "openxlsx")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg), call. = FALSE)
    }
  }
  have_gt      <- requireNamespace("gt", quietly = TRUE)
  have_webshot <- requireNamespace("webshot2", quietly = TRUE) ||
    requireNamespace("chromote", quietly = TRUE)
  have_magick  <- requireNamespace("magick", quietly = TRUE)
  
  # ---- Paths ----------------------------------------------------------------
  message("- Resolving paths...")
  code    <- toupper(alpha_code)
  gray    <- "#BFBFBF"
  
  project_dir <- rENM_project_dir()
  runsDir <- file.path(project_dir, "runs", code)
  inDir   <- file.path(runsDir, "Trends", "centroids")
  outDir  <- file.path(runsDir, "Summaries", "tables")
  if (!dir.exists(outDir)) dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  
  f_velocity <- file.path(inDir,  sprintf("%s-Bioclimatic-Velocity.csv",        code))
  f_lon_sum  <- file.path(inDir,  sprintf("%s-Centroids-Longitude-Summary.csv", code))
  f_lat_sum  <- file.path(inDir,  sprintf("%s-Centroids-Latitude-Summary.csv",  code))
  f_xlsx     <- file.path(outDir, sprintf("%s-Centroid-Trend-Summary.xlsx",     code))
  f_png      <- file.path(outDir, sprintf("%s-Centroid-Trend-Summary.png",      code))
  f_pdf      <- file.path(outDir, sprintf("%s-Centroid-Trend-Summary.pdf",      code))

  # ---- Read and validate inputs --------------------------------------------
  message("- Reading inputs...")
  if (!file.exists(f_velocity)) stop("Missing input: ", f_velocity, call. = FALSE)
  vel <- readr::read_csv(f_velocity, show_col_types = FALSE)
  need_vel <- c(
    "start_lon", "start_lat", "end_lon", "end_lat",
    "distance_km", "bearing_deg", "velocity_km_per_yr"
  )
  miss_vel <- setdiff(need_vel, names(vel))
  if (length(miss_vel)) {
    stop(
      "Bioclimatic-Velocity.csv missing: ",
      paste(miss_vel, collapse = ", "),
      call. = FALSE
    )
  }
  if (nrow(vel) < 1) stop("Bioclimatic-Velocity.csv has no rows.", call. = FALSE)
  vel <- vel[1, , drop = TRUE]

  if (!file.exists(f_lon_sum)) stop("Missing input: ", f_lon_sum, call. = FALSE)
  if (!file.exists(f_lat_sum)) stop("Missing input: ", f_lat_sum, call. = FALSE)
  lon <- readr::read_csv(f_lon_sum, show_col_types = FALSE)
  lat <- readr::read_csv(f_lat_sum, show_col_types = FALSE)
  need_trend <- c("mean", "ci_low", "ci_high", "pd", "rope_pct")
  miss_lon <- setdiff(need_trend, names(lon))
  miss_lat <- setdiff(need_trend, names(lat))
  if (length(miss_lon)) {
    stop(
      "Longitude-Summary.csv missing: ",
      paste(miss_lon, collapse = ", "),
      call. = FALSE
    )
  }
  if (length(miss_lat)) {
    stop(
      "Latitude-Summary.csv missing: ",
      paste(miss_lat, collapse = ", "),
      call. = FALSE
    )
  }
  lon <- lon[1, , drop = TRUE]
  lat <- lat[1, , drop = TRUE]

  # ---- Build data frames ----------------------------------------------------
  message("- Building Shift and Regression blocks...")

  shift_df <- data.frame(
    check.names = FALSE,
    "1980 Longitude"   = as.numeric(vel[["start_lon"]]),
    "1980 Latitude"    = as.numeric(vel[["start_lat"]]),
    "2020 Longitude"   = as.numeric(vel[["end_lon"]]),
    "2020 Latitude"    = as.numeric(vel[["end_lat"]]),
    "Distance (km)"    = as.numeric(vel[["distance_km"]]),
    "Bearing (deg)"    = as.numeric(vel[["bearing_deg"]]),
    "Velocity (km/yr)" = as.numeric(vel[["velocity_km_per_yr"]])
  )

  reg_df <- data.frame(
    Regression = c("Longitude", "Latitude"),
    Slope      = c(as.numeric(lon[["mean"]]),    as.numeric(lat[["mean"]])),
    "CI Low"   = c(as.numeric(lon[["ci_low"]]),  as.numeric(lat[["ci_low"]])),
    "CI High"  = c(as.numeric(lon[["ci_high"]]), as.numeric(lat[["ci_high"]])),
    PD         = c(as.numeric(lon[["pd"]]),      as.numeric(lat[["pd"]])),
    "Rope %"   = c(as.numeric(lon[["rope_pct"]]),as.numeric(lat[["rope_pct"]])),
    check.names = FALSE
  )

  # ---- Excel export ---------------------------------------------------------
  message("- Writing Excel: ", basename(f_xlsx))
  wb <- openxlsx::createWorkbook()
  sheet <- "Centroid Summary"
  openxlsx::addWorksheet(wb, sheet)

  title_text <- sprintf("%s CENTROID SHIFT SUMMARY", code)
  openxlsx::writeData(wb, sheet, title_text, startCol = 1, startRow = 1, colNames = FALSE)
  openxlsx::mergeCells(wb, sheet, cols = 1:7, rows = 1)
  titleStyle <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    fontSize = title_size,
    fontName = font_name
  )
  openxlsx::addStyle(wb, sheet, titleStyle, rows = 1, cols = 1:7, gridExpand = TRUE, stack = TRUE)

  gray <- "#BFBFBF"
  hdrStyle <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "Bottom",
    borderStyle = "thin",
    borderColour = gray,
    fontSize = 8,
    fontName = font_name
  )

  openxlsx::writeData(wb, sheet, shift_df, startCol = 1, startRow = 3, headerStyle = hdrStyle)
  num_fmt <- if (decimals > 0) {
    paste0("0.", paste(rep("0", decimals), collapse = ""))
  } else {
    "0"
  }
  shiftNum <- openxlsx::createStyle(
    numFmt = num_fmt,
    halign = "center",
    fontSize = 8,
    fontName = font_name
  )
  openxlsx::addStyle(wb, sheet, shiftNum, rows = 4, cols = 1:7, gridExpand = TRUE)
  sepBottom <- openxlsx::createStyle(
    border = "Bottom",
    borderStyle = "thin",
    borderColour = gray
  )
  openxlsx::addStyle(wb, sheet, sepBottom, rows = 4, cols = 1:7, gridExpand = TRUE)

  regHdr <- openxlsx::createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "Bottom",
    borderStyle = "thin",
    borderColour = gray,
    fontSize = 8,
    fontName = font_name
  )
  openxlsx::writeData(wb, sheet, reg_df, startCol = 1, startRow = 6, headerStyle = regHdr)
  regLabel <- openxlsx::createStyle(
    halign = "left",
    fontSize = 8,
    fontName = font_name
  )
  regNum <- openxlsx::createStyle(
    numFmt = num_fmt,
    halign = "right",
    fontSize = 8,
    fontName = font_name
  )
  openxlsx::addStyle(wb, sheet, regLabel, rows = 7:8, cols = 1, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, regNum,   rows = 7:8, cols = 2:6, gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, sepBottom, rows = 8, cols = 1:6, gridExpand = TRUE)

  openxlsx::setRowHeights(wb, sheet, rows = 1,   heights = 16)
  openxlsx::setRowHeights(wb, sheet, rows = 3:8, heights = 14)
  openxlsx::setColWidths(wb, sheet, cols = 1:7, widths = "auto")
  openxlsx::saveWorkbook(wb, f_xlsx, overwrite = TRUE)

  # ---- PNG/PDF export -------------------------------------------------------
  files_written <- c(f_xlsx)
  if (have_gt && have_webshot && have_magick) {
    message("- Rendering PNG/PDF (centered blocks; robust PDF)...")

    pdf_dpi <- 220

    top_gt <- gt::gt(shift_df) |>
      gt::tab_header(title = title_text) |>
      gt::opt_table_font(font = list(gt::google_font(font_name), gt::default_fonts())) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_title(groups = "title")
      ) |>
      gt::fmt_number(columns = gt::everything(), decimals = decimals) |>
      gt::cols_align(align = "center", columns = gt::everything()) |>
      gt::tab_options(
        table.width = gt::px(target_px),
        table.align = "center",
        table.font.size = gt::px(8),
        data_row.padding = gt::px(1),
        table.border.top.style = "none",
        table.border.bottom.style = "solid",
        table.border.bottom.color = gray,
        table.border.bottom.width = gt::px(1),
        column_labels.border.bottom.style = "solid",
        column_labels.border.bottom.color = gray,
        column_labels.border.bottom.width = gt::px(1)
      )

    bottom_gt <- gt::gt(reg_df) |>
      gt::opt_table_font(font = list(gt::google_font(font_name), gt::default_fonts())) |>
      gt::fmt_number(columns = 2:6, decimals = decimals) |>
      gt::cols_align(align = "left",  columns = 1) |>
      gt::cols_align(align = "right", columns = 2:6) |>
      gt::tab_options(
        table.width = gt::px(target_px),
        table.align = "center",
        table.font.size = gt::px(8),
        data_row.padding = gt::px(1),
        table.border.top.style = "none",
        table.border.bottom.style = "solid",
        table.border.bottom.color = gray,
        table.border.bottom.width = gt::px(1),
        column_labels.border.bottom.style = "solid",
        column_labels.border.bottom.color = gray,
        column_labels.border.bottom.width = gt::px(1)
      )

    tmp_top <- tempfile(fileext = ".png")
    tmp_bot <- tempfile(fileext = ".png")
    gt::gtsave(top_gt, tmp_top, expand = 5)
    gt::gtsave(bottom_gt, tmp_bot, expand = 5)

    img_top <- magick::image_read(tmp_top)
    img_bot <- magick::image_read(tmp_bot)
    w_max   <- max(
      magick::image_info(img_top)$width,
      magick::image_info(img_bot)$width,
      target_px
    )
    h_top <- magick::image_info(img_top)$height
    h_bot <- magick::image_info(img_bot)$height

    img_top <- magick::image_extent(
      img_top,
      geometry = sprintf("%dx%d", w_max, h_top),
      color = "white",
      gravity = "center"
    )
    img_bot <- magick::image_extent(
      img_bot,
      geometry = sprintf("%dx%d", w_max, h_bot),
      color = "white",
      gravity = "center"
    )

    spacer <- magick::image_blank(width = w_max, height = 16, color = "white")
    combo  <- magick::image_append(c(img_top, spacer, img_bot), stack = TRUE)

    magick::image_write(combo, path = f_png, format = "png")

    w_px <- magick::image_info(combo)$width
    h_px <- magick::image_info(combo)$height
    w_in <- w_px / pdf_dpi
    h_in <- h_px / pdf_dpi

    grDevices::pdf(
      f_pdf,
      width = w_in,
      height = h_in,
      onefile = FALSE,
      paper = "special",
      useDingbats = FALSE
    )
    grid::grid.newpage()
    grid::grid.raster(
      grDevices::as.raster(combo),
      width = grid::unit(1, "npc"),
      height = grid::unit(1, "npc"),
      interpolate = FALSE
    )
    grDevices::dev.off()

    files_written <- c(files_written, f_png, f_pdf)

  } else {
    warning(
      "PNG/PDF skipped (need packages: gt and (webshot2 or chromote) and magick). Excel written."
    )
  }

  # ---- eBird-standard log entry --------------------------------------------
  stop_time  <- Sys.time()
  elapsed    <- difftime(stop_time, start_time, units = "secs")
  elapsed_sec <- as.integer(elapsed)
  elapsed_hms <- sprintf(
    "%02d:%02d:%02d",
    elapsed_sec %/% 3600,
    (elapsed_sec %% 3600) %/% 60,
    elapsed_sec %% 60
  )
  sep_line <- paste(rep("-", 72), collapse = "")
  log_file <- file.path(runsDir, "_log.txt")
  log_lines <- c(
    "",
    sep_line,
    "Processing summary (create_centroid_trend_summary_table)",
    sprintf("Timestamp:        %s", format(stop_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Alpha code:       %s", code),
    sprintf("Outputs saved:    %s", paste(basename(files_written), collapse = "; ")),
    sprintf("Output file:      %s", basename(f_xlsx)),
    sprintf("Total elapsed:    %s", elapsed_hms)
  )
  cat(paste0(log_lines, collapse = "\n"), "\n", file = log_file, append = TRUE)

  message("- Done. Files written: ", paste(basename(files_written), collapse = ", "))
  invisible(list(shift_df = shift_df, reg_df = reg_df, files = files_written))
}
