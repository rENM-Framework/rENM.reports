#' Assemble a "Centroid Trends" page
#'
#' Builds a single-page PDF combining a suitability map with centroids and a
#' centroid trend summary table.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' This function assembles precomputed visualization outputs into a single
#' standardized reporting page. It uses only package code and standard R
#' functions and does not source external scripts.
#'
#' \strong{Inputs}
#' Required input files:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Trends/suitability/<alpha_code>-Suitability-Trend-With-Centroids.png
#'   <project_dir>/runs/<alpha_code>/Summaries/tables/<alpha_code>-Centroid-Trend-Summary.pdf
#' }
#'
#' \strong{Outputs}
#' Output file:
#' \preformatted{
#' <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Centroid-Trends.pdf
#' }
#'
#' \strong{Logging}
#' Appends a standard eBird-format processing summary to
#' \code{<project_dir>/runs/<alpha_code>/_log.txt} (blank line before dashed
#' separator, no trailing separator).
#'
#' \strong{Layout and scaling}
#' \itemize{
#'   \item Top:    (A) \code{<ALPHA>-Suitability-Trend-With-Centroids.png}
#'   \item Bottom: (B) \code{<ALPHA>-Centroid-Trend-Summary.pdf}
#'   \item Map scaled to \code{map_width_fraction x content width}
#'   \item Table scaled to \code{table_width_fraction x content width}
#'   \item Both preserve aspect ratio and are centered horizontally.
#' }
#'
#' \strong{Methods}
#' Image composition is performed using the magick package. The summary table
#' PDF is rasterized using \code{pdftools::pdf_convert()} at the specified DPI
#' before scaling and placement.
#'
#' \strong{Data requirements}
#' All required input files must exist prior to execution. The function will
#' terminate with an error if any required input is missing.
#'
#' @param alpha_code Character. Species alpha code (case-insensitive).
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param margin_top_in Numeric. Top margin in inches. Default 1.0.
#' @param margin_right_in Numeric. Right margin in inches. Default 1.0.
#' @param margin_bottom_in Numeric. Bottom margin in inches. Default 1.0.
#' @param margin_left_in Numeric. Left margin in inches. Default 1.0.
#' @param dpi Integer. Working DPI for the page canvas and PNG scaling.
#' Default 300.
#' @param table_dpi Integer. Rasterization DPI for the table PDF. Default 600.
#' @param map_width_fraction Numeric. Fraction (0, 1] of content width used
#' for the top map (1.0 = full content width). Default 1.0.
#' @param table_width_fraction Numeric. Fraction (0, 1] of content width used
#' for the table (1.0 = full content width). Default 1.0.
#' @param v_gap_in Numeric. Vertical gap in inches between the map and table.
#' Default 0.25.
#'
#' @return
#' Character. Invisibly returns the output PDF file path.
#'
#' Side effects:
#' \itemize{
#'   \item Writes a PDF file to the project directory.
#'   \item Creates output directories if they do not exist.
#'   \item Appends a processing summary to the run log file.
#' }
#'
#' @importFrom magick image_read image_resize image_info image_blank
#' @importFrom magick image_composite image_write
#' @importFrom pdftools pdf_convert
#'
#' @examples
#' \dontrun{
#' assemble_centroid_trends_page("CASP")
#' }
#'
#' @export
assemble_centroid_trends_page <- function(alpha_code,
                                          page_width_in  = 8.5,
                                          page_height_in = 11,
                                          margin_top_in    = 1.0,
                                          margin_right_in  = 1.75,
                                          margin_bottom_in = 1.0,
                                          margin_left_in   = 1.75,
                                          dpi        = 300,
                                          table_dpi  = 600,
                                          map_width_fraction   = 1.0,
                                          table_width_fraction = 1.0,
                                          v_gap_in   = 0.25) {
  
  # ---- Timing & identifiers -------------------------------------------------
  start_time <- Sys.time()
  code <- toupper(alpha_code)
  message("[assemble_centroid_trends_page] Begin for ", code)
  
  # ---- Dependencies ---------------------------------------------------------
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required. Please install.packages('magick').",
         call. = FALSE)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required. Please install.packages('pdftools').",
         call. = FALSE)
  }
  
  # Clamp width fractions to (0, 1]
  map_width_fraction   <- max(0.05, min(1.0, as.numeric(map_width_fraction)))
  table_width_fraction <- max(0.05, min(1.0, as.numeric(table_width_fraction)))
  
  # ---- Paths ----------------------------------------------------------------
  project_dir <- rENM_project_dir()
  
  runs_dir   <- file.path(project_dir, "runs", code)
  trends_dir <- file.path(runs_dir, "Trends", "suitability")
  tables_dir <- file.path(runs_dir, "Summaries", "tables")
  pages_dir  <- file.path(runs_dir, "Summaries", "pages")
  
  if (!dir.exists(pages_dir)) {
    dir.create(pages_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  file_A  <- file.path(trends_dir, sprintf("%s-Suitability-Trend-With-Centroids.png", code))
  file_B  <- file.path(tables_dir, sprintf("%s-Centroid-Trend-Summary.pdf",     code))
  out_pdf <- file.path(pages_dir,  sprintf("%s-Centroid-Trends.pdf",            code))
  
  # ---- Validate inputs ------------------------------------------------------
  for (f in c(file_A, file_B)) {
    if (!file.exists(f)) {
      stop("Missing input file: ", f, call. = FALSE)
    }
  }
  
  # ---- Page geometry in pixels ---------------------------------------------
  px <- function(inches) as.integer(round(inches * dpi))
  
  page_w_px <- px(page_width_in)
  page_h_px <- px(page_height_in)
  left_px   <- px(margin_left_in)
  right_px  <- px(margin_right_in)
  top_px    <- px(margin_top_in)
  bottom_px <- px(margin_bottom_in)
  
  content_w_px <- page_w_px - left_px - right_px
  content_h_px <- page_h_px - top_px  - bottom_px
  v_gap_px     <- px(v_gap_in)
  
  # ---- Read and scale top map ----------------------------------------------
  message("[assemble_centroid_trends_page] Reading map image...")
  img_map <- magick::image_read(file_A)
  
  # Scale to desired fraction of content width (height adjusts to preserve aspect)
  target_map_w   <- max(1L, floor(content_w_px * map_width_fraction))
  img_map_scaled <- magick::image_resize(img_map, geometry = sprintf("%d", target_map_w))
  
  map_info <- magick::image_info(img_map_scaled)
  map_w <- as.integer(map_info$width[1])
  map_h <- as.integer(map_info$height[1])
  
  # ---- Rasterize table PDF at high DPI -------------------------------------
  message("[assemble_centroid_trends_page] Rasterizing table PDF at ", table_dpi, " DPI...")
  table_png <- pdftools::pdf_convert(
    pdf       = file_B,
    format    = "png",
    dpi       = table_dpi,
    pages     = 1,
    filenames = file.path(tempdir(), "centroid_table_%d.png")
  )
  
  img_table <- magick::image_read(table_png)
  tbl_info  <- magick::image_info(img_table)
  tbl_w     <- as.integer(tbl_info$width[1])
  tbl_h     <- as.integer(tbl_info$height[1])
  
  # ---- Scale table to requested width fraction -----------------------------
  target_tbl_w <- max(1L, floor(content_w_px * table_width_fraction))
  tbl_scale    <- target_tbl_w / tbl_w
  
  new_tbl_w <- target_tbl_w
  new_tbl_h <- max(1L, round(tbl_h * tbl_scale))
  
  img_table_scaled <- magick::image_resize(
    img_table,
    geometry = sprintf("%dx%d", new_tbl_w, new_tbl_h)
  )
  tbl_w <- new_tbl_w
  tbl_h <- new_tbl_h
  
  # ---- Adjust vertical fit if needed ---------------------------------------
  total_needed_h <- map_h + v_gap_px + tbl_h
  
  if (total_needed_h > content_h_px) {
    max_tbl_h <- max(1L, content_h_px - map_h - v_gap_px)
    scale_tbl <- max_tbl_h / tbl_h
    
    adj_tbl_w <- max(1L, round(tbl_w * scale_tbl))
    adj_tbl_h <- max(1L, round(tbl_h * scale_tbl))
    
    img_table_scaled <- magick::image_resize(
      img_table_scaled,
      geometry = sprintf("%dx%d", adj_tbl_w, adj_tbl_h)
    )
    tbl_w <- adj_tbl_w
    tbl_h <- adj_tbl_h
  }
  
  # ---- Compose page --------------------------------------------------------
  message("[assemble_centroid_trends_page] Compositing final page...")
  page_canvas <- magick::image_blank(
    width  = page_w_px,
    height = page_h_px,
    color  = "white"
  )
  
  # Center map horizontally
  map_x <- left_px + floor((content_w_px - map_w) / 2)
  map_y <- top_px
  
  page_canvas <- magick::image_composite(
    page_canvas,
    img_map_scaled,
    offset = sprintf("+%d+%d", map_x, map_y)
  )
  
  # Center table horizontally below map
  tbl_x <- left_px + floor((content_w_px - tbl_w) / 2)
  tbl_y <- top_px + map_h + v_gap_px
  
  page_canvas <- magick::image_composite(
    page_canvas,
    img_table_scaled,
    offset = sprintf("+%d+%d", tbl_x, tbl_y)
  )
  
  # ---- Write output --------------------------------------------------------
  message("[assemble_centroid_trends_page] Writing PDF: ", out_pdf)
  magick::image_write(page_canvas, path = out_pdf, format = "pdf")
  
  # ---- eBird-standard log ---------------------------------------------------
  stop_time   <- Sys.time()
  elapsed_sec <- as.numeric(difftime(stop_time, start_time, units = "secs"))
  
  log_file <- file.path(runs_dir, "_log.txt")
  sep_line <- paste(rep("-", 72), collapse = "")
  
  log_lines <- c(
    "",
    sep_line,
    "Processing summary (assemble_centroid_trends_page)",
    sprintf("Alpha code:       %s", code),
    sprintf("Start time:       %s", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Stop time:        %s", format(stop_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Outputs saved:    %d", 1L),
    sprintf("Output file:      %s", out_pdf),
    sprintf("Total elapsed:    %.1f seconds", elapsed_sec)
  )
  
  cat(paste0(log_lines, collapse = "\n"),
      file = log_file, sep = "\n", append = TRUE)
  
  message("[assemble_centroid_trends_page] Appended processing summary to: ", log_file)
  
  invisible(out_pdf)
}