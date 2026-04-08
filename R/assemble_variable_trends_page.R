#' Assemble a "Variable Trends" page
#'
#' Builds a single-page PDF that vertically stacks a variable-contribution
#' lines plot above a summary table with proportional scaling and centered
#' alignment.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' This function composes precomputed variable-trend visualizations into a
#' single standardized reporting page. All operations are performed using
#' package-based functionality without sourcing external scripts.
#'
#' \strong{Inputs}
#' Required input files:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Trends/variables/<alpha_code>-Variable-Contributions-BR-Lines-NoRibbon.png
#'   <project_dir>/runs/<alpha_code>/Summaries/tables/<alpha_code>-Variable-Trend-Summary.pdf
#'   <project_dir>/runs/<alpha_code>/Summaries/tables/<alpha_code>-Variable-Trend-Summary.png
#' }
#'
#' \strong{Outputs}
#' Output file:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Variable-Trends.pdf
#' }
#'
#' \strong{Layout and scaling}
#' \itemize{
#'   \item Top: variable-contribution lines plot
#'   \item Bottom: summary table
#'   \item Plot is placed below the top margin
#'   \item Table is placed exactly \code{v_gap_in} inches below the plot
#'   \item Both elements are scaled proportionally and centered horizontally
#'   \item No cropping is performed; only scaling
#' }
#'
#' \strong{Table rendering}
#' \itemize{
#'   \item By default, the table is read from PDF and rasterized at high DPI
#'         using \code{magick::image_read_pdf()} for crisp text
#'   \item If PDF is unavailable or \code{prefer_table_pdf = FALSE}, a PNG is
#'         used instead
#'   \item PNG tables may be supersampled using
#'         \code{table_png_supersample} (for example, 2 = 200 percent)
#'   \item Optional sharpening can be applied using \code{table_sharpen}
#' }
#'
#' \strong{Logging}
#' The function prints progress messages and appends an eBird-style processing
#' summary to \code{<project_dir>/runs/<alpha_code>/_log.txt} with start and
#' stop times and elapsed seconds. The log entry includes a blank line before
#' the separator and no trailing separator after the entry.
#'
#' \strong{Methods}
#' All image reading, trimming, scaling, and composition is handled using the
#' magick package. Table rasterization from PDF uses
#' \code{magick::image_read_pdf()}.
#'
#' \strong{Data requirements}
#' Required input files must exist prior to execution. The function will stop
#' with an error if required inputs are missing.
#'
#' @param alpha_code Character. Species code (case-insensitive).
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param dpi Numeric. DPI for the output page raster composition.
#' Default 300.
#' @param margin_left_in Numeric. Left margin in inches. Default 0.75.
#' @param margin_right_in Numeric. Right margin in inches. Default 0.75.
#' @param margin_bottom_in Numeric. Bottom margin in inches. Default 0.75.
#' @param margin_top_in Numeric. Top margin in inches. Default 1.00.
#' @param v_gap_in Numeric. Visible gap in inches between plot and table.
#' Default 0.50.
#' @param plot_width_factor Numeric. Fraction (0, 1] of content width used
#' by the plot. Default 0.85.
#' @param table_width_factor Numeric. Fraction (0, 1] of content width used
#' by the table. Default 0.50.
#' @param table_max_height_in Numeric. Maximum table height in inches.
#' Default 3.00.
#' @param prefer_table_pdf Logical. If TRUE and a PDF exists, read the table
#' using \code{magick::image_read_pdf()}. Default TRUE.
#' @param table_pdf_dpi Integer. DPI when rasterizing the table PDF.
#' Default 600.
#' @param table_png_supersample Numeric. Greater than or equal to 1; upscale
#' PNG tables by this factor before fitting (for example, 2 = 200 percent).
#' Default 1.5.
#' @param table_sharpen Logical. If TRUE, apply mild sharpening after scaling.
#' Default TRUE.
#' @param trim_plot Logical. If TRUE, trim borders from the plot image.
#' Default TRUE.
#' @param trim_table Logical. If TRUE, trim borders from the table image.
#' Default TRUE.
#' @param trim_fuzz Integer. Tolerance (0 to 100 percent) for trimming.
#' Default 8.
#' @param append_version_to_filename Logical. If TRUE, append a timestamp to
#' the output filename to avoid preview caching. Default FALSE.
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
#' @importFrom magick image_read image_read_pdf image_trim image_scale
#' @importFrom magick image_resize image_info image_blank image_composite
#' @importFrom magick image_write
#'
#' @examples
#' \dontrun{
#' assemble_variable_trends_page("CASP")
#' }
#'
#' @export
assemble_variable_trends_page <- function(alpha_code,
                                          page_width_in = 8.5,
                                          page_height_in = 11,
                                          dpi = 300,
                                          margin_left_in = 0.75,
                                          margin_right_in = 0.75,
                                          margin_bottom_in = 0.75,
                                          margin_top_in = 1.00,
                                          v_gap_in = 0.50,
                                          plot_width_factor = 0.85,
                                          table_width_factor = 0.50,
                                          table_max_height_in = 3.00,
                                          prefer_table_pdf = TRUE,
                                          table_pdf_dpi = 600,
                                          table_png_supersample = 1.5,
                                          table_sharpen = TRUE,
                                          trim_plot = TRUE,
                                          trim_table = TRUE,
                                          trim_fuzz = 8,
                                          append_version_to_filename = FALSE) {
  
  # ---- Start timing & setup ---------------------------------------------------
  start_time <- Sys.time()
  code <- toupper(alpha_code)
  message("[assemble_variable_trends_page] Begin for ", code)
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required. Please install.packages('magick').", call. = FALSE)
  }
  
  plot_width_factor  <- max(0.05, min(1.00, plot_width_factor))
  table_width_factor <- max(0.05, min(1.00, table_width_factor))
  table_png_supersample <- max(1.0, as.numeric(table_png_supersample))
  table_pdf_dpi <- max(72L, as.integer(table_pdf_dpi))
  fuzz <- max(0L, min(100L, as.integer(trim_fuzz)))
  
  # ---- Paths -----------------------------------------------------------------
  project_dir <- rENM_project_dir()
  
  runs_dir <- file.path(project_dir, "runs", code)
  
  in_plot_png <- file.path(
    runs_dir, "Trends", "variables",
    sprintf("%s-Variable-Contributions-BR-Lines-NoRibbon.png", code)
  )
  in_tbl_pdf  <- file.path(
    runs_dir, "Summaries", "tables",
    sprintf("%s-Variable-Trend-Summary.pdf", code)
  )
  in_tbl_png  <- file.path(
    runs_dir, "Summaries", "tables",
    sprintf("%s-Variable-Trend-Summary.png", code)
  )
  
  out_dir <- file.path(runs_dir, "Summaries", "pages")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  base_out <- file.path(out_dir, sprintf("%s-Variable-Trends", code))
  out_pdf  <- paste0(
    base_out,
    if (append_version_to_filename) {
      paste0("-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    } else {
      ""
    },
    ".pdf"
  )
  
  # ---- Validate inputs -------------------------------------------------------
  if (!file.exists(in_plot_png)) {
    stop("Missing required input: ", in_plot_png, call. = FALSE)
  }
  
  if (prefer_table_pdf && !file.exists(in_tbl_pdf) && !file.exists(in_tbl_png)) {
    stop(
      "Table missing: expected ", in_tbl_pdf, " or ", in_tbl_png,
      call. = FALSE
    )
  }
  
  if (prefer_table_pdf && file.exists(in_tbl_pdf) &&
      !requireNamespace("pdftools", quietly = TRUE)) {
    stop(
      "Reading a table PDF requires the 'pdftools' package. ",
      "Install it or set prefer_table_pdf = FALSE.",
      call. = FALSE
    )
  }
  
  # ---- Geometry (inches -> pixels @ dpi) -------------------------------------
  px <- function(inches) as.integer(round(inches * dpi))
  page_w <- px(page_width_in)
  page_h <- px(page_height_in)
  mL <- px(margin_left_in)
  mR <- px(margin_right_in)
  mB <- px(margin_bottom_in)
  mT <- px(margin_top_in)
  vG <- px(v_gap_in)
  tblMaxH <- px(table_max_height_in)
  
  content_x0 <- mL
  content_y0 <- mT
  content_x1 <- page_w - mR
  content_y1 <- page_h - mB
  content_w  <- content_x1 - content_x0
  content_h  <- content_y1 - content_y0
  
  # ---- Read & trim plot ------------------------------------------------------
  message("[assemble_variable_trends_page] Reading plot: ", in_plot_png)
  im_plot <- magick::image_read(in_plot_png)
  if (trim_plot) {
    im_plot <- magick::image_trim(im_plot, fuzz = fuzz)
    message("[assemble_variable_trends_page] Trimmed plot borders")
  }
  
  # ---- Read table (PDF preferred) --------------------------------------------
  use_pdf <- FALSE
  if (prefer_table_pdf && file.exists(in_tbl_pdf)) {
    im_table <- magick::image_read_pdf(in_tbl_pdf, density = table_pdf_dpi)
    use_pdf <- TRUE
    message("[assemble_variable_trends_page] Read table from PDF @ ", table_pdf_dpi, " dpi")
  } else {
    im_table <- magick::image_read(in_tbl_png)
    if (table_png_supersample > 1.0) {
      im_table <- magick::image_scale(
        im_table,
        paste0(round(100 * table_png_supersample), "%")
      )
      message("[assemble_variable_trends_page] Supersampled table PNG x", table_png_supersample)
    }
  }
  
  if (trim_table) {
    im_table <- magick::image_trim(im_table, fuzz = fuzz)
    message("[assemble_variable_trends_page] Trimmed table borders")
  }
  
  # ---- Helpers for fit scaling -----------------------------------------------
  fit_dims <- function(info, box_w, box_h) {
    s <- min(box_w / info$width, box_h / info$height)
    c(
      w = max(1L, floor(info$width  * s)),
      h = max(1L, floor(info$height * s))
    )
  }
  
  fit_image <- function(im, box_w, box_h) {
    d <- fit_dims(magick::image_info(im), box_w, box_h)
    magick::image_resize(im, paste0(d["w"], "x", d["h"], "!"))
  }
  
  # ---- Table size first (width factor + height cap) --------------------------
  tbl_target_w <- floor(content_w * table_width_factor)
  tbl_dims <- fit_dims(magick::image_info(im_table), tbl_target_w, tblMaxH)
  tbl_w <- as.integer(tbl_dims["w"])
  tbl_h <- as.integer(tbl_dims["h"])
  
  im_table_fit <- magick::image_resize(im_table, paste0(tbl_w, "x", tbl_h, "!"))
  
  if (isTRUE(table_sharpen)) {
    # Try to use magick's image_sharpen if it exists and is exported;
    # otherwise fall back to a tiny rescale "nudge".
    if ("image_sharpen" %in% getNamespaceExports("magick")) {
      sharpen_fun <- get("image_sharpen", envir = asNamespace("magick"))
      im_table_fit <- sharpen_fun(im_table_fit, radius = 0.5, sigma = 0.5)
    } else {
      info_now <- magick::image_info(im_table_fit)
      im_table_fit <- magick::image_resize(
        im_table_fit,
        paste0(
          round(info_now$width * 1.01), "x",
          round(info_now$height * 1.01), "!"
        )
      )
    }
  }
  
  # ---- Fit plot; compute layout with exact gap -------------------------------
  plot_target_w <- floor(content_w * plot_width_factor)
  plot_x0 <- content_x0 + floor((content_w - plot_target_w) / 2)
  plot_max_h <- content_h - vG - tbl_h
  im_plot_fit <- fit_image(im_plot, plot_target_w, plot_max_h)
  plot_h <- magick::image_info(im_plot_fit)$height
  
  plot_y0  <- content_y0
  table_y0 <- plot_y0 + plot_h + vG
  table_x0 <- content_x0 + floor((content_w - tbl_w) / 2)
  
  # ---- Compose & write output ------------------------------------------------
  canvas <- magick::image_blank(width = page_w, height = page_h, color = "white")
  canvas <- magick::image_composite(
    canvas,
    im_plot_fit,
    offset = sprintf("+%d+%d", plot_x0, plot_y0)
  )
  canvas <- magick::image_composite(
    canvas,
    im_table_fit,
    offset = sprintf("+%d+%d", table_x0, table_y0)
  )
  
  magick::image_write(canvas, path = out_pdf, format = "pdf")
  message("[assemble_variable_trends_page] Wrote: ", out_pdf)
  
  # ---- Log entry (eBird standard) --------------------------------------------
  stop_time <- Sys.time()
  elapsed_sec <- as.numeric(difftime(stop_time, start_time, units = "secs"))
  log_file <- file.path(runs_dir, "_log.txt")
  sep_line <- paste(rep("-", 72), collapse = "")
  
  log_lines <- c(
    "",  # blank line before separator
    sep_line,
    "Processing summary (assemble_variable_trends_page)",
    sprintf("Alpha code:       %s", code),
    sprintf("Start time:       %s", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Stop time:        %s", format(stop_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Outputs saved:    %d", 1L),
    sprintf("Output file:      %s", out_pdf),
    sprintf("Total elapsed:    %.1f seconds", elapsed_sec)
  )
  
  cat(paste0(log_lines, collapse = "\n"), file = log_file, sep = "\n", append = TRUE)
  message("[assemble_variable_trends_page] Appended processing summary to: ", log_file)
  
  invisible(out_pdf)
}