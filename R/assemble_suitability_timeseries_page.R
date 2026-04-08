#' Assemble a "Suitability Time Series" page
#'
#' Builds a single-page PDF that vertically stacks a collection of suitability
#' maps above a shared caption with controlled spacing and proportional scaling.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' For a given species alpha code (\code{alpha_code}), this function builds a
#' single-page PDF that stacks:
#'
#'   1. The suitability contact-sheet map PNG (A) on the top of the page:
#'      \code{<project_dir>/runs/<alpha_code>/Summaries/maps/<alpha_code>-Suitability-Maps.png}
#'   2. A shared suitability time-series caption PDF (B; first page only)
#'      underneath:
#'      \code{system.file("captions",
#'      "suitability_timeseries_caption.pdf",
#'      package = "rENM.reports")}
#'
#' Both elements are:
#' \itemize{
#'   \item Converted to raster images
#'   \item Trimmed to remove uniform white margins via
#'         \code{magick::image_trim()}
#'   \item Scaled so that, when \code{map_width_fraction = 1} and
#'         \code{caption_width_fraction = 1}, the visible width of both (A)
#'         and (B) is \code{target_content_width_in} inches (default 6),
#'         subject to the page interior width
#'   \item Separated by an adjustable vertical gap of
#'         \code{gap_between_in} inches
#'   \item Stacked vertically (map above gap, gap above caption)
#'   \item Placed on a white Letter-sized page (8.5 x 11 inches) with
#'         uniform margins
#' }
#'
#' Trimming the source rasters first ensures that any extra baked-in white
#' margins are removed, so the final 6-inch width corresponds to the graphic
#' itself rather than its original page padding.
#'
#' The function also appends an eBird-standard processing summary to
#' \code{<project_dir>/runs/<alpha_code>/_log.txt}, including the caption
#' source path.
#'
#' \strong{Inputs}
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/maps/<alpha_code>-Suitability-Maps.png
#'   system.file("captions", "suitability_timeseries_caption.pdf",
#'               package = "rENM.reports")
#' }
#'
#' \strong{Outputs}
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Suitability-TimeSeries.pdf
#' }
#'
#' \strong{Methods}
#' All image reading, trimming, scaling, and composition are handled using
#' the magick package. The caption PDF is rasterized using
#' \code{pdftools::pdf_render_page()}.
#'
#' \strong{Data requirements}
#' Required input files must exist prior to execution. The function will stop
#' with an error if the map PNG or caption PDF is missing.
#'
#' @param alpha_code Character. Species alpha code (for example, \code{"CASP"}).
#' Converted internally to uppercase for file naming and paths.
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param dpi Numeric. Target raster resolution in dots per inch for both the
#' caption rendering and the final layout. Default 300.
#' @param margin_in Numeric. Uniform margin on all sides in inches. Default 1.
#' @param target_content_width_in Numeric. Target graphic width in inches for
#' the map and caption when width fractions are 1.0. Default 6.
#' @param map_width_fraction Numeric. Fraction (0, 1] of target width for map.
#' @param caption_width_fraction Numeric. Fraction (0, 1] of target width for
#' caption.
#' @param gap_between_in Numeric. Vertical gap between map and caption in
#' inches.
#' @param trim_fuzz Numeric. Fuzz percentage for
#' \code{magick::image_trim()}.
#' @param verbose Logical. If TRUE, print diagnostic messages.
#'
#' @return
#' Character. Invisibly returns the output PDF file path:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Suitability-TimeSeries.pdf
#' }
#'
#' Side effects:
#' \itemize{
#'   \item Writes a PDF file to the project directory
#'   \item Creates output directories if they do not exist
#'   \item Appends a processing summary to the run log file
#' }
#'
#' @importFrom magick image_read image_info image_scale image_append
#' @importFrom magick image_blank image_composite image_write image_trim
#' @importFrom pdftools pdf_render_page
#'
#' @examples
#' \dontrun{
#' assemble_suitability_timeseries_page("CASP")
#' }
#'
#' @export
assemble_suitability_timeseries_page <- function(alpha_code,
                                                 page_width_in           = 8.5,
                                                 page_height_in          = 11,
                                                 dpi                     = 300,
                                                 margin_in               = 1,
                                                 target_content_width_in = 6,
                                                 map_width_fraction      = 0.85,
                                                 caption_width_fraction  = 0.85,
                                                 gap_between_in          = 0.50,
                                                 trim_fuzz               = 5,
                                                 verbose                 = TRUE) {
  
  # --------------------------------------------------------------------------
  # 0. Basic argument handling and sanity checks
  # --------------------------------------------------------------------------
  
  start_time <- Sys.time()
  code <- toupper(alpha_code)
  
  if (!is.numeric(map_width_fraction) || length(map_width_fraction) != 1L ||
      map_width_fraction <= 0 || map_width_fraction > 1) {
    stop("map_width_fraction must be in (0, 1].")
  }
  
  if (!is.numeric(caption_width_fraction) || length(caption_width_fraction) != 1L ||
      caption_width_fraction <= 0 || caption_width_fraction > 1) {
    stop("caption_width_fraction must be in (0, 1].")
  }
  
  if (!is.numeric(target_content_width_in) || length(target_content_width_in) != 1L ||
      target_content_width_in <= 0) {
    stop("target_content_width_in must be positive.")
  }
  
  if (!is.numeric(gap_between_in) || length(gap_between_in) != 1L ||
      gap_between_in < 0) {
    stop("gap_between_in must be >= 0.")
  }
  
  if (!is.numeric(trim_fuzz) || length(trim_fuzz) != 1L || trim_fuzz < 0) {
    stop("trim_fuzz must be >= 0.")
  }
  
  # --------------------------------------------------------------------------
  # 1. Package checks
  # --------------------------------------------------------------------------
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required.", call. = FALSE)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required.", call. = FALSE)
  }
  
  magick_ns   <- asNamespace("magick")
  pdftools_ns <- asNamespace("pdftools")
  
  # --------------------------------------------------------------------------
  # 2. Construct paths using rENM_project_dir() + system.file()
  # --------------------------------------------------------------------------
  
  project_dir <- rENM_project_dir()
  
  species_dir <- file.path(project_dir, "runs", code)
  
  if (!dir.exists(species_dir)) {
    stop(sprintf("Species directory not found: %s", species_dir), call. = FALSE)
  }
  
  map_path <- file.path(
    species_dir, "Summaries", "maps",
    sprintf("%s-Suitability-Maps.png", code)
  )
  
  caption_pdf_path <- system.file(
    "captions",
    "suitability_timeseries_caption.pdf",
    package = "rENM.reports"
  )
  
  if (caption_pdf_path == "") {
    stop("Caption PDF not found in rENM.reports package.", call. = FALSE)
  }
  
  out_dir <- file.path(species_dir, "Summaries", "pages")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  out_pdf_path <- file.path(
    out_dir,
    sprintf("%s-Suitability-TimeSeries.pdf", code)
  )
  
  if (verbose) {
    message("[assemble] Map: ", map_path)
    message("[assemble] Caption: ", caption_pdf_path)
    message("[assemble] Output: ", out_pdf_path)
  }
  
  if (!file.exists(map_path)) stop("Map PNG not found.")
  if (!file.exists(caption_pdf_path)) stop("Caption PDF not found.")
  
  page_w_px <- round(page_width_in * dpi)
  page_h_px <- round(page_height_in * dpi)
  margin_px <- round(margin_in * dpi)
  
  content_w_px <- page_w_px - 2 * margin_px
  content_h_px <- page_h_px - 2 * margin_px
  
  target_px <- min(round(target_content_width_in * dpi), content_w_px)
  gap_px <- round(gap_between_in * dpi)
  
  map_img <- magick_ns$image_read(map_path)
  cap_raw <- pdftools_ns$pdf_render_page(caption_pdf_path, page = 1, dpi = dpi)
  cap_img <- magick_ns$image_read(cap_raw)
  
  map_trimmed <- magick_ns$image_trim(map_img, fuzz = trim_fuzz)
  cap_trimmed <- magick_ns$image_trim(cap_img, fuzz = trim_fuzz)
  
  map_scaled <- magick_ns$image_scale(map_trimmed, as.character(round(target_px * map_width_fraction)))
  cap_scaled <- magick_ns$image_scale(cap_trimmed, as.character(round(target_px * caption_width_fraction)))
  
  spacer <- if (gap_px > 0) {
    magick_ns$image_blank(
      width = max(magick_ns$image_info(map_scaled)$width,
                  magick_ns$image_info(cap_scaled)$width),
      height = gap_px,
      color = "white"
    )
  }
  
  stacked <- if (gap_px > 0) {
    magick_ns$image_append(c(map_scaled, spacer, cap_scaled), stack = TRUE)
  } else {
    magick_ns$image_append(c(map_scaled, cap_scaled), stack = TRUE)
  }
  
  info <- magick_ns$image_info(stacked)
  scale_factor <- min(1, content_w_px / info$width, content_h_px / info$height)
  
  if (scale_factor < 1) {
    stacked <- magick_ns$image_scale(stacked, as.character(round(info$width * scale_factor)))
  }
  
  page <- magick_ns$image_blank(page_w_px, page_h_px, color = "white")
  
  info <- magick_ns$image_info(stacked)
  
  offset <- sprintf("+%d+%d",
                    margin_px + (content_w_px - info$width) / 2,
                    margin_px)
  
  final <- magick_ns$image_composite(page, stacked, offset = offset)
  
  magick_ns$image_write(final, out_pdf_path, format = "pdf")
  
  log_file <- file.path(species_dir, "_log.txt")
  
  cat(paste(
    "",
    paste(rep("-", 72), collapse = ""),
    "Processing summary (assemble_suitability_timeseries_page)",
    paste("Alpha code:", code),
    paste("Output:", out_pdf_path),
    sep = "\n"
  ), file = log_file, append = TRUE)
  
  invisible(out_pdf_path)
}