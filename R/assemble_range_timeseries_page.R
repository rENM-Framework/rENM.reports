#' Assemble a "Range Time Series" page
#'
#' Builds a single-page PDF that vertically stacks a collection of range
#' maps above a shared caption with controlled spacing and proportional scaling.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' \itemize{
#'   \item Input map: per-species PNG created earlier in the reporting workflow.
#'   \item Input caption: shared PDF stored in the \code{rENM.reports} package.
#'   \item Output page: PDF saved under each species' \code{Summaries/pages/}
#'         directory.
#' }
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Range map PNG (per species):
#'     \code{<project_dir>/runs/<alpha_code>/Summaries/maps/<alpha_code>-Range-Maps.png}
#'   \item Range time-series caption PDF (first page only, shared):
#'     \code{system.file("captions", "range_timeseries_caption.pdf",
#'                       package = "rENM.reports")}
#' }
#'
#' \strong{Outputs}
#' \itemize{
#'   \item Assembled PDF page:
#'     \code{<project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Range-TimeSeries.pdf}
#'   \item Log entry appended to:
#'     \code{<project_dir>/runs/<alpha_code>/_log.txt}
#' }
#'
#' \strong{Methods}
#' \itemize{
#'   \item Rasterization: \code{pdftools::pdf_render_page()} at the requested
#'         \code{dpi}.
#'   \item Margin trimming: \code{magick::image_trim()} with \code{trim_fuzz}
#'         tolerance.
#'   \item Scaling: width fractions applied to
#'         \code{target_content_width_in * dpi} pixels.
#'   \item Gap: white spacer of \code{gap_between_in * dpi} pixels inserted
#'         between map and caption.
#' }
#'
#' \strong{Data requirements}
#' All file paths must exist and be writable; the species directory created by
#' earlier rENM steps is assumed present.
#'
#' @param alpha_code Character. Species alpha code (e.g., "CASP"). Converted
#'   to uppercase internally for file naming and paths.
#' @param page_width_in Numeric. Page width in inches; default 8.5 (Letter).
#' @param page_height_in Numeric. Page height in inches; default 11 (Letter).
#' @param dpi Numeric. Raster resolution (dots per inch) for caption rendering
#'   and final layout. Default 300.
#' @param margin_in Numeric. Uniform page margin in inches. Default 1.
#' @param target_content_width_in Numeric. Target graphic width in inches when
#'   both width fractions equal 1.0. Default 6.
#' @param map_width_fraction Numeric. Fraction (0,1] of
#'   \code{target_content_width_in} assigned to the map. Default 0.85.
#' @param caption_width_fraction Numeric. Fraction (0,1] of
#'   \code{target_content_width_in} assigned to the caption. Default 0.85.
#' @param gap_between_in Numeric. Vertical gap between map and caption in
#'   inches. Default 0.50; set to 0 for no gap.
#' @param trim_fuzz Numeric. Fuzz percentage for
#'   \code{magick::image_trim()}.
#'   \itemize{
#'     \item \code{0}   trims only perfectly uniform borders.
#'     \item \code{5-10} often works for anti-aliased white backgrounds.
#'   }
#'   Default 5.
#' @param verbose Logical. If \code{TRUE}, prints diagnostic messages.
#'   Default \code{TRUE}.
#'
#' @return Invisible \code{Character}. Full file path of the assembled PDF.
#' \itemize{
#'   \item Structure: length-one character vector.
#'   \item Side effects: writes the PDF to disk and appends a processing
#'         summary to the species log file.
#' }
#'
#' @importFrom magick image_read image_info image_scale image_append
#' @importFrom magick image_blank image_composite image_write image_trim
#' @importFrom pdftools pdf_render_page
#'
#' @examples
#' \dontrun{
#' assemble_range_timeseries_page("CASP")
#'
#' assemble_range_timeseries_page(
#'   "CASP",
#'   map_width_fraction     = 1.0,
#'   caption_width_fraction = 0.9,
#'   gap_between_in         = 0.25
#' )
#' }
#'
#' @export
assemble_range_timeseries_page <- function(alpha_code,
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
  
  # Normalize alpha code (e.g., "casp" -> "CASP") so paths are consistent.
  code <- toupper(alpha_code)
  
  # Width fractions must be in (0, 1].
  if (!is.numeric(map_width_fraction) ||
      length(map_width_fraction) != 1L ||
      map_width_fraction <= 0 || map_width_fraction > 1) {
    stop("map_width_fraction must be a single numeric value in (0, 1].")
  }
  
  if (!is.numeric(caption_width_fraction) ||
      length(caption_width_fraction) != 1L ||
      caption_width_fraction <= 0 || caption_width_fraction > 1) {
    stop("caption_width_fraction must be a single numeric value in (0, 1].")
  }
  
  # Target content width must be positive (in inches).
  if (!is.numeric(target_content_width_in) ||
      length(target_content_width_in) != 1L ||
      target_content_width_in <= 0) {
    stop("target_content_width_in must be a single positive numeric value (in inches).")
  }
  
  # Gap can be zero or positive, but not negative.
  if (!is.numeric(gap_between_in) ||
      length(gap_between_in) != 1L ||
      gap_between_in < 0) {
    stop("gap_between_in must be a single numeric value >= 0 (in inches).")
  }
  
  # Trim fuzz must be >= 0.
  if (!is.numeric(trim_fuzz) || length(trim_fuzz) != 1L || trim_fuzz < 0) {
    stop("trim_fuzz must be a single numeric value >= 0.")
  }
  
  # --------------------------------------------------------------------------
  # 1. Package checks (magick + pdftools)
  # --------------------------------------------------------------------------
  
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required but not installed.", call. = FALSE)
  }
  
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required but not installed.", call. = FALSE)
  }
  
  magick_ns   <- asNamespace("magick")
  pdftools_ns <- asNamespace("pdftools")
  
  # --------------------------------------------------------------------------
  # 2. Construct file paths using rENM_project_dir()
  # --------------------------------------------------------------------------
  
  project_dir <- rENM_project_dir()
  
  # Root directory for this species: <project_dir>/runs/<alpha_code>
  species_dir <- file.path(project_dir, "runs", code)
  
  if (!dir.exists(species_dir)) {
    stop(sprintf("Species directory not found: %s", species_dir), call. = FALSE)
  }
  
  # Range map PNG (per species).
  map_path <- file.path(
    species_dir, "Summaries", "maps",
    sprintf("%s-Range-Maps.png", code)
  )
  
  # Shared range time–series caption: packaged resource.
  caption_pdf_path <- system.file(
    "captions",
    "range_timeseries_caption.pdf",
    package = "rENM.reports"
  )
  
  # Output directory and assembled PDF path (range time-series page).
  out_dir <- file.path(species_dir, "Summaries", "pages")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  out_pdf_path <- file.path(
    out_dir,
    sprintf("%s-Range-TimeSeries.pdf", code)
  )
  
  if (verbose) {
    message("[assemble_range_timeseries_page] Map:     ", map_path)
    message("[assemble_range_timeseries_page] Caption: ", caption_pdf_path)
    message("[assemble_range_timeseries_page] Output:  ", out_pdf_path)
  }
  
  if (!file.exists(map_path)) {
    stop(sprintf("Range map PNG not found: %s", map_path), call. = FALSE)
  }
  if (!file.exists(caption_pdf_path)) {
    stop(sprintf("Range caption PDF not found: %s", caption_pdf_path), call. = FALSE)
  }
  
  # --------------------------------------------------------------------------
  # 3. Compute page and content geometry (in pixels)
  # --------------------------------------------------------------------------
  
  page_w_px <- as.integer(round(page_width_in  * dpi))
  page_h_px <- as.integer(round(page_height_in * dpi))
  margin_px <- as.integer(round(margin_in      * dpi))
  
  content_w_px <- max(page_w_px - 2L * margin_px, 1L)
  content_h_px <- max(page_h_px - 2L * margin_px, 1L)
  
  target_px <- as.integer(round(target_content_width_in * dpi))
  if (target_px > content_w_px) {
    target_px <- content_w_px
    if (verbose) {
      message(
        "[assemble_range_timeseries_page] target_content_width_in exceeds content width; ",
        "capping to interior width."
      )
    }
  }
  
  if (verbose) {
    message(sprintf(
      "[assemble_range_timeseries_page] Page %dx%d px, content %dx%d px (dpi = %d, target width = %d px)",
      page_w_px, page_h_px, content_w_px, content_h_px, dpi, target_px
    ))
  }
  
  gap_px <- as.integer(round(gap_between_in * dpi))
  
  # --------------------------------------------------------------------------
  # 4. Read map PNG and caption PDF (first page) as magick images
  # --------------------------------------------------------------------------
  
  map_img <- magick_ns$image_read(map_path)
  
  caption_raw <- pdftools_ns$pdf_render_page(
    pdf  = caption_pdf_path,
    page = 1,
    dpi  = dpi
  )
  caption_img <- magick_ns$image_read(caption_raw)
  
  # --------------------------------------------------------------------------
  # 5. Trim white margins from map and caption
  # --------------------------------------------------------------------------
  
  if (verbose) {
    message(sprintf(
      "[assemble_range_timeseries_page] Trimming map and caption borders (fuzz = %.1f%%)...",
      trim_fuzz
    ))
  }
  
  map_trimmed <- magick_ns$image_trim(map_img, fuzz = trim_fuzz)
  cap_trimmed <- magick_ns$image_trim(caption_img, fuzz = trim_fuzz)
  
  # --------------------------------------------------------------------------
  # 6. Scale trimmed map and caption to requested width fractions
  # --------------------------------------------------------------------------
  
  target_map_w_px     <- as.integer(round(target_px * map_width_fraction))
  target_caption_w_px <- as.integer(round(target_px * caption_width_fraction))
  
  map_scaled <- magick_ns$image_scale(map_trimmed, geometry = as.character(target_map_w_px))
  cap_scaled <- magick_ns$image_scale(cap_trimmed, geometry = as.character(target_caption_w_px))
  
  if (verbose) {
    map_info <- magick_ns$image_info(map_scaled)
    cap_info <- magick_ns$image_info(cap_scaled)
    message(sprintf(
      "[assemble_range_timeseries_page] Map scaled (trimmed) to: %dx%d px",
      map_info$width, map_info$height
    ))
    message(sprintf(
      "[assemble_range_timeseries_page] Caption scaled (trimmed) to: %dx%d px",
      cap_info$width, cap_info$height
    ))
  }
  
  # --------------------------------------------------------------------------
  # 7. Build gap spacer (if any) and vertically stack map, gap, and caption
  # --------------------------------------------------------------------------
  
  map_info <- magick_ns$image_info(map_scaled)
  cap_info <- magick_ns$image_info(cap_scaled)
  spacer_w_px <- max(map_info$width, cap_info$width)
  
  if (gap_px > 0) {
    spacer <- magick_ns$image_blank(
      width  = spacer_w_px,
      height = gap_px,
      color  = "white"
    )
    
    stacked <- magick_ns$image_append(
      c(map_scaled, spacer, cap_scaled),
      stack = TRUE
    )
  } else {
    stacked <- magick_ns$image_append(
      c(map_scaled, cap_scaled),
      stack = TRUE
    )
  }
  
  stack_info <- magick_ns$image_info(stacked)
  stack_w_px <- stack_info$width
  stack_h_px <- stack_info$height
  
  if (verbose) {
    message(sprintf(
      "[assemble_range_timeseries_page] Stacked size: %dx%d px",
      stack_w_px, stack_h_px
    ))
  }
  
  # --------------------------------------------------------------------------
  # 8. Ensure the stacked image fits inside the interior content rectangle
  # --------------------------------------------------------------------------
  
  scale_factor <- min(
    1.0,
    content_w_px / stack_w_px,
    content_h_px / stack_h_px
  )
  
  if (scale_factor < 1.0) {
    new_w_px <- as.integer(round(stack_w_px * scale_factor))
    stacked  <- magick_ns$image_scale(stacked, geometry = as.character(new_w_px))
    
    stack_info <- magick_ns$image_info(stacked)
    stack_w_px <- stack_info$width
    stack_h_px <- stack_info$height
    
    if (verbose) {
      message(sprintf(
        "[assemble_range_timeseries_page] Stack downscaled to: %dx%d px (factor %.3f)",
        stack_w_px, stack_h_px, scale_factor
      ))
    }
  } else if (verbose) {
    message("[assemble_range_timeseries_page] Stack fits in content; no additional downscaling.")
  }
  
  # --------------------------------------------------------------------------
  # 9. Create blank page and composite stack at top margin (centered horizontally)
  # --------------------------------------------------------------------------
  
  page_blank <- magick_ns$image_blank(
    width  = page_w_px,
    height = page_h_px,
    color  = "white"
  )
  
  offset_x <- margin_px + as.integer((content_w_px - stack_w_px) / 2L)
  offset_y <- margin_px
  
  offset_str <- sprintf("+%d+%d", offset_x, offset_y)
  
  if (verbose) {
    message(sprintf(
      "[assemble_range_timeseries_page] Compositing at offset %s.",
      offset_str
    ))
  }
  
  page_final <- magick_ns$image_composite(
    image           = page_blank,
    composite_image = stacked,
    operator        = "over",
    offset          = offset_str
  )
  
  # --------------------------------------------------------------------------
  # 10. Write output as a single-page PDF
  # --------------------------------------------------------------------------
  
  magick_ns$image_write(page_final, path = out_pdf_path, format = "pdf")
  
  if (verbose) {
    message("[assemble_range_timeseries_page] Wrote PDF: ", out_pdf_path)
  }
  
  # --------------------------------------------------------------------------
  # 11. eBird-standard log entry
  # --------------------------------------------------------------------------
  
  stop_time   <- Sys.time()
  elapsed_sec <- as.numeric(difftime(stop_time, start_time, units = "secs"))
  log_file    <- file.path(species_dir, "_log.txt")
  sep_line    <- paste(rep("-", 72), collapse = "")
  
  log_lines <- c(
    "",
    sep_line,
    "Processing summary (assemble_range_timeseries_page)",
    sprintf("Alpha code:       %s", code),
    sprintf("Caption source:   %s", caption_pdf_path),
    sprintf("Start time:       %s", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Stop time:        %s", format(stop_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Outputs saved:    %d", 1L),
    sprintf("Output file:      %s", out_pdf_path),
    sprintf("Total elapsed:    %.1f seconds", elapsed_sec)
  )
  
  cat(paste0(log_lines, collapse = "\n"),
      file = log_file, sep = "\n", append = TRUE)
  if (verbose) {
    message("[assemble_range_timeseries_page] Appended processing summary to: ", log_file)
  }
  
  invisible(out_pdf_path)
}