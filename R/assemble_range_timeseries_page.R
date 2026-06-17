#' Assemble a "Range Time Series" page
#'
#' Builds a single-page PDF that places a range contact-sheet map above a
#' caption in a structured layout.
#'
#' @details
#' \strong{Pipeline context}
#' This function composes a precomputed range contact-sheet image and a package
#' caption into a single standardized reporting page. It operates entirely
#' within the rENM project directory and does not rely on external scripts.
#'
#' \strong{Inputs}
#' Inputs must exist at the following locations:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/maps/<alpha_code>-Range-Maps.png
#' }
#' The caption is read from the installed package resource:
#' \preformatted{
#'   inst/captions/range_timeseries_caption.pdf
#' }
#'
#' \strong{Outputs}
#' The function writes a single-page PDF to:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Range-TimeSeries.pdf
#' }
#'
#' \strong{Layout and composition}
#' \itemize{
#'   \item The contact-sheet map is placed at the top of the content area,
#'         scaled to fill the content width (subject to \code{map_width_factor})
#'         while preserving its aspect ratio.
#'   \item A caption is placed \code{v_gap_in} inches below the map, centered
#'         horizontally and never stretched (aspect ratio preserved).
#'   \item Layout dimensions are controlled in inches and converted to pixels
#'         using the specified DPI.
#' }
#'
#' \strong{Caption rendering}
#' The caption PDF (\code{range_timeseries_caption.pdf}) is rasterized via
#' \code{pdftools::pdf_convert()} at \code{caption_dpi} (default 600) to
#' preserve original typography (including bold). It is trimmed to remove
#' surrounding page whitespace, then centered horizontally and scaled down
#' only if wider than the content area; otherwise it is drawn at native size.
#'
#' \strong{Trimming}
#' \itemize{
#'   \item Uniform borders are optionally trimmed from the map (controlled by
#'         \code{trim_map}) to remove hidden padding.
#'   \item The caption is always trimmed after rasterization to remove the
#'         surrounding page whitespace, so only the text block contributes
#'         to the layout height calculation.
#'   \item Trimming uses a tolerance defined by \code{trim_fuzz}.
#' }
#'
#' \strong{Methods}
#' Image reading, trimming, scaling, and composition are handled by the
#' magick package. The caption PDF is rasterized by pdftools.
#' Aspect-preserving resizing ensures consistent layout while respecting
#' maximum width and height constraints.
#'
#' \strong{Data requirements}
#' All required input files must exist prior to execution. The function will
#' terminate with an error if the map PNG or the caption PDF are missing.
#'
#' @param alpha_code Character. Species alpha code (case-insensitive).
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param dpi Numeric. DPI for raster composition. Default 300.
#' @param margin_left_in Numeric. Left margin in inches. Default 0.75.
#' @param margin_right_in Numeric. Right margin in inches. Default 0.75.
#' @param margin_bottom_in Numeric. Bottom margin in inches. Default 0.75.
#' @param margin_top_in Numeric. Top margin in inches. Default 1.00.
#' @param v_gap_in Numeric. Vertical gap between map and caption in inches.
#' Default 0.30.
#' @param map_width_factor Numeric. Fraction (0 to 1) of content width used
#' by the map. Default 1.00.
#' @param trim_map Logical. If TRUE, trim borders from the map PNG.
#' Default TRUE.
#' @param trim_fuzz Integer. Tolerance (0 to 100 percent) for trimming.
#' Default 8.
#' @param caption_dpi Integer. DPI used when rasterizing the caption PDF.
#' Default 600.
#' @param debug_frames Logical. If TRUE, draw thin frames around placed images
#' for debugging. Default FALSE.
#'
#' @return
#' Character. Invisibly returns the output PDF file path.
#'
#' Side effects:
#' \itemize{
#'   \item Writes a PDF file to the project directory.
#'   \item Creates output directories if they do not exist.
#'   \item Emits a console message indicating the output path.
#' }
#'
#' @importFrom magick image_read image_trim image_resize image_info image_blank
#' @importFrom magick image_composite image_write
#' @importFrom pdftools pdf_convert
#'
#' @examples
#' \dontrun{
#' assemble_range_timeseries_page("CASP")
#' }
#'
#' @export
assemble_range_timeseries_page <- function(alpha_code,
                                           page_width_in    = 8.5,
                                           page_height_in   = 11,
                                           dpi              = 300,
                                           margin_left_in   = 0.75,
                                           margin_right_in  = 0.75,
                                           margin_bottom_in = 0.75,
                                           margin_top_in    = 1.00,
                                           v_gap_in         = 0.30,
                                           map_width_factor = 1.00,
                                           trim_map         = TRUE,
                                           trim_fuzz        = 8,
                                           caption_dpi      = 600L,
                                           debug_frames     = FALSE) {

  # ---- Dependencies -----------------------------------------------------------
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required. Please install.packages('magick').",
         call. = FALSE)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for caption fidelity. install.packages('pdftools')",
         call. = FALSE)
  }

  # ---- Normalize / guards -----------------------------------------------------
  code             <- toupper(alpha_code)
  map_width_factor <- max(0.05, min(1.00, map_width_factor))
  fuzz             <- max(0L, min(100L, as.integer(trim_fuzz)))

  # ---- Paths ------------------------------------------------------------------
  project_dir <- rENM_project_dir()
  runs_dir    <- file.path(project_dir, "runs", code)
  pages_dir   <- file.path(runs_dir, "Summaries", "pages")
  if (!dir.exists(pages_dir)) {
    dir.create(pages_dir, recursive = TRUE, showWarnings = FALSE)
  }

  in_map_png     <- file.path(runs_dir, "Summaries", "maps",
                               sprintf("%s-Range-Maps.png", code))
  in_caption_pdf <- system.file("captions", "range_timeseries_caption.pdf",
                                package = "rENM.reports")
  out_pdf        <- file.path(pages_dir,
                               sprintf("%s-Range-TimeSeries.pdf", code))

  # ---- Validate inputs --------------------------------------------------------
  if (!file.exists(in_map_png)) {
    stop("Map PNG not found: ", in_map_png, call. = FALSE)
  }
  if (!nzchar(in_caption_pdf) || !file.exists(in_caption_pdf)) {
    stop("Caption PDF not found in installed package: inst/captions/range_timeseries_caption.pdf",
         call. = FALSE)
  }

  # ---- Geometry (inches -> pixels @ dpi) -------------------------------------
  px <- function(inches) as.integer(round(inches * dpi))

  page_w    <- px(page_width_in)
  page_h    <- px(page_height_in)
  mL        <- px(margin_left_in)
  mR        <- px(margin_right_in)
  mT        <- px(margin_top_in)
  mB        <- px(margin_bottom_in)
  vG        <- px(v_gap_in)

  content_x0 <- mL
  content_y0 <- mT
  content_x1 <- page_w - mR
  content_y1 <- page_h - mB
  content_w  <- content_x1 - content_x0
  content_h  <- content_y1 - content_y0

  # ---- Read and trim map ------------------------------------------------------
  im_map <- magick::image_read(in_map_png)
  if (trim_map) {
    im_map <- magick::image_trim(im_map, fuzz = fuzz)
  }

  # ---- Helpers: aspect-preserving "fit" ---------------------------------------
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

  # ---- Rasterize and trim caption PDF -----------------------------------------
  cap_png <- pdftools::pdf_convert(
    pdf       = in_caption_pdf,
    format    = "png",
    dpi       = as.integer(caption_dpi),
    pages     = 1L,
    filenames = file.path(tempdir(), "range_timeseries_caption_1.png")
  )
  im_cap   <- magick::image_read(cap_png)
  im_cap   <- magick::image_trim(im_cap, fuzz = fuzz)
  cap_info <- magick::image_info(im_cap)
  cap_w    <- as.integer(cap_info$width[1])
  cap_h    <- as.integer(cap_info$height[1])
  if (is.na(cap_w) || is.na(cap_h) || cap_w <= 0 || cap_h <= 0) {
    stop("Caption image has invalid dimensions after pdftools rasterization.",
         call. = FALSE)
  }
  if (cap_w > content_w) {
    scale_factor <- content_w / cap_w
    cap_w <- max(1L, as.integer(floor(cap_w * scale_factor)))
    cap_h <- max(1L, as.integer(floor(cap_h * scale_factor)))
    im_cap <- magick::image_resize(im_cap, paste0(cap_w, "x", cap_h, "!"))
  }

  # ---- Fit map to available vertical space ------------------------------------
  map_target_w    <- floor(content_w * map_width_factor)
  max_map_h_avail <- content_h - vG - cap_h
  if (max_map_h_avail < px(0.5)) {
    stop("Not enough vertical space: reduce v_gap_in or margins.", call. = FALSE)
  }

  im_map_fit <- fit_image(im_map, map_target_w, max_map_h_avail)
  map_info   <- magick::image_info(im_map_fit)
  map_w      <- as.integer(map_info$width)
  map_h      <- as.integer(map_info$height)

  map_y0 <- content_y0
  cap_y0 <- map_y0 + map_h + vG

  if (cap_y0 + cap_h > content_y1) {
    stop("Layout overflow: lower v_gap_in or increase bottom margin.", call. = FALSE)
  }

  # ---- Canvas -----------------------------------------------------------------
  canvas <- magick::image_blank(width = page_w, height = page_h, color = "white")

  # ---- Optional debug frames --------------------------------------------------
  if (debug_frames) {
    draw_frame <- function(img, x, y, w, h, col = "gray60") {
      top    <- magick::image_blank(w, 1, color = col)
      bottom <- magick::image_blank(w, 1, color = col)
      left   <- magick::image_blank(1, h, color = col)
      right  <- magick::image_blank(1, h, color = col)
      img <- magick::image_composite(img, top,
                                     offset = sprintf("+%d+%d", x, y))
      img <- magick::image_composite(img, bottom,
                                     offset = sprintf("+%d+%d", x, y + h - 1))
      img <- magick::image_composite(img, left,
                                     offset = sprintf("+%d+%d", x, y))
      img <- magick::image_composite(img, right,
                                     offset = sprintf("+%d+%d", x + w - 1, y))
      img
    }
    canvas <- draw_frame(canvas,
                         content_x0 + floor((content_w - map_w) / 2),
                         map_y0, map_w, map_h)
    canvas <- draw_frame(canvas,
                         content_x0 + floor((content_w - cap_w) / 2),
                         cap_y0, cap_w, cap_h)
  }

  # ---- Composite (centered map and caption) -----------------------------------
  map_dx <- content_x0 + floor((content_w - map_w) / 2)
  canvas <- magick::image_composite(canvas, im_map_fit,
                                    offset = sprintf("+%d+%d", map_dx, map_y0))

  cap_dx <- content_x0 + floor((content_w - cap_w) / 2)
  canvas <- magick::image_composite(canvas, im_cap,
                                    offset = sprintf("+%d+%d", cap_dx, cap_y0))

  # ---- Output -----------------------------------------------------------------
  magick::image_write(canvas, path = out_pdf, format = "pdf")
  message("Wrote: ", out_pdf)

  invisible(out_pdf)
}
