#' Assemble a "Variable Trends" page
#'
#' Builds a single-page PDF combining a variable-contribution lines plot,
#' a summary table, and a caption into a structured layout.
#'
#' @details
#' \strong{Pipeline context}
#' This function composes a precomputed variable-contribution trend plot, a
#' summary table, and a package caption into a single standardized reporting
#' page. It operates entirely within the rENM project directory and does not
#' rely on external scripts.
#'
#' \strong{Inputs}
#' Inputs must exist at the following locations:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Trends/variables/<alpha_code>-Variable-Contributions-BR-Lines-NoRibbon.png
#'   <project_dir>/runs/<alpha_code>/Summaries/tables/<alpha_code>-Variable-Trend-Summary.pdf
#' }
#' The caption is read from the installed package resource:
#' \preformatted{
#'   inst/captions/variable_trend_caption.pdf
#' }
#'
#' \strong{Outputs}
#' The function writes a single-page PDF to:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-Variable-Trends.pdf
#' }
#'
#' \strong{Layout and composition}
#' \itemize{
#'   \item The variable-contribution plot is placed at the top of the content
#'         area, scaled to fill the content width (subject to
#'         \code{plot_width_factor}) while preserving its aspect ratio.
#'   \item A summary table is placed \code{v_gap_in} inches below the plot,
#'         centered horizontally (subject to \code{table_width_factor}).
#'   \item A caption is placed \code{v_caption_gap_in} inches below the table,
#'         centered horizontally and never stretched (aspect ratio preserved).
#'   \item Layout dimensions are controlled in inches and converted to pixels
#'         using the specified DPI.
#' }
#'
#' \strong{Caption rendering}
#' The caption PDF (\code{variable_trend_caption.pdf}) is rasterized via
#' \code{pdftools::pdf_convert()} at \code{caption_dpi} (default 600) to
#' preserve original typography (including bold). It is trimmed to remove
#' surrounding page whitespace, then centered horizontally and scaled down
#' only if wider than the content area; otherwise it is drawn at native size.
#'
#' \strong{Table rendering}
#' The table PDF is rasterized via \code{pdftools::pdf_convert()} at
#' \code{table_pdf_dpi} (default 600) to improve text clarity. The table is
#' then fitted within \code{table_width_factor} of the content width subject
#' to \code{table_max_height_in}.
#'
#' \strong{Trimming}
#' \itemize{
#'   \item Uniform borders are optionally trimmed from the plot (controlled by
#'         \code{trim_plot}) and the table (controlled by \code{trim_table})
#'         to remove hidden padding.
#'   \item The caption is always trimmed after rasterization to remove the
#'         surrounding page whitespace, so only the text block contributes
#'         to the layout height calculation.
#'   \item Trimming uses a tolerance defined by \code{trim_fuzz}.
#' }
#'
#' \strong{Methods}
#' Image reading, trimming, scaling, and composition are handled by the
#' magick package. The table and caption PDFs are rasterized by pdftools.
#' Aspect-preserving resizing ensures consistent layout while respecting
#' maximum width and height constraints.
#'
#' \strong{Data requirements}
#' All required input files must exist prior to execution. The function will
#' terminate with an error if any required input or the caption PDF is missing.
#'
#' @param alpha_code Character. Species alpha code (case-insensitive).
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param dpi Numeric. DPI for raster composition. Default 300.
#' @param margin_left_in Numeric. Left margin in inches. Default 0.75.
#' @param margin_right_in Numeric. Right margin in inches. Default 0.75.
#' @param margin_bottom_in Numeric. Bottom margin in inches. Default 0.75.
#' @param margin_top_in Numeric. Top margin in inches. Default 1.00.
#' @param v_gap_in Numeric. Vertical gap between plot and table in inches.
#' Default 0.30.
#' @param v_caption_gap_in Numeric. Vertical gap between table and caption
#' in inches. Default 0.30.
#' @param plot_width_factor Numeric. Fraction (0 to 1) of content width used
#' by the plot. Default 0.85.
#' @param table_width_factor Numeric. Fraction (0 to 1) of content width used
#' by the table. Default 0.50.
#' @param table_max_height_in Numeric. Maximum table height in inches.
#' Default 3.00.
#' @param table_pdf_dpi Integer. DPI used when rasterizing the table PDF.
#' Default 600.
#' @param trim_plot Logical. If TRUE, trim borders from the plot PNG.
#' Default TRUE.
#' @param trim_table Logical. If TRUE, trim borders from the table image.
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
#' assemble_variable_trends_page("CASP")
#' }
#'
#' @export
assemble_variable_trends_page <- function(alpha_code,
                                          page_width_in       = 8.5,
                                          page_height_in      = 11,
                                          dpi                 = 300,
                                          margin_left_in      = 0.75,
                                          margin_right_in     = 0.75,
                                          margin_bottom_in    = 0.75,
                                          margin_top_in       = 1.00,
                                          v_gap_in            = 0.30,
                                          v_caption_gap_in    = 0.30,
                                          plot_width_factor   = 0.85,
                                          table_width_factor  = 0.50,
                                          table_max_height_in = 3.00,
                                          table_pdf_dpi       = 600L,
                                          trim_plot           = TRUE,
                                          trim_table          = TRUE,
                                          trim_fuzz           = 8,
                                          caption_dpi         = 600L,
                                          debug_frames        = FALSE) {

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
  code               <- toupper(alpha_code)
  plot_width_factor  <- max(0.05, min(1.00, plot_width_factor))
  table_width_factor <- max(0.05, min(1.00, table_width_factor))
  table_pdf_dpi      <- max(72L, as.integer(table_pdf_dpi))
  fuzz               <- max(0L, min(100L, as.integer(trim_fuzz)))

  # ---- Paths ------------------------------------------------------------------
  project_dir <- rENM_project_dir()
  runs_dir    <- file.path(project_dir, "runs", code)
  pages_dir   <- file.path(runs_dir, "Summaries", "pages")
  if (!dir.exists(pages_dir)) {
    dir.create(pages_dir, recursive = TRUE, showWarnings = FALSE)
  }

  in_plot_png    <- file.path(runs_dir, "Trends", "variables",
                               sprintf("%s-Variable-Contributions-BR-Lines-NoRibbon.png", code))
  in_tbl_pdf     <- file.path(runs_dir, "Summaries", "tables",
                               sprintf("%s-Variable-Trend-Summary.pdf", code))
  in_caption_pdf <- system.file("captions", "variable_trend_caption.pdf",
                                package = "rENM.reports")
  out_pdf        <- file.path(pages_dir, sprintf("%s-Variable-Trends.pdf", code))

  # ---- Validate inputs --------------------------------------------------------
  for (f in c(in_plot_png, in_tbl_pdf)) {
    if (!file.exists(f)) {
      stop("Missing input file: ", f, call. = FALSE)
    }
  }
  if (!nzchar(in_caption_pdf) || !file.exists(in_caption_pdf)) {
    stop("Caption PDF not found in installed package: inst/captions/variable_trend_caption.pdf",
         call. = FALSE)
  }

  # ---- Geometry (inches -> pixels @ dpi) --------------------------------------
  px <- function(inches) as.integer(round(inches * dpi))

  page_w  <- px(page_width_in)
  page_h  <- px(page_height_in)
  mL      <- px(margin_left_in)
  mR      <- px(margin_right_in)
  mT      <- px(margin_top_in)
  mB      <- px(margin_bottom_in)
  vG      <- px(v_gap_in)
  vCG     <- px(v_caption_gap_in)
  tblMaxH <- px(table_max_height_in)

  content_x0 <- mL
  content_y0 <- mT
  content_x1 <- page_w - mR
  content_y1 <- page_h - mB
  content_w  <- content_x1 - content_x0
  content_h  <- content_y1 - content_y0

  # ---- Read and trim plot -----------------------------------------------------
  im_plot <- magick::image_read(in_plot_png)
  if (trim_plot) {
    im_plot <- magick::image_trim(im_plot, fuzz = fuzz)
  }

  # ---- Rasterize and trim table PDF -------------------------------------------
  tbl_png <- pdftools::pdf_convert(
    pdf       = in_tbl_pdf,
    format    = "png",
    dpi       = table_pdf_dpi,
    pages     = 1L,
    filenames = file.path(tempdir(), "variable_trend_table_1.png")
  )
  im_tbl <- magick::image_read(tbl_png)
  if (trim_table) {
    im_tbl <- magick::image_trim(im_tbl, fuzz = fuzz)
  }

  # ---- Rasterize and trim caption PDF -----------------------------------------
  cap_png <- pdftools::pdf_convert(
    pdf       = in_caption_pdf,
    format    = "png",
    dpi       = as.integer(caption_dpi),
    pages     = 1L,
    filenames = file.path(tempdir(), "variable_trend_caption_1.png")
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

  # ---- Fit table (width factor + height cap) ----------------------------------
  tbl_target_w <- floor(content_w * table_width_factor)
  tbl_dims     <- fit_dims(magick::image_info(im_tbl), tbl_target_w, tblMaxH)
  tbl_w        <- as.integer(tbl_dims["w"])
  tbl_h        <- as.integer(tbl_dims["h"])
  im_tbl_fit   <- magick::image_resize(im_tbl, paste0(tbl_w, "x", tbl_h, "!"))

  # ---- Fit plot to available vertical space -----------------------------------
  plot_target_w    <- floor(content_w * plot_width_factor)
  max_plot_h_avail <- content_h - vG - tbl_h - vCG - cap_h
  if (max_plot_h_avail < px(0.5)) {
    stop("Not enough vertical space: reduce table_max_height_in, v_gap_in, or margins.",
         call. = FALSE)
  }

  im_plot_fit <- fit_image(im_plot, plot_target_w, max_plot_h_avail)
  plot_info   <- magick::image_info(im_plot_fit)
  plot_w      <- as.integer(plot_info$width)
  plot_h      <- as.integer(plot_info$height)

  plot_y0 <- content_y0
  tbl_y0  <- plot_y0 + plot_h + vG
  cap_y0  <- tbl_y0 + tbl_h + vCG

  if (cap_y0 + cap_h > content_y1) {
    stop("Layout overflow: lower v_gap_in or table_max_height_in, or increase bottom margin.",
         call. = FALSE)
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
                         content_x0 + floor((content_w - plot_w) / 2),
                         plot_y0, plot_w, plot_h)
    canvas <- draw_frame(canvas,
                         content_x0 + floor((content_w - tbl_w) / 2),
                         tbl_y0, tbl_w, tbl_h)
    canvas <- draw_frame(canvas,
                         content_x0 + floor((content_w - cap_w) / 2),
                         cap_y0, cap_w, cap_h)
  }

  # ---- Composite (centered plot, table, and caption) --------------------------
  plot_dx <- content_x0 + floor((content_w - plot_w) / 2)
  canvas <- magick::image_composite(canvas, im_plot_fit,
                                    offset = sprintf("+%d+%d", plot_dx, plot_y0))

  tbl_dx <- content_x0 + floor((content_w - tbl_w) / 2)
  canvas <- magick::image_composite(canvas, im_tbl_fit,
                                    offset = sprintf("+%d+%d", tbl_dx, tbl_y0))

  cap_dx <- content_x0 + floor((content_w - cap_w) / 2)
  canvas <- magick::image_composite(canvas, im_cap,
                                    offset = sprintf("+%d+%d", cap_dx, cap_y0))

  # ---- Output -----------------------------------------------------------------
  magick::image_write(canvas, path = out_pdf, format = "pdf")
  message("Wrote: ", out_pdf)

  invisible(out_pdf)
}
