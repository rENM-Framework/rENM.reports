#' Assemble a "Suitability Trends" page
#'
#' Builds a single-page PDF combining a suitability trend map, a suitability
#' change trend map (acceleration/deceleration), and a caption. 
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' Produces a single-page PDF with:
#' \itemize{
#'   \item Top row:
#'     \itemize{
#'       \item (A) "<ALPHA>-Suitability-Trend.png"
#'       \item (B) "<ALPHA>-Suitability-Change-Trend.png", side-by-side.
#'     }
#'   \item Bottom:
#'     \itemize{
#'       \item (C) A fixed, shared caption PDF included with the
#'       rENM.reports package:
#'       "inst/captions/suitability_trend_caption.pdf"
#'       (accessed via system.file()).
#'     }
#' }
#'
#' The caption is rasterized via pdftools at high DPI to preserve
#' original typography (including bold), centered horizontally, and
#' never stretched (aspect ratio preserved).
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Caption PDF is read directly from installed package resources
#'   (no staging).
#'   \item A and B are read as PNG files from:
#'     \code{<project_dir>/runs/<alpha_code>/Trends/suitability/}
#' }
#'
#' \strong{Processing steps}
#' \itemize{
#'   \item Reads the caption PDF directly from the installed package
#'   resources (no staging).
#'   \item Reads A + B (PNG) and rasterizes C (PDF) using
#'   pdftools::pdf_convert() at a specified caption_dpi (default 600).
#'   \item Scales the caption down only if wider than the content area;
#'   otherwise draws at native size, centered.
#'   \item Composites everything onto a Letter page and writes:
#'     \code{<project_dir>/runs/<alpha_code>/Summaries/pages/}
#'     "<alpha_code>-Suitability-Trends.pdf".
#' }
#'
#' \strong{Outputs}
#' \itemize{
#'   \item Output PDF written to:
#'     \code{<project_dir>/runs/<alpha_code>/Summaries/pages/}
#'     "<alpha_code>-Suitability-Trends.pdf"
#' }
#'
#' \strong{Methods}
#' Why pdftools? Using pdftools::pdf_convert() (Poppler) avoids font
#' substitution issues that can occur when ImageMagick/Ghostscript
#' cannot find the bold font family. Rasterizing at a high DPI preserves
#' the visual weight of bold text exactly as in the source PDF. The image
#' is then centered and never stretched (aspect ratio preserved).
#'
#' \strong{Data requirements}
#' Dependencies: Requires the magick and pdftools packages. If png is
#' missing on your system, install it as well; pdf_convert() writes a PNG
#' which is then read via magick.
#'
#' Logging:
#' Appends an eBird-standard processing summary to
#' \code{<project_dir>/runs/<alpha_code>/}
#' \code{_log.txt}
#' with a blank line before the dashed separator and no trailing
#' separator, including the caption source path.
#'
#' @param alpha_code Character. Species alpha code (case-insensitive).
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param margin_top_in Numeric. Top margin in inches. Default 1.
#' @param margin_right_in Numeric. Right margin in inches. Default 1.
#' @param margin_bottom_in Numeric. Bottom margin in inches. Default 1.
#' @param margin_left_in Numeric. Left margin in inches. Default 1.
#' @param dpi Integer. Working resolution for page/maps (dots per inch).
#' Default 300.
#' @param caption_dpi Integer. Rasterization DPI for the caption PDF
#' (higher = crisper). Default 600.
#' @param panel_gap_in Numeric. Horizontal gap (inches) between the two
#' top panels. Default 0.15.
#' @param v_gap_in Numeric. Vertical gap (inches) between the top row and
#' the caption. Default 0.25.
#'
#' @return
#' Character. Invisibly returns the output PDF path.
#' \itemize{
#'   \item Output file written to:
#'     \code{<project_dir>/runs/<alpha_code>/Summaries/pages/}
#'     "<alpha_code>-Suitability-Trends.pdf"
#'   \item Appends processing summary to:
#'     \code{<project_dir>/runs/<alpha_code>/}
#'     \code{_log.txt}
#' }
#'
#' @importFrom magick image_read image_info image_resize image_blank
#' image_append image_composite image_write
#' @importFrom pdftools pdf_convert
#'
#' @examples
#' \dontrun{
#' assemble_suitability_trends_page("CASP")
#' assemble_suitability_trends_page("CASP", caption_dpi = 900)
#' }
#'
#' @export
assemble_suitability_trends_page <- function(alpha_code,
                                             page_width_in  = 8.5,
                                             page_height_in = 11,
                                             margin_top_in    = 1.0,
                                             margin_right_in  = 1.0,
                                             margin_bottom_in = 1.0,
                                             margin_left_in   = 1.0,
                                             dpi = 300,
                                             caption_dpi = 600,
                                             panel_gap_in = 0.15,
                                             v_gap_in     = 0.25) {
  # ---- Start & identifiers ----------------------------------------------------
  start_time <- Sys.time()
  code <- toupper(alpha_code)
  message("[assemble_suitability_trends_page] Begin for ", code)
  
  # ---- Paths -----------------------------------------------------------------
  project_dir <- rENM_project_dir()
  
  runs_dir   <- file.path(project_dir, "runs", code)
  trends_dir <- file.path(runs_dir, "Trends", "suitability")
  pages_dir  <- file.path(runs_dir, "Summaries", "pages")
  if (!dir.exists(pages_dir)) {
    dir.create(pages_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  file_A <- file.path(trends_dir, sprintf("%s-Suitability-Trend.png", code))
  file_B <- file.path(trends_dir, sprintf("%s-Suitability-Change-Trend.png", code))
  
  # Fixed, shared caption path (from installed package)
  file_C <- system.file("captions", "suitability_trend_caption.pdf",
                        package = "rENM.reports")
  
  out_pdf <- file.path(pages_dir, sprintf("%s-Suitability-Trends.pdf", code))
  
  # ---- Validate inputs --------------------------------------------------------
  for (f in c(file_A, file_B, file_C)) {
    if (!file.exists(f)) {
      stop("Missing input file: ", f, call. = FALSE)
    }
  }
  
  # ---- Dependencies -----------------------------------------------------------
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required. Please install.packages('magick').", call. = FALSE)
  }
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required for caption fidelity. install.packages('pdftools')", call. = FALSE)
  }
  magick <- asNamespace("magick")
  
  # ---- Page geometry (pixels) -------------------------------------------------
  px <- function(inches) as.integer(round(inches * dpi))
  page_w_px <- px(page_width_in)
  page_h_px <- px(page_height_in)
  left_px   <- px(margin_left_in)
  right_px  <- px(margin_right_in)
  top_px    <- px(margin_top_in)
  bottom_px <- px(margin_bottom_in)
  content_w_px <- page_w_px - left_px - right_px
  h_gap_px     <- px(panel_gap_in)
  v_gap_px     <- px(v_gap_in)
  
  # ---- Read and scale top panels (PNG) ---------------------------------------
  message("[assemble_suitability_trends_page] Reading trend panels...")
  img_A <- magick$image_read(file_A)
  img_B <- magick$image_read(file_B)
  
  target_w_each <- max(1L, floor((content_w_px - h_gap_px) / 2))
  
  scale_to_width <- function(im, w_target) {
    info <- magick$image_info(im)
    w0 <- as.numeric(info$width[1])
    h0 <- as.numeric(info$height[1])
    if (is.na(w0) || is.na(h0) || w0 <= 0 || h0 <= 0) {
      stop("Invalid source image size.")
    }
    h_target <- max(1L, as.integer(round(h0 * (w_target / w0))))
    magick$image_resize(im, geometry = sprintf("%dx%d!", w_target, h_target))
  }
  
  img_A_res <- scale_to_width(img_A, target_w_each)
  img_B_res <- scale_to_width(img_B, target_w_each)
  
  info_A  <- magick$image_info(img_A_res)
  info_B  <- magick$image_info(img_B_res)
  row_h_px <- max(as.integer(info_A$height[1]), as.integer(info_B$height[1]))
  
  pad_to_height <- function(im, target_h) {
    inf <- magick$image_info(im)
    h0  <- as.integer(inf$height[1])
    w0  <- as.integer(inf$width[1])
    if (h0 >= target_h) return(im)
    dy <- target_h - h0
    pad_top <- magick$image_blank(width = w0, height = floor(dy / 2), color = "white")
    pad_bot <- magick$image_blank(width = w0, height = ceiling(dy / 2), color = "white")
    magick$image_append(c(pad_top, im, pad_bot), stack = TRUE)
  }
  
  img_A_row <- pad_to_height(img_A_res, row_h_px)
  img_B_row <- pad_to_height(img_B_res, row_h_px)
  
  row_strip <- magick$image_append(
    c(
      img_A_row,
      magick$image_blank(width = h_gap_px, height = row_h_px, color = "white"),
      img_B_row
    ),
    stack = FALSE
  )
  
  # ---- Rasterize caption with pdftools ---------------------------------------
  message("[assemble_suitability_trends_page] Rasterizing caption PDF via pdftools (",
          caption_dpi, " dpi)...")
  cap_png <- pdftools::pdf_convert(
    pdf       = file_C,
    format    = "png",
    dpi       = caption_dpi,
    pages     = 1,
    filenames = file.path(tempdir(), "caption_%d.png")
  )
  caption_img <- magick$image_read(cap_png)
  
  cap_info <- magick$image_info(caption_img)
  cap_w <- as.integer(cap_info$width[1])
  cap_h <- as.integer(cap_info$height[1])
  if (is.na(cap_w) || is.na(cap_h) || cap_w <= 0 || cap_h <= 0) {
    stop("Caption image has invalid dimensions after pdftools rasterization.")
  }
  
  if (cap_w > content_w_px) {
    scale_factor <- content_w_px / cap_w
    new_w <- max(1L, as.integer(floor(cap_w * scale_factor)))
    new_h <- max(1L, as.integer(floor(cap_h * scale_factor)))
    caption_img <- magick$image_resize(
      caption_img,
      geometry = sprintf("%dx%d", new_w, new_h)
    )
    cap_w <- new_w
    cap_h <- new_h
  }
  
  # ---- Compose final page -----------------------------------------------------
  message("[assemble_suitability_trends_page] Compositing page...")
  page_canvas <- magick$image_blank(width = page_w_px,
                                    height = page_h_px,
                                    color = "white")
  
  page_canvas <- magick$image_composite(
    page_canvas,
    row_strip,
    offset = sprintf("+%d+%d", left_px, top_px)
  )
  
  cap_x <- left_px + floor((content_w_px - cap_w) / 2)
  cap_y <- top_px + row_h_px + v_gap_px
  
  page_canvas <- magick$image_composite(
    page_canvas,
    caption_img,
    offset = sprintf("+%d+%d", cap_x, cap_y)
  )
  
  # ---- Write output PDF -------------------------------------------------------
  message("[assemble_suitability_trends_page] Writing PDF: ", out_pdf)
  magick$image_write(page_canvas, path = out_pdf, format = "pdf")
  
  # ---- eBird-standard log -----------------------------------------------------
  stop_time   <- Sys.time()
  elapsed_sec <- as.numeric(difftime(stop_time, start_time, units = "secs"))
  log_file    <- file.path(runs_dir, "_log.txt")
  sep_line    <- paste(rep("-", 72), collapse = "")
  
  log_lines <- c(
    "",
    sep_line,
    "Processing summary (assemble_suitability_trends_page)",
    sprintf("Alpha code:       %s", code),
    sprintf("Caption source:   %s", file_C),
    sprintf("Start time:       %s", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Stop time:        %s", format(stop_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Outputs saved:    %d", 1L),
    sprintf("Output file:      %s", out_pdf),
    sprintf("Total elapsed:    %.1f seconds", elapsed_sec)
  )
  
  cat(paste0(log_lines, collapse = "\n"),
      file = log_file, sep = "\n", append = TRUE)
  message("[assemble_suitability_trends_page] Appended processing summary to: ", log_file)
  
  invisible(out_pdf)
}