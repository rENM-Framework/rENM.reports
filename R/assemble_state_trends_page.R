#' Assemble a "State Trends" page
#'
#' Builds a single-page PDF that combines a state-level suitability map and
#' a state hot spot map with a summary table into a structured layout. 
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' This function composes multiple precomputed visualization products into a
#' single standardized reporting page. It operates entirely within the rENM
#' project directory and does not rely on external scripts.
#'
#' \strong{Inputs}
#' Inputs must exist at the following locations:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Trends/suitability/<alpha_code>-Suitability-Trend-State-Analysis.png
#'   <project_dir>/runs/<alpha_code>/Trends/suitability/<alpha_code>-Suitability-Trend-State-Analysis-Hotspots.png
#'   <project_dir>/runs/<alpha_code>/Summaries/tables/<alpha_code>-Suitability-Trend-Summary.png
#'   (Optional) PDF table:
#'   <project_dir>/runs/<alpha_code>/Summaries/tables/<alpha_code>-Suitability-Trend-Summary.pdf
#' }
#'
#' \strong{Outputs}
#' The function writes a single-page PDF to:
#' \preformatted{
#'   <project_dir>/runs/<alpha_code>/Summaries/pages/<alpha_code>-State-Trends.pdf
#' }
#'
#' \strong{Layout and composition}
#' \itemize{
#'   \item Two top-aligned maps (State Analysis and Hotspots) are placed under
#'         a configurable top margin.
#'   \item The map row can be narrowed and centered using
#'         \code{map_width_factor}.
#'   \item A summary table is placed exactly \code{v_gap_in} inches below the
#'         taller of the two maps.
#'   \item The table can be narrowed and centered using
#'         \code{table_width_factor}.
#'   \item Layout dimensions are controlled in inches and converted to pixels
#'         using the specified DPI.
#' }
#'
#' \strong{Table rendering}
#' \itemize{
#'   \item If \code{prefer_table_pdf = TRUE}, the table is read from PDF using
#'         \code{magick::image_read_pdf()} and rasterized at
#'         \code{table_pdf_dpi} (for example, 600) to improve text clarity.
#'   \item If using a PNG, \code{table_png_supersample} (for example, 2 for
#'         200 percent) increases resolution prior to fitting.
#'   \item Optional sharpening can be applied using
#'         \code{table_sharpen = TRUE}.
#' }
#'
#' \strong{Trimming}
#' \itemize{
#'   \item Uniform borders are trimmed from maps and optionally from the
#'         table to remove hidden padding.
#'   \item Trimming uses a tolerance defined by \code{trim_fuzz}.
#' }
#'
#' \strong{Methods}
#' All image reading, trimming, scaling, and composition are handled using the
#' magick package. Aspect-preserving resizing ensures consistent layout while
#' respecting maximum width and height constraints.
#'
#' \strong{Data requirements}
#' All required input files must exist prior to execution. The function will
#' terminate with an error if required PNGs or table sources are missing.
#'
#' @param alpha_code Character. Species code (case-insensitive).
#' @param page_width_in Numeric. Page width in inches. Default 8.5.
#' @param page_height_in Numeric. Page height in inches. Default 11.
#' @param dpi Numeric. DPI for raster composition. Default 300.
#' @param margin_left_in Numeric. Left margin in inches. Default 0.75.
#' @param margin_right_in Numeric. Right margin in inches. Default 0.75.
#' @param margin_bottom_in Numeric. Bottom margin in inches. Default 0.75.
#' @param margin_top_in Numeric. Top margin in inches. Default 1.00.
#' @param h_gutter_in Numeric. Horizontal gap between maps in inches.
#' Default 0.20.
#' @param v_gap_in Numeric. Vertical gap between maps and table in inches.
#' Default 1.00.
#' @param map_width_factor Numeric. Fraction (0 to 1) of content width used
#' by the map row. Default 1.00.
#' @param table_width_factor Numeric. Fraction (0 to 1) of content width used
#' by the table. Default 0.75.
#' @param table_max_height_in Numeric. Maximum table height in inches.
#' Default 2.50.
#' @param prefer_table_pdf Logical. If TRUE and a PDF exists, read the table
#' using \code{magick::image_read_pdf()}. Default TRUE.
#' @param table_pdf_dpi Integer. DPI used when rasterizing table PDF.
#' Default 600.
#' @param table_png_supersample Numeric. Supersampling factor for PNG tables;
#' values greater than or equal to 1 increase resolution (for example,
#' 2 = 200 percent). Default 1.5.
#' @param table_sharpen Logical. If TRUE, apply mild sharpening after scaling.
#' Default TRUE.
#' @param trim_maps Logical. If TRUE, trim borders from map PNGs.
#' Default TRUE.
#' @param trim_table Logical. If TRUE, trim borders from the table image.
#' Default TRUE.
#' @param trim_fuzz Integer. Tolerance (0 to 100 percent) for trimming.
#' Default 8.
#' @param append_version_to_filename Logical. If TRUE, append a timestamp to
#' the output filename to avoid preview caching. Default FALSE.
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
#' @importFrom magick image_read image_read_pdf image_trim image_scale
#' @importFrom magick image_resize image_info image_blank image_composite
#' @importFrom magick image_write
#'
#' @examples
#' \dontrun{
#' assemble_state_trends_page("CASP")
#' }
#'
#' @export
assemble_state_trends_page <- function(alpha_code,
                                       page_width_in = 8.5,
                                       page_height_in = 11,
                                       dpi = 300,
                                       margin_left_in = 0.75,
                                       margin_right_in = 0.75,
                                       margin_bottom_in = 0.75,
                                       margin_top_in = 1.00,
                                       h_gutter_in = 0.20,
                                       v_gap_in = 1.00,
                                       map_width_factor = 1.00,
                                       table_width_factor = 0.75,
                                       table_max_height_in = 2.50,
                                       prefer_table_pdf = TRUE,
                                       table_pdf_dpi = 600,
                                       table_png_supersample = 1.5,
                                       table_sharpen = TRUE,
                                       trim_maps = TRUE,
                                       trim_table = TRUE,
                                       trim_fuzz = 8,
                                       append_version_to_filename = FALSE,
                                       debug_frames = FALSE) {
  
  # ---- Dependencies ---------------------------------------------------------
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required. Please install.packages('magick').",
         call. = FALSE)
  }
  
  # ---- Normalize / guards ---------------------------------------------------
  code <- toupper(alpha_code)
  map_width_factor       <- max(0.05, min(1.00, map_width_factor))
  table_width_factor     <- max(0.05, min(1.00, table_width_factor))
  table_png_supersample  <- max(1.0, as.numeric(table_png_supersample))
  table_pdf_dpi          <- max(72L, as.integer(table_pdf_dpi))
  fuzz                   <- max(0L, min(100L, as.integer(trim_fuzz)))
  
  # ---- Paths ----------------------------------------------------------------
  project_dir <- rENM_project_dir()
  runs_dir <- file.path(project_dir, "runs", code)
  
  in_state_png <- file.path(
    runs_dir, "Trends", "suitability",
    sprintf("%s-Suitability-Trend-State-Analysis.png", code)
  )
  in_hot_png <- file.path(
    runs_dir, "Trends", "suitability",
    sprintf("%s-Suitability-Trend-State-Analysis-Hotspots.png", code)
  )
  in_tbl_png <- file.path(
    runs_dir, "Summaries", "tables",
    sprintf("%s-Suitability-Trend-Summary.png", code)
  )
  in_tbl_pdf <- file.path(
    runs_dir, "Summaries", "tables",
    sprintf("%s-Suitability-Trend-Summary.pdf", code)
  )
  
  out_dir <- file.path(runs_dir, "Summaries", "pages")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  base_out <- file.path(out_dir, sprintf("%s-State-Trends", code))
  out_pdf <- paste0(
    base_out,
    if (append_version_to_filename) {
      paste0("-", format(Sys.time(), "%Y%m%d-%H%M%S"))
    } else {
      ""
    },
    ".pdf"
  )
  
  # Required PNGs
  need <- c(in_state_png, in_hot_png)
  if (any(!file.exists(need))) {
    stop(
      paste0(
        "Missing required PNG(s):\n - ",
        paste(need[!file.exists(need)], collapse = "\n - ")
      ),
      call. = FALSE
    )
  }
  
  # Table: need PNG or (PDF + prefer_table_pdf)
  if (!file.exists(in_tbl_png) && !(prefer_table_pdf && file.exists(in_tbl_pdf))) {
    stop(
      "Table file missing: need PNG or set prefer_table_pdf = TRUE and provide the PDF.",
      call. = FALSE
    )
  }
  
  # ---- Geometry (inches -> pixels @ dpi) ------------------------------------
  px <- function(inches) as.integer(round(inches * dpi))
  
  page_w <- px(page_width_in)
  page_h <- px(page_height_in)
  mL <- px(margin_left_in)
  mR <- px(margin_right_in)
  mB <- px(margin_bottom_in)
  mT <- px(margin_top_in)
  hG <- px(h_gutter_in)
  vG <- px(v_gap_in)
  tblMaxH <- px(table_max_height_in)
  
  content_x0 <- mL
  content_y0 <- mT
  content_x1 <- page_w - mR
  content_y1 <- page_h - mB
  content_w  <- content_x1 - content_x0
  content_h  <- content_y1 - content_y0
  
  # ---- Read and trim maps ---------------------------------------------------
  im_state <- magick::image_read(in_state_png)
  im_hot   <- magick::image_read(in_hot_png)
  
  if (trim_maps) {
    im_state <- magick::image_trim(im_state, fuzz = fuzz)
    im_hot   <- magick::image_trim(im_hot,   fuzz = fuzz)
  }
  
  # ---- Read table (PDF preferred) ------------------------------------------
  if (prefer_table_pdf && file.exists(in_tbl_pdf)) {
    # High-DPI rasterization of the PDF to get crisp text
    im_tbl <- magick::image_read_pdf(in_tbl_pdf, density = table_pdf_dpi)
  } else {
    im_tbl <- magick::image_read(in_tbl_png)
    # Supersample PNG before fitting to reduce fuzziness
    if (table_png_supersample > 1.0) {
      im_tbl <- magick::image_scale(
        im_tbl,
        paste0(round(100 * table_png_supersample), "%")
      )
    }
  }
  
  if (trim_table) {
    im_tbl <- magick::image_trim(im_tbl, fuzz = fuzz)
  }
  
  # ---- Helpers: aspect-preserving "fit" ------------------------------------
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
  
  # ---- Decide table size first (width factor + height cap) -----------------
  tbl_target_w <- floor(content_w * table_width_factor)
  tbl_dims     <- fit_dims(magick::image_info(im_tbl), tbl_target_w, tblMaxH)
  tbl_w <- as.integer(tbl_dims["w"])
  tbl_h <- as.integer(tbl_dims["h"])
  
  im_tbl_fit <- magick::image_resize(im_tbl, paste0(tbl_w, "x", tbl_h, "!"))
  
  if (isTRUE(table_sharpen)) {
    # Try to use magick's image_sharpen if it exists and is exported;
    # otherwise fall back to a tiny rescale "nudge".
    if ("image_sharpen" %in% getNamespaceExports("magick")) {
      sharpen_fun <- get("image_sharpen", envir = asNamespace("magick"))
      im_tbl_fit <- sharpen_fun(im_tbl_fit, radius = 0.5, sigma = 0.5)
    } else {
      # Fallback: light rescale (~1%) to refresh edge interpolation
      info_now <- magick::image_info(im_tbl_fit)
      im_tbl_fit <- magick::image_resize(
        im_tbl_fit,
        paste0(
          round(info_now$width * 1.01), "x",
          round(info_now$height * 1.01), "!"
        )
      )
    }
  }
  
  # ---- Map row width factor (centered) -------------------------------------
  map_row_w <- floor(content_w * map_width_factor)
  slot_w    <- floor((map_row_w - hG) / 2)
  slot_x0   <- content_x0 + floor((content_w - map_row_w) / 2)
  left_x0   <- slot_x0
  right_x0  <- slot_x0 + slot_w + hG
  
  # ---- Fit maps with exact visible gap calculation -------------------------
  max_map_h_avail <- content_h - vG - tbl_h
  if (max_map_h_avail < px(0.5)) {
    stop(
      "Not enough vertical space: reduce table_max_height_in / v_gap_in, or margins.",
      call. = FALSE
    )
  }
  
  im_state_fit <- fit_image(im_state, slot_w, max_map_h_avail)
  im_hot_fit   <- fit_image(im_hot,   slot_w, max_map_h_avail)
  
  h1 <- magick::image_info(im_state_fit)$height
  h2 <- magick::image_info(im_hot_fit)$height
  max_map_h <- max(h1, h2)
  
  maps_y0  <- content_y0
  table_y0 <- maps_y0 + max_map_h + vG
  
  if (table_y0 + tbl_h > content_y1) {
    stop(
      "Layout overflow: lower v_gap_in or table_max_height_in, or increase bottom margin.",
      call. = FALSE
    )
  }
  
  # ---- Canvas --------------------------------------------------------------
  canvas <- magick::image_blank(
    width  = page_w,
    height = page_h,
    color  = "white"
  )
  
  # ---- Optional debug frames ----------------------------------------------
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
    
    canvas <- draw_frame(canvas, left_x0,  maps_y0, slot_w, h1)
    canvas <- draw_frame(canvas, right_x0, maps_y0, slot_w, h2)
    canvas <- draw_frame(canvas,
                         content_x0 + floor((content_w - tbl_w) / 2),
                         table_y0,
                         tbl_w,
                         tbl_h)
  }
  
  # ---- Composite (top-aligned maps; centered table) ------------------------
  place_top_center <- function(base, overlay, slot_x0, slot_y0, slot_w) {
    oi <- magick::image_info(overlay)
    dx <- slot_x0 + floor((slot_w - oi$width) / 2)
    magick::image_composite(
      base,
      overlay,
      offset = sprintf("+%d+%d", dx, slot_y0)
    )
  }
  
  canvas <- place_top_center(canvas, im_state_fit, left_x0,  maps_y0, slot_w)
  canvas <- place_top_center(canvas, im_hot_fit,   right_x0, maps_y0, slot_w)
  
  t_dx <- content_x0 + floor((content_w - tbl_w) / 2)
  canvas <- magick::image_composite(
    canvas,
    im_tbl_fit,
    offset = sprintf("+%d+%d", t_dx, table_y0)
  )
  
  # ---- Output --------------------------------------------------------------
  magick::image_write(canvas, path = out_pdf, format = "pdf")
  message("Wrote: ", out_pdf)
  
  invisible(out_pdf)
}