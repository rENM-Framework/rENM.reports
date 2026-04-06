#' Assemble 3x3 range map contact sheet
#'
#' Builds a 3x3 contact sheet of range maps for a species code by
#' collecting PNG tiles from TimeSeries outputs and arranging them on
#' a standardized Letter-sized page layout for export to multiple formats.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline 
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Pipeline context}
#' Assembles retrospective ENM range map outputs into a standardized
#' multi-format visual summary for interpretation and reporting.
#'
#' \strong{Inputs}
#' Source tiles are expected at:
#'
#' \code{<project_dir>/runs/<alpha_code>/TimeSeries/}
#' \code{<year>/model/}
#' \code{<alpha_code>-<year>-Range.png}
#'
#' for year in 1980, 1985, ..., 2020 (9 images total).
#'
#' \strong{Processing steps}
#' \itemize{
#'   \item Reads and trims each tile to remove stray white borders using
#'   \code{magick::image_trim}.
#'   \item Creates a 3x3 montage with small gutters.
#'   \item Trims the montage again to tighten outer bounds.
#'   \item Scales the montage to the content width (6.5 inches at 300 dpi).
#'   \item If the montage exceeds the content height (9 inches at 300 dpi),
#'   crops excess from the bottom while preserving the top.
#'   \item Composites onto a white Letter-sized page.
#' }
#'
#' \strong{Outputs}
#' The final page is exported as:
#' \itemize{
#'   \item A one-page PNG (<alpha_code>-Range-Maps.png)
#'   \item A one-page PDF (<alpha_code>-Range-Maps.pdf)
#'   \item A one-page DOCX (<alpha_code>-Range-Maps.docx)
#' }
#'
#' All files are written to:
#'
#' \code{<project_dir>/runs/<alpha_code>/Summaries/maps}
#'
#' The DOCX placement matches the PNG/PDF visually: the montage is scaled
#' to the page content width (page width minus 2 x margin) and cropped from
#' the bottom if it exceeds the content height, preserving the top of the
#' montage so it sits 1 inch from the top margin and is centered horizontally.
#'
#' \strong{Data requirements}
#' Exactly 9 range PNG files must exist for the specified alpha_code.
#'
#' @param alpha_code Character. Species code used in folder and file names.
#' @param margin_in Numeric. Page margin on top, left, and right in inches.
#' @param page_width_in Numeric. Page width in inches (Letter = 8.5).
#' @param page_height_in Numeric. Page height in inches (Letter = 11).
#' @param tile Character. Montage tile specification for
#'   magick::image_montage.
#' @param trim_fuzz Numeric. Trim tolerance percent for
#'   magick::image_trim to remove light halos.
#' @param dpi Numeric. Rendering resolution for raster-based layout.
#'
#' @return
#' A named list returned invisibly with the following elements:
#' \itemize{
#'   \item png: Character. Absolute file path to PNG output.
#'   \item pdf: Character. Absolute file path to PDF output.
#'   \item docx: Character. Absolute file path to DOCX output.
#' }
#'
#' Side effects:
#' \itemize{
#'   \item Writes PNG, PDF, and DOCX files to the summaries directory.
#'   \item Appends a processing summary to:
#'   \code{<project_dir>/runs/<alpha_code>/_log.txt}
#' }
#'
#' @importFrom magick image_read image_trim image_montage image_join
#'   image_info image_scale image_crop image_blank image_composite
#'   image_write
#' @importFrom officer read_docx prop_section page_size page_mar
#'   body_set_default_section external_img fpar fp_par body_add_fpar
#'
#' @examples
#' \dontrun{
#' # For BETH using default layout and margins:
#' gather_range_maps("BETH")
#'
#' # For a different alpha_code:
#' gather_range_maps("AMRO")
#'
#' # Tighter trim if input PNGs have larger white borders:
#' gather_range_maps("BETH", trim_fuzz = 12)
#' }
#'
#' @export
gather_range_maps <- function(alpha_code,
                              margin_in = 1,
                              page_width_in = 8.5,
                              page_height_in = 11,
                              tile = "3x3",
                              trim_fuzz = 8,
                              dpi = 300) {

  t_start <- Sys.time()
  code <- toupper(alpha_code)

  cat("------------------------------------------------------------------------\n")
  cat(sprintf("gather_range_maps(): Starting composite build for %s\n", code))

  # Dependency checks (packages should be listed in DESCRIPTION Imports)
  for (pkg in c("magick", "officer")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is required but not installed. Please run install.packages('%s').",
        pkg, pkg
      ), call. = FALSE)
    }
  }
  magick  <- asNamespace("magick")
  officer <- asNamespace("officer")

  # Project directory (CRAN-compliant)
  project_dir <- rENM_project_dir()

  # Output directory
  out_dir <- file.path(project_dir, "runs", code, "Summaries", "maps")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    cat("Created output directory:", out_dir, "\n")
  }

  # Build expected source list
  years <- seq(1980, 2020, by = 5)
  src_pngs <- file.path(
    project_dir, "runs", code, "TimeSeries",
    years, "model", sprintf("%s-%d-Range.png", code, years)
  )

  cat("Checking source tiles (9 expected)...\n")
  missing <- src_pngs[!file.exists(src_pngs)]
  if (length(missing) > 0) {
    stop(sprintf(
      "Missing %d expected range map(s) for '%s':\n%s",
      length(missing), code, paste(missing, collapse = "\n")
    ), call. = FALSE)
  }

  cat("Reading and trimming source images...\n")
  imgs <- lapply(src_pngs, function(p) {
    im <- magick$image_read(p)
    magick$image_trim(im, fuzz = trim_fuzz)
  })

  cat("Compositing 3x3 montage...\n")
  contact <- magick$image_montage(
    magick$image_join(imgs),
    tile     = tile,
    geometry = "+5+5",
    bg       = "white"
  )
  contact <- magick$image_trim(contact, fuzz = trim_fuzz)

  # Geometry
  full_w_px    <- round(page_width_in  * dpi)
  full_h_px    <- round(page_height_in * dpi)
  content_w_px <- round((page_width_in  - 2 * margin_in) * dpi)
  content_h_px <- round((page_height_in - 2 * margin_in) * dpi)

  cat("Scaling montage to content width and cropping if needed...\n")
  info <- magick$image_info(contact)
  scaled_height  <- round(info$height * (content_w_px / info$width))
  contact_scaled <- magick$image_scale(contact, sprintf("%dx%d", content_w_px, scaled_height))
  contact_cropped <- if (scaled_height > content_h_px) {
    magick$image_crop(
      contact_scaled,
      geometry = sprintf("%dx%d+0+0", content_w_px, content_h_px)
    )
  } else {
    contact_scaled
  }

  cat("Compositing on Letter-sized page...\n")
  page <- magick$image_blank(width = full_w_px, height = full_h_px, color = "white")
  page <- magick$image_composite(
    page,
    contact_cropped,
    offset = paste0("+", margin_in * dpi, "+", margin_in * dpi)
  )

  # Outputs
  base_name <- file.path(out_dir, sprintf("%s-Range-Maps", code))
  png_out  <- sprintf("%s.png",  base_name)
  pdf_out  <- sprintf("%s.pdf",  base_name)
  docx_out <- sprintf("%s.docx", base_name)

  cat("Saving PNG and PDF...\n")
  magick$image_write(page, path = png_out, format = "png")
  magick$image_write(page, path = pdf_out, format = "pdf")

  cat("Embedding montage in DOCX...\n")
  doc <- officer$read_docx()
  sect <- officer$prop_section(
    page_size = officer$page_size(
      orient = "portrait",
      width = page_width_in,
      height = page_height_in
    ),
    page_margins = officer$page_mar(
      top = margin_in,
      bottom = margin_in,
      left = margin_in,
      right = margin_in
    )
  )
  doc <- officer$body_set_default_section(doc, value = sect)

  tmp_png <- tempfile(fileext = ".png")
  on.exit({
    if (file.exists(tmp_png)) unlink(tmp_png)
  }, add = TRUE)
  magick$image_write(contact_cropped, path = tmp_png, format = "png")

  img_block <- officer$external_img(
    src    = tmp_png,
    width  = (content_w_px / dpi),
    height = (magick$image_info(contact_cropped)$height / dpi)
  )
  fp <- officer$fpar(img_block, fp_p = officer$fp_par(text.align = "center"))
  doc <- officer$body_add_fpar(doc, fp)
  print(doc, target = docx_out)

  cat("All outputs saved successfully.\n")
  cat("  -", png_out, "\n")
  cat("  -", pdf_out, "\n")
  cat("  -", docx_out, "\n")

  # eBird-standard log append
  t_end <- Sys.time()
  elapsed <- difftime(t_end, t_start, units = "secs")
  s <- as.numeric(elapsed)
  hh <- sprintf("%02d", s %/% 3600); s <- s %% 3600
  mm <- sprintf("%02d", s %/% 60);   ss <- sprintf("%02d", round(s %% 60))
  fmt_elapsed <- paste0(hh, ":", mm, ":", ss)

  log_file <- file.path(project_dir, "runs", code, "_log.txt")
  sep_line <- paste(rep("-", 72), collapse = "")
  header <- "Processing summary (gather_range_maps)"
  ts_str <- format(t_end, "%Y-%m-%d %H:%M:%S %Z")
  f <- function(k, v) sprintf("%-18s : %s", k, v)

  outputs <- c(png_out, pdf_out, docx_out)
  log_block <- c(
    "",
    sep_line,
    header,
    f("Timestamp", ts_str),
    f("Alpha code", code),
    f("Years", "1980-2020 (5-yr steps)"),
    f("Grid", "3x3"),
    f("DPI", dpi),
    f("Margin (in)", margin_in),
    f("Tile spec", tile),
    f("Trim fuzz (%)", trim_fuzz),
    f("Outputs saved", length(outputs)),
    vapply(outputs, function(x) paste0(" - ", x), character(1)),
    f("Total elapsed", fmt_elapsed),
    ""
  )

  try({
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    cat(paste0(log_block, collapse = "\n"), file = log_file, append = TRUE)
  }, silent = TRUE)

  cat("------------------------------------------------------------------------\n")
  cat("Processing summary written to log file.\n")

  invisible(list(png = png_out, pdf = pdf_out, docx = docx_out))
}
