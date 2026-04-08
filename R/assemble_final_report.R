#' Assemble a final multi-page report for an rENM run
#'
#' Combines a predefined set of per-page PDF products into a single
#' report PDF for a given species run. Optionally rasterizes the pages
#' and draws centered page numbers on all but the cover page.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Pre-generated page PDFs in
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Summaries/pages/}
#'         (nine files, in the exact order listed):
#'         \itemize{
#'           \item \code{<alpha_code>-Suitability-Trend-Analysis.pdf}
#'           \item \code{<alpha_code>-Suitability-Trends.pdf}
#'           \item \code{<alpha_code>-State-Trends.pdf}
#'           \item \code{<alpha_code>-Centroid-Trends.pdf}
#'           \item \code{<alpha_code>-Suitability-TimeSeries.pdf}
#'           \item \code{<alpha_code>-Range-TimeSeries.pdf}
#'           \item \code{<alpha_code>-Variable-Trends.pdf}
#'           \item \code{<alpha_code>-Variable-Trend-Maps1.pdf}
#'           \item \code{<alpha_code>-Variable-Trend-Maps2.pdf}
#'         }
#' }
#'
#' \strong{Outputs}
#' \itemize{
#'   \item Final report:
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Summaries/
#'         <alpha_code>-Final-Report.pdf}
#'   \item Processing summary appended to
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/_log.txt}
#' }
#'
#' \strong{Methods}
#' \itemize{
#'   \item \code{pdftools::pdf_combine()} merges component PDFs.
#'   \item When \code{page_numbers = TRUE} the merged PDF is rasterized
#'         with \code{pdftools::pdf_convert()}, read by
#'         \code{png::readPNG()}, redrawn via base graphics, and saved as
#'         a numbered PDF.
#' }
#'
#' \strong{Dependencies}
#' \itemize{
#'   \item \pkg{pdftools} for PDF I/O and rasterization.
#'   \item \pkg{png} for reading rasterized pages when numbering is
#'         requested.
#' }
#'
#' @param alpha_code Character. Four-letter species alpha code that
#'   identifies the run, for example \code{"BETH"}.
#' @param page_numbers Logical. If \code{TRUE} (default), draw centered
#'   page numbers on all pages except the first. If \code{FALSE}, simply
#'   combine the PDFs without numbering.
#' @param dpi Numeric. Rasterization resolution (dots per inch) used
#'   when \code{page_numbers = TRUE}. Ignored otherwise. Typical values
#'   range from 120 to 300.
#' @param cleanup Logical. If \code{TRUE} (default), delete temporary
#'   files generated during rasterization. Ignored when
#'   \code{page_numbers = FALSE}.
#' @param verbose Logical. If \code{TRUE} (default), print progress
#'   messages.
#'
#' @return Character. Invisibly returns the absolute path to the final
#'   report PDF.
#'   \itemize{
#'     \item Writes the combined report PDF.
#'     \item Appends a formatted processing summary to the run log.
#'   }
#'
#' @importFrom pdftools pdf_combine pdf_length pdf_convert
#' @importFrom png readPNG
#'
#' @examples
#' \dontrun{
#' # Default: combine pages and add numbers
#' assemble_final_report("BETH")
#'
#' # Fast combine without numbering
#' assemble_final_report("BETH", page_numbers = FALSE)
#'
#' # High-resolution numbering
#' assemble_final_report("BETH", dpi = 300)
#' }
#'
#' @export
assemble_final_report <- function(alpha_code,
                                  page_numbers = TRUE,
                                  dpi = 150,
                                  cleanup = TRUE,
                                  verbose = TRUE) {
  # -------------------------------------------------------------------
  # Start timing
  # -------------------------------------------------------------------
  start_time <- Sys.time()

  # -------------------------------------------------------------------
  # Check for required packages
  # -------------------------------------------------------------------
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required but not installed. Install with install.packages('pdftools').")
  }
  if (page_numbers && !requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required when page_numbers = TRUE. Install with install.packages('png').")
  }

  # -------------------------------------------------------------------
  # Determine project root (CRAN-compliant: no hard-coded paths)
  # -------------------------------------------------------------------
  project_dir <- rENM_project_dir()

  # -------------------------------------------------------------------
  # Define key paths
  # -------------------------------------------------------------------
  # Base summaries directory for this alpha code
  base_dir  <- file.path(project_dir, "runs", alpha_code, "Summaries")
  # Run directory root
  run_dir   <- file.path(project_dir, "runs")
  # Directory where component page PDFs live
  pages_dir <- file.path(base_dir, "pages")
  # Final report output path
  out_path  <- file.path(base_dir, sprintf("%s-Final-Report.pdf", alpha_code))
  # Log file (eBird-standard log style) for this alpha code
  log_path  <- file.path(run_dir, alpha_code, "_log.txt")

  # Ensure summary directory exists (pages_dir is assumed to exist already)
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  # -------------------------------------------------------------------
  # Construct the list of expected input PDFs in the desired order
  # -------------------------------------------------------------------
  page_files <- c(
    sprintf("%s-Suitability-Trend-Analysis.pdf", alpha_code),
    sprintf("%s-Suitability-Trends.pdf",          alpha_code),
    sprintf("%s-State-Trends.pdf",                alpha_code),
    sprintf("%s-Centroid-Trends.pdf",             alpha_code),
    sprintf("%s-Suitability-TimeSeries.pdf",      alpha_code),
    sprintf("%s-Range-TimeSeries.pdf",            alpha_code),
    sprintf("%s-Variable-Trends.pdf",             alpha_code),
    sprintf("%s-Variable-Trend-Maps1.pdf",        alpha_code),
    sprintf("%s-Variable-Trend-Maps2.pdf",        alpha_code)
  )

  input_paths <- file.path(pages_dir, page_files)

  # -------------------------------------------------------------------
  # Console: report starting state
  # -------------------------------------------------------------------
  if (verbose) {
    message("[assemble_final_report] Starting final report assembly for alpha_code = '", alpha_code, "'.")
    message("  - Summaries directory : ", base_dir)
    message("  - Pages directory     : ", pages_dir)
    message("  - Output file         : ", out_path)
    message("  - Page numbers        : ", page_numbers)
    if (page_numbers) {
      message("  - Rasterization DPI   : ", dpi)
    }
  }

  # -------------------------------------------------------------------
  # Verify all expected input PDFs exist
  # -------------------------------------------------------------------
  missing <- input_paths[!file.exists(input_paths)]
  if (length(missing) > 0) {
    stop(
      "The following input PDF(s) are missing:\n",
      paste(missing, collapse = "\n")
    )
  }

  if (verbose) {
    message("[assemble_final_report] All input PDFs found (", length(input_paths), " files).")
  }

  # -------------------------------------------------------------------
  # Branch 1: page_numbers = FALSE (fast, combine-only path)
  # -------------------------------------------------------------------
  if (!page_numbers) {
    if (verbose) {
      message("[assemble_final_report] Page numbering disabled; performing fast combine-only operation.")
    }

    # Directly combine component PDFs into final report (no rasterization)
    pdftools::pdf_combine(input_paths, output = out_path)

    # Determine number of pages in the final combined PDF
    n_pages <- pdftools::pdf_length(out_path)

    if (verbose) {
      message("[assemble_final_report] Combined PDF created with ", n_pages, " page(s).")
      message("[assemble_final_report] Final report written to: ", out_path)
    }

    # -----------------------------------------------------------------
    # Write log entry (eBird-standard format)
    # -----------------------------------------------------------------
    end_time    <- Sys.time()
    elapsed_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
    timestamp   <- format(end_time, "%Y-%m-%d %H:%M:%S %Z")

    sep_line <- strrep("-", 72L)

    log_lines <- c(
      "", sep_line,
      "Processing summary (assemble_final_report)",
      sprintf("Timestamp       : %s", timestamp),
      sprintf("Alpha code      : %s", alpha_code),
      sprintf("Input PDFs      : %d", length(input_paths)),
      sprintf("Page numbers    : %s", "FALSE"),
      sprintf("DPI             : %s", "NA"),
      sprintf("Total pages     : %d", n_pages),
      sprintf("Outputs saved   : %d", 1L),
      sprintf("Total elapsed   : %.3f s", elapsed_sec),
      sprintf("Output file     : %s", out_path)
    )

    cat(paste(log_lines, collapse = "\n"), "\n",
        file = log_path, append = TRUE)

    if (verbose) {
      message("[assemble_final_report] Log entry appended to: ", log_path)
    }

    return(invisible(out_path))
  }

  # -------------------------------------------------------------------
  # Branch 2: page_numbers = TRUE (rasterization + overlay path)
  # -------------------------------------------------------------------

  if (verbose) {
    message("[assemble_final_report] Page numbering enabled; combining PDFs then rasterizing to PNG.")
  }

  # Temporary combined PDF without page numbers
  tmp_pdf <- file.path(tempdir(), sprintf("%s-Final-Report-raw.pdf", alpha_code))

  # Combine component PDFs into the temporary PDF
  pdftools::pdf_combine(input_paths, output = tmp_pdf)

  # Determine number of pages in the combined PDF
  n_pages <- pdftools::pdf_length(tmp_pdf)
  if (is.na(n_pages) || n_pages == 0L) {
    stop("Combined PDF appears to have zero pages; check input files.")
  }

  if (verbose) {
    message("[assemble_final_report] Combined intermediate PDF has ", n_pages, " page(s).")
    message("[assemble_final_report] Converting combined PDF to PNG (DPI = ", dpi, ").")
  }

  # Convert each page of the combined PDF to a PNG file
  png_files <- file.path(
    tempdir(),
    sprintf("%s-page-%02d.png", alpha_code, seq_len(n_pages))
  )

  pdftools::pdf_convert(
    pdf       = tmp_pdf,
    format    = "png",
    pages     = 1:n_pages,
    filenames = png_files,
    dpi       = dpi,
    verbose   = FALSE
  )

  if (verbose) {
    message("[assemble_final_report] PNG conversion complete (", length(png_files), " files).")
  }

  # Use the first PNG to determine page dimensions (inches) for the output PDF
  first_img <- png::readPNG(png_files[1])
  dims      <- dim(first_img)  # [height_px, width_px, channels]
  height_px <- dims[1]
  width_px  <- dims[2]

  width_in  <- width_px  / dpi
  height_in <- height_px / dpi

  if (verbose) {
    message("[assemble_final_report] Output PDF page size: ",
            sprintf("%.2f x %.2f inches (W x H)", width_in, height_in))
    message("[assemble_final_report] Writing final PDF with page numbers...")
  }

  # Open PDF device for the final output
  grDevices::pdf(out_path, width = width_in, height = height_in)
  on.exit(grDevices::dev.off(), add = TRUE)

  # Loop over pages: draw each PNG as a full-page raster, then add page numbers
  for (i in seq_len(n_pages)) {
    img <- png::readPNG(png_files[i])

    # Set margins to zero and draw a blank 0–1 plotting region
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot(
      0:1, 0:1, type = "n",
      xlab = "", ylab = "",
      axes = FALSE, xaxs = "i", yaxs = "i"
    )

    # Draw the page image to occupy the full plotting region
    graphics::rasterImage(img, xleft = 0, ybottom = 0, xright = 1, ytop = 1)

    # Overlay a centered page number at the bottom, skipping the first page
    if (i > 1L) {
      graphics::text(
        x      = 0.5,
        y      = 0.03,  # adjust as needed
        labels = i,
        cex    = 0.9
      )
    }

    if (verbose) {
      message("[assemble_final_report] Processed page ", i, " of ", n_pages, ".")
    }
  }

  # Close PDF device (handled by on.exit) and optionally remove temporary files
  if (cleanup) {
    if (verbose) {
      message("[assemble_final_report] Cleaning up temporary files.")
    }
    unlink(c(tmp_pdf, png_files), recursive = FALSE, force = TRUE)
  }

  if (verbose) {
    message("[assemble_final_report] Final report with page numbers written to: ", out_path)
  }

  # -------------------------------------------------------------------
  # Write log entry (eBird-standard format)
  # -------------------------------------------------------------------
  end_time    <- Sys.time()
  elapsed_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
  timestamp   <- format(end_time, "%Y-%m-%d %H:%M:%S %Z")

  sep_line <- strrep("-", 72L)

  log_lines <- c(
    "", sep_line,
    "Processing summary (assemble_final_report)",
    sprintf("Timestamp       : %s", timestamp),
    sprintf("Alpha code      : %s", alpha_code),
    sprintf("Input PDFs      : %d", length(input_paths)),
    sprintf("Page numbers    : %s", "TRUE"),
    sprintf("DPI             : %d", dpi),
    sprintf("Total pages     : %d", n_pages),
    sprintf("Outputs saved   : %d", 1L),
    sprintf("Total elapsed   : %.3f s", elapsed_sec),
    sprintf("Output file     : %s", out_path)
  )

  cat(paste(log_lines, collapse = "\n"), "\n",
      file = log_path, append = TRUE)

  if (verbose) {
    message("[assemble_final_report] Log entry appended to: ", log_path)
  }

  invisible(out_path)
}
