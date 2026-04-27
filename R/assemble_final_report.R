#' Assemble a final report for an rENM run
#'
#' Combines a predefined or user-specified set of per-page PDF products
#' into a single report PDF for a given species run. Optionally
#' rasterizes the pages and draws centered page numbers on all but
#' the cover page.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' rENM_project_dir().
#'
#' By default, a fixed set of pages is assembled in a predefined order.
#' Users may override this behavior via the \code{pages} argument to:
#' \itemize{
#'   \item reorder pages
#'   \item omit pages
#'   \item include a subset of pages
#' }
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Pre-generated page PDFs in
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Summaries/pages/}
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
#' @param alpha_code Character. Four-letter species alpha code.
#' @param pages Character vector. Optional. Specifies which pages to
#'   include and their order. May be either:
#'   \itemize{
#'     \item full filenames (e.g., "BETH-Suitability-Trends.pdf"), or
#'     \item short keys (see Details; e.g., "Suitability-Trends")
#'   }
#'   If \code{NULL} (default), the standard page set and order are used.
#' @param page_numbers Logical. Add page numbers (default TRUE).
#' @param dpi Numeric. Rasterization resolution when numbering.
#' @param cleanup Logical. Remove temporary files.
#' @param verbose Logical. Print progress messages.
#'
#' @return Character. Invisibly returns the final PDF path.
#'
#' @importFrom pdftools pdf_combine pdf_length pdf_convert
#' @importFrom png readPNG
#'
#' @examples
#' \dontrun{
#' assemble_final_report("BETH")
#'
#' # Reorder and omit pages
#' assemble_final_report(
#'   "BETH",
#'   pages = c(
#'     "Suitability-Trend-Analysis",
#'     "Suitability-Trends",
#'     "Centroid-Trends"
#'   )
#' )
#' }
#'
#' @export
assemble_final_report <- function(alpha_code,
                                  pages = NULL,
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
    stop("Package 'pdftools' is required but not installed.")
  }
  if (page_numbers && !requireNamespace("png", quietly = TRUE)) {
    stop("Package 'png' is required when page_numbers = TRUE.")
  }
  
  # -------------------------------------------------------------------
  # Determine project root
  # -------------------------------------------------------------------
  project_dir <- rENM_project_dir()
  
  # -------------------------------------------------------------------
  # Define key paths
  # -------------------------------------------------------------------
  base_dir  <- file.path(project_dir, "runs", alpha_code, "Summaries")
  run_dir   <- file.path(project_dir, "runs")
  pages_dir <- file.path(base_dir, "pages")
  out_path  <- file.path(base_dir, sprintf("%s-Final-Report.pdf", alpha_code))
  log_path  <- file.path(run_dir, alpha_code, "_log.txt")
  
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  # -------------------------------------------------------------------
  # Default page definitions (canonical order)
  # -------------------------------------------------------------------
  default_pages <- c(
    "Suitability-Trend-Analysis",
    "Suitability-Trends",
    "Suitability-TimeSeries",
    "Centroid-Trends",
    "State-Trends",
    # "Range-TimeSeries",
    "Variable-Trends",
    "Variable-Trend-Maps1",
    "Variable-Trend-Maps2"
  )
  
  # -------------------------------------------------------------------
  # Resolve pages (NEW — surgical insertion)
  # -------------------------------------------------------------------
  if (is.null(pages)) {
    page_keys <- default_pages
  } else {
    page_keys <- pages
  }
  
  # If user passed full filenames, keep them; otherwise construct names
  if (all(grepl("\\.pdf$", page_keys))) {
    page_files <- page_keys
  } else {
    page_files <- sprintf("%s-%s.pdf", alpha_code, page_keys)
  }
  
  input_paths <- file.path(pages_dir, page_files)
  
  # -------------------------------------------------------------------
  # Console: report starting state
  # -------------------------------------------------------------------
  if (verbose) {
    message("[assemble_final_report] Starting final report assembly for '", alpha_code, "'.")
    message("  - Pages selected     : ", length(input_paths))
    message("  - Page numbers       : ", page_numbers)
  }
  
  # -------------------------------------------------------------------
  # Verify inputs exist
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
