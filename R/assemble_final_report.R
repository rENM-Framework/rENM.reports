#' Assemble a final report for an rENM run
#'
#' Combines a predefined or user-specified set of per-page PDF products
#' into a single report for a given species run. Produces a vector PDF
#' (page numbers stamped via cpdf, no rasterization) and optionally a
#' .docx version. Optionally prepends a front matter PDF and/or appends
#' an appendix PDF sourced from rENM.reports.
#'
#' @details
#' By default, a fixed set of pages is assembled in a predefined order.
#' Users may override this behavior via the \code{pages} argument to:
#' \itemize{
#'   \item reorder pages
#'   \item omit pages
#'   \item include a subset of pages
#' }
#'
#' Optional front matter and appendix PDFs must be located in
#' \code{inst/resources/} within the rENM.reports package.
#'
#' Page numbers are stamped directly onto the combined PDF using
#' \code{cpdf} (Coherent PDF Tools), preserving full vector quality
#' without rasterization. \code{cpdf} must be on the system PATH;
#' install via \code{brew install cpdf} on macOS.
#'
#' When \code{docx = TRUE}, each page is rasterized to PNG and inserted
#' into a Word document via the \code{officer} package. Rasterization is
#' unavoidable for the .docx format (Word cannot embed PDF vector
#' content), so it only runs when explicitly requested.
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Pre-generated page PDFs in
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Summaries/pages/}
#'   \item Optional front matter / appendix PDFs in
#'         \code{rENM.reports::inst/resources/}
#' }
#'
#' \strong{Outputs}
#' \itemize{
#'   \item Final PDF report:
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Summaries/
#'         <alpha_code>-Final-Report.pdf}
#'   \item Optional Word report (when \code{docx = TRUE}):
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Summaries/
#'         <alpha_code>-Final-Report.docx}
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
#' @param front_matter Character. Optional. Filename of a PDF located in
#'   \code{rENM.reports::inst/resources/} to prepend.
#' @param appendix Character. Optional. Filename of a PDF located in
#'   \code{rENM.reports::inst/resources/} to append.
#' @param page_numbers Logical. Stamp page numbers on the PDF (default
#'   TRUE). Cover page (page 1) is always left unnumbered.
#' @param docx Logical. Also produce a .docx version (default FALSE).
#'   Requires the \code{officer} and \code{png} packages. Pages are
#'   rasterized to PNG at \code{dpi} resolution for Word embedding.
#' @param dpi Numeric. Rasterization resolution used only when
#'   \code{docx = TRUE} (default 150).
#' @param cleanup Logical. Remove temporary files (default TRUE).
#' @param verbose Logical. Print progress messages (default TRUE).
#'
#' @return Character vector. Invisibly returns the path(s) to the
#'   output file(s): the PDF path always first, the .docx path second
#'   when \code{docx = TRUE}.
#'
#' @importFrom pdftools pdf_combine pdf_length pdf_convert
#' @importFrom png readPNG
#' @importFrom officer read_docx prop_section page_size page_mar body_set_default_section body_add_break body_add_img
#'
#' @examples
#' \dontrun{
#' assemble_final_report("BETH")
#'
#' assemble_final_report(
#'   "BETH",
#'   front_matter = "Front-Matter.pdf",
#'   appendix     = "Appendix.pdf",
#'   docx         = TRUE
#' )
#' }
#'
#' @export
assemble_final_report <- function(alpha_code,
                                  pages        = NULL,
                                  front_matter = NULL,
                                  appendix     = "variables.pdf",
                                  page_numbers = TRUE,
                                  docx         = FALSE,
                                  dpi          = 150,
                                  cleanup      = TRUE,
                                  verbose      = TRUE) {
  # -------------------------------------------------------------------
  # Start timing
  # -------------------------------------------------------------------
  start_time <- Sys.time()

  # -------------------------------------------------------------------
  # Check required packages and system tools
  # -------------------------------------------------------------------
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("Package 'pdftools' is required but not installed.")
  }
  if (page_numbers) {
    cpdf_path <- Sys.which("cpdf")
    if (cpdf_path == "") {
      stop(
        "cpdf is required for page numbering but was not found on PATH.\n",
        "Install via: brew install cpdf"
      )
    }
  }
  if (docx) {
    if (!requireNamespace("officer", quietly = TRUE)) {
      stop("Package 'officer' is required when docx = TRUE.")
    }
    if (!requireNamespace("png", quietly = TRUE)) {
      stop("Package 'png' is required when docx = TRUE.")
    }
  }

  # -------------------------------------------------------------------
  # Paths
  # -------------------------------------------------------------------
  project_dir <- rENM_project_dir()
  base_dir    <- file.path(project_dir, "runs", alpha_code, "Summaries")
  pages_dir   <- file.path(base_dir, "pages")
  pdf_out     <- file.path(base_dir, sprintf("%s-Final-Report.pdf", alpha_code))
  docx_out    <- file.path(base_dir, sprintf("%s-Final-Report.docx", alpha_code))
  log_path    <- file.path(project_dir, "runs", alpha_code, "_log.txt")

  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

  # -------------------------------------------------------------------
  # Default page definitions (canonical order)
  # -------------------------------------------------------------------
  default_pages <- c(
    "Suitability-Trend-Analysis",
    "Suitability-TimeSeries",
    "Range-TimeSeries",
    "Suitability-Trends",
    "State-Trends",
    "Centroid-Trends",
    "Variable-Trends",
    "Variable-Trend-Maps1",
    "Variable-Trend-Maps2"
  )

  # -------------------------------------------------------------------
  # Resolve page file list
  # -------------------------------------------------------------------
  page_keys <- if (is.null(pages)) default_pages else pages

  if (all(grepl("\\.pdf$", page_keys))) {
    page_files <- page_keys
  } else {
    page_files <- sprintf("%s-%s.pdf", alpha_code, page_keys)
  }

  input_paths <- file.path(pages_dir, page_files)

  # -------------------------------------------------------------------
  # Resolve optional front matter and appendix
  # -------------------------------------------------------------------
  get_resource_pdf <- function(filename) {
    if (is.null(filename)) return(NULL)
    path <- system.file("resources", filename, package = "rENM.reports")
    if (path == "") {
      stop(sprintf("Resource PDF not found in rENM.reports: %s", filename))
    }
    path
  }

  input_paths <- c(
    get_resource_pdf(front_matter),
    input_paths,
    get_resource_pdf(appendix)
  )

  # -------------------------------------------------------------------
  # Verify inputs exist
  # -------------------------------------------------------------------
  missing_files <- input_paths[!file.exists(input_paths)]
  if (length(missing_files) > 0L) {
    stop("The following input PDF(s) are missing:\n",
         paste(missing_files, collapse = "\n"))
  }

  if (verbose) {
    message("[assemble_final_report] Starting final report assembly for '",
            alpha_code, "'.")
    message("  - Pages selected : ", length(input_paths))
    message("  - Front matter   : ",
            ifelse(is.null(front_matter), "NONE", front_matter))
    message("  - Appendix       : ",
            ifelse(is.null(appendix), "NONE", appendix))
    message("  - Page numbers   : ", page_numbers)
    message("  - DOCX output    : ", docx)
    message("[assemble_final_report] All input PDFs found.")
  }

  # -------------------------------------------------------------------
  # Step 1: Combine all input PDFs into one
  # -------------------------------------------------------------------
  tmp_combined <- file.path(
    tempdir(), sprintf("%s-combined.pdf", alpha_code)
  )

  pdftools::pdf_combine(input_paths, output = tmp_combined)
  n_pages <- pdftools::pdf_length(tmp_combined)

  if (is.na(n_pages) || n_pages == 0L) {
    stop("Combined PDF appears to have zero pages; check input files.")
  }

  if (verbose) {
    message("[assemble_final_report] Combined PDF has ", n_pages, " page(s).")
  }

  # -------------------------------------------------------------------
  # Step 2: Normalize all pages to letter size (612 x 792 pt)
  #
  # Input pages may have different PDF coordinate dimensions — e.g.
  # raster-embedded pages at 300 dpi have a point-space size of
  # 2550 x 3300 rather than 612 x 792. cpdf -scale-to-fit rescales
  # all pages to a uniform size so the document displays consistently
  # and page-number stamping can use a single fixed font size.
  #
  # Step 3: Stamp page numbers with cpdf (vector, no rasterization)
  #
  # Page 1 (cover) is left unnumbered via the "2-end" range.
  # All pages are now 612 x 792 pt so a fixed font size is correct.
  #
  # NOTE: cpdf runs as a subprocess and cannot write directly to
  # CloudStorage / Dropbox paths (macOS TCC permission boundary).
  # All cpdf operations stay in tempdir(); the final result is moved
  # to pdf_out via file.copy(), which runs in-process and has access.
  # -------------------------------------------------------------------
  tmp_normalized <- file.path(tempdir(),
                               sprintf("%s-normalized.pdf", alpha_code))

  cpdf_run <- function(...) {
    args <- c(...)
    ret  <- system2("cpdf", args, stdout = TRUE, stderr = TRUE)
    code <- attr(ret, "status")
    if (!is.null(code) && code != 0L) {
      stop("cpdf failed (exit ", code, "): ", paste(ret, collapse = " "))
    }
    invisible(ret)
  }

  if (verbose) {
    message("[assemble_final_report] Normalizing all pages to letter size.")
  }

  cpdf_run("-scale-to-fit", shQuote("612 792"), shQuote(tmp_combined), "all",
           "-o", shQuote(tmp_normalized))

  if (!file.exists(tmp_normalized)) {
    stop("cpdf normalization produced no output: ", tmp_normalized)
  }

  if (page_numbers && n_pages > 1L) {
    if (verbose) {
      message("[assemble_final_report] Stamping page numbers with cpdf ",
              "(pages 2-", n_pages, ").")
    }

    cpdf_run("-add-text", "%Page",
             "-bottom", "20",
             "-font", "Helvetica",
             "-font-size", "10",
             shQuote(tmp_normalized), "2-end",
             "-o", shQuote(pdf_out))

  } else {
    file.copy(tmp_normalized, pdf_out, overwrite = TRUE)
  }

  if (cleanup) unlink(tmp_normalized)

  if (!file.exists(pdf_out)) {
    stop("Final PDF was not created at: ", pdf_out)
  }

  if (verbose) {
    message("[assemble_final_report] PDF written to: ", pdf_out)
  }

  output_files <- pdf_out

  # -------------------------------------------------------------------
  # Step 3 (optional): Build .docx via officer
  #
  # Word cannot embed vector PDFs, so pages must be rasterized. This
  # uses the *numbered* PDF so the Word doc matches the PDF exactly.
  # -------------------------------------------------------------------
  if (docx) {
    if (verbose) {
      message("[assemble_final_report] Building .docx (rasterizing at ",
              dpi, " dpi)...")
    }

    png_files <- file.path(
      tempdir(),
      sprintf("%s-page-%03d.png", alpha_code, seq_len(n_pages))
    )

    pdftools::pdf_convert(
      pdf       = pdf_out,
      format    = "png",
      pages     = seq_len(n_pages),
      filenames = png_files,
      dpi       = dpi,
      verbose   = FALSE
    )

    # Determine page dimensions from the first image
    first_img <- png::readPNG(png_files[1])
    width_in  <- dim(first_img)[2] / dpi
    height_in <- dim(first_img)[1] / dpi

    doc <- officer::read_docx()

    # Set page size and zero margins so images fill each page exactly
    sec_props <- officer::prop_section(
      page_size    = officer::page_size(width = width_in, height = height_in),
      page_margins = officer::page_mar(top = 0, bottom = 0,
                                       left = 0, right = 0)
    )
    doc <- officer::body_set_default_section(doc, sec_props)

    for (i in seq_len(n_pages)) {
      if (i > 1L) {
        doc <- officer::body_add_break(doc, pos = "after")
      }
      doc <- officer::body_add_img(
        doc,
        src    = png_files[i],
        width  = width_in,
        height = height_in
      )
      if (verbose) {
        message("[assemble_final_report] DOCX: added page ", i,
                " of ", n_pages, ".")
      }
    }

    print(doc, target = docx_out)
    output_files <- c(output_files, docx_out)

    if (cleanup) {
      unlink(png_files)
    }

    if (verbose) {
      message("[assemble_final_report] DOCX written to: ", docx_out)
    }
  }

  # -------------------------------------------------------------------
  # Cleanup combined temp file
  # -------------------------------------------------------------------
  if (cleanup) {
    unlink(tmp_combined)
  }

  # -------------------------------------------------------------------
  # Log entry
  # -------------------------------------------------------------------
  end_time    <- Sys.time()
  elapsed_sec <- as.numeric(difftime(end_time, start_time, units = "secs"))
  timestamp   <- format(end_time, "%Y-%m-%d %H:%M:%S %Z")

  log_lines <- c(
    "", strrep("-", 72L),
    "Processing summary (assemble_final_report)",
    sprintf("Timestamp       : %s", timestamp),
    sprintf("Alpha code      : %s", alpha_code),
    sprintf("Input PDFs      : %d", length(input_paths)),
    sprintf("Front matter    : %s",
            ifelse(is.null(front_matter), "NONE", front_matter)),
    sprintf("Appendix        : %s",
            ifelse(is.null(appendix), "NONE", appendix)),
    sprintf("Page numbers    : %s", page_numbers),
    sprintf("DOCX output     : %s", docx),
    sprintf("DPI             : %s", ifelse(docx, dpi, "NA")),
    sprintf("Total pages     : %d", n_pages),
    sprintf("Outputs saved   : %d", length(output_files)),
    sprintf("Total elapsed   : %.3f s", elapsed_sec),
    paste(sprintf("Output file %d   : %s", seq_along(output_files),
                  output_files), collapse = "\n")
  )

  cat(paste(log_lines, collapse = "\n"), "\n",
      file = log_path, append = TRUE)

  if (verbose) {
    message("[assemble_final_report] Log entry appended to: ", log_path)
    message("[assemble_final_report] Done in ",
            sprintf("%.1f", elapsed_sec), " s.")
  }

  invisible(output_files)
}
