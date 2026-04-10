#' Assemble "Variable Trend Maps" pages
#'
#' Builds a multi-page PDF that shows trends and acceleration/deceleration
#' patterns of the top contributing variables.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline
#' and operates within the project directory structure defined by
#' \code{rENM_project_dir()}.
#'
#' \strong{Pipeline context}
#' \itemize{
#'   \item Copies a shared caption PDF (A) and a species-specific
#'     variable-trend-maps PDF (B) into the \code{Summaries/pages}
#'     directory for the target species.
#'   \item Ensures consistent naming so that downstream report builders
#'     pick up both pages automatically.
#' }
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Shared caption PDF (A):\cr
#'     \code{system.file("captions", "variable_trend_maps_caption.pdf", package = "rENM.reports")}
#'   \item Species maps PDF (B):\cr
#'     \code{file.path(rENM_project_dir(), "runs", <alpha_code>,}
#'     \code{"Summaries", "maps",}
#'     \code{paste0(<alpha_code>, "-Variable-Trend-Maps.pdf"))}
#' }
#'
#' \strong{Outputs}
#' \itemize{
#'   \item Caption copy:\cr
#'     \code{file.path(rENM_project_dir(), "runs", <alpha_code>,}
#'     \code{"Summaries", "pages",}
#'     \code{paste0(<alpha_code>, "-Variable-Trend-Maps1.pdf"))}
#'   \item Maps copy:\cr
#'     \code{file.path(rENM_project_dir(), "runs", <alpha_code>,}
#'     \code{"Summaries", "pages",}
#'     \code{paste0(<alpha_code>, "-Variable-Trend-Maps2.pdf"))}
#'   \item Processing summary appended to:\cr
#'     \code{file.path(rENM_project_dir(), "runs", <alpha_code>, "_log.txt")}
#' }
#'
#' \strong{Methods}
#' \itemize{
#'   \item Pure file staging via \code{file.copy()} with optional overwrite.
#'   \item No PDF merging, rasterization, or resizing.
#' }
#'
#' @param alpha_code Character. Species alpha code (e.g., "CASP"). Converted
#'   internally to uppercase for all paths and filenames.
#' @param overwrite Logical. If TRUE (default) overwrites existing destination
#'   files; if FALSE the function stops if a destination file already exists.
#' @param verbose Logical. If TRUE (default) prints processing steps and paths
#'   to the console.
#'
#' @return
#' A named list (invisibly returned):
#' \itemize{
#'   \item \code{caption_page} – full file path to the staged caption PDF
#'   \item \code{maps_page}    – full file path to the staged maps PDF
#' }
#' Side effects:
#' \itemize{
#'   \item Two PDF files copied into
#'     \code{<project>/runs/<alpha_code>/Summaries/pages}
#'   \item Processing summary appended to
#'     \code{<project>/runs/<alpha_code>/_log.txt}
#' }
#'
#' @examples
#' \dontrun{
#' assemble_variable_trend_maps_page("CASP")
#' }
#'
#' @export
assemble_variable_trend_maps_page <- function(alpha_code,
                                              overwrite = TRUE,
                                              verbose   = TRUE) {
  # ---------------------------------------------------------------------------
  # 0. Setup, argument checks, and timing
  # ---------------------------------------------------------------------------
  start_time <- Sys.time()
  code <- toupper(alpha_code)
  
  if (!is.character(code) || length(code) != 1L || nchar(code) == 0L) {
    stop("alpha_code must be a non-empty character scalar.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L) {
    stop("overwrite must be a single logical value.", call. = FALSE)
  }
  if (!is.logical(verbose) || length(verbose) != 1L) {
    stop("verbose must be a single logical value.", call. = FALSE)
  }
  
  # ---------------------------------------------------------------------------
  # 1. Construct paths using rENM_project_dir()
  # ---------------------------------------------------------------------------
  proj_dir    <- rENM_project_dir()
  species_dir <- file.path(proj_dir, "runs", code)
  
  if (!dir.exists(species_dir)) {
    stop(sprintf("Species directory not found: %s", species_dir), call. = FALSE)
  }
  
  # (A) Caption source: now from package inst/ directory
  caption_src <- system.file(
    "captions",
    "variable_trend_maps_caption.pdf",
    package = "rENM.reports"
  )
  
  # (B) Maps source: species-specific
  maps_src <- file.path(
    species_dir, "Summaries", "maps",
    sprintf("%s-Variable-Trend-Maps.pdf", code)
  )
  
  # Destination dir for both staged PDFs
  pages_dir <- file.path(species_dir, "Summaries", "pages")
  if (!dir.exists(pages_dir)) {
    dir.create(pages_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # (1) Caption destination
  caption_dst <- file.path(
    pages_dir,
    sprintf("%s-Variable-Trend-Maps1.pdf", code)
  )
  
  # (2) Maps destination
  maps_dst <- file.path(
    pages_dir,
    sprintf("%s-Variable-Trend-Maps2.pdf", code)
  )
  
  # Log file
  log_file <- file.path(species_dir, "_log.txt")
  
  if (verbose) {
    message("[assemble_variable_trend_maps_page] Alpha code:      ", code)
    message("[assemble_variable_trend_maps_page] Caption source:  ", caption_src)
    message("[assemble_variable_trend_maps_page] Caption dest:    ", caption_dst)
    message("[assemble_variable_trend_maps_page] Maps source:     ", maps_src)
    message("[assemble_variable_trend_maps_page] Maps dest:       ", maps_dst)
  }
  
  # ---------------------------------------------------------------------------
  # 2. Validate sources and overwrite logic
  # ---------------------------------------------------------------------------
  if (!nzchar(caption_src) || !file.exists(caption_src)) {
    stop("Caption source PDF not found in package: rENM.reports", call. = FALSE)
  }
  if (!file.exists(maps_src)) {
    stop(sprintf("Maps source PDF not found: %s", maps_src), call. = FALSE)
  }
  
  if (!overwrite) {
    conflict <- character(0)
    if (file.exists(caption_dst)) conflict <- c(conflict, caption_dst)
    if (file.exists(maps_dst))    conflict <- c(conflict, maps_dst)
    if (length(conflict) > 0L) {
      stop(
        "Destination file(s) already exist and overwrite = FALSE:\n  ",
        paste(conflict, collapse = "\n  "),
        call. = FALSE
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 3. Copy caption and maps PDFs into Summaries/pages
  # ---------------------------------------------------------------------------
  if (verbose) {
    message("[assemble_variable_trend_maps_page] Copying caption PDF to pages directory...")
  }
  ok_cap <- file.copy(
    from      = caption_src,
    to        = caption_dst,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!isTRUE(ok_cap)) {
    stop("Failed to copy caption PDF from\n  ", caption_src,
         "\n  to\n  ", caption_dst, call. = FALSE)
  }
  
  if (verbose) {
    message("[assemble_variable_trend_maps_page] Copying maps PDF to pages directory...")
  }
  ok_maps <- file.copy(
    from      = maps_src,
    to        = maps_dst,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )
  if (!isTRUE(ok_maps)) {
    stop("Failed to copy maps PDF from\n  ", maps_src,
         "\n  to\n  ", maps_dst, call. = FALSE)
  }
  
  if (verbose) {
    message("[assemble_variable_trend_maps_page] Staging complete.")
  }
  
  # ---------------------------------------------------------------------------
  # 4. eBird-standard log entry
  # ---------------------------------------------------------------------------
  stop_time   <- Sys.time()
  elapsed_sec <- as.numeric(difftime(stop_time, start_time, units = "secs"))
  sep_line    <- paste(rep("-", 72), collapse = "")
  
  log_lines <- c(
    "",
    sep_line,
    "Processing summary (assemble_variable_trend_maps_page)",
    sprintf("Alpha code:       %s", code),
    sprintf("Caption source:   %s", caption_src),
    sprintf("Caption copy:     %s", caption_dst),
    sprintf("Maps source:      %s", maps_src),
    sprintf("Maps copy:        %s", maps_dst),
    sprintf("Start time:       %s", format(start_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Stop time:        %s", format(stop_time, "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Outputs saved:    %d", 2L),
    sprintf("Output file 1:    %s", caption_dst),
    sprintf("Output file 2:    %s", maps_dst),
    sprintf("Total elapsed:    %.1f seconds", elapsed_sec)
  )
  
  cat(paste0(log_lines, collapse = "\n"),
      file = log_file, sep = "\n", append = TRUE)
  
  if (verbose) {
    message("[assemble_variable_trend_maps_page] Appended processing summary to: ",
            log_file)
  }
  
  invisible(list(
    caption_page = caption_dst,
    maps_page    = maps_dst
  ))
}