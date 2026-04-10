#' Assemble side-by-side variable trend maps into paged composites
#'
#' Pair trend and rate PNGs for each variable, build A | B montages, arrange
#' several montages per page, and export them as PDF, DOCX, and PNG. Designed
#' for variable-contribution reporting in the rENM workflow.
#'
#' @details
#' This function is part of the rENM framework's processing pipeline  
#' and operates within the project directory structure defined by  
#' rENM_project_dir().
#'
#' \strong{Pipeline steps}
#' \itemize{
#'   \item 1. Read the variable list from  
#'         \code{<rENM_project_dir()>/runs/<alpha_code>/Trends/variables/}
#'         \code{<alpha_code>-Variable-Contributions-BR-Stats.csv}  
#'         (must contain \code{Variable}; \code{pd_slope} is optional).
#'   \item 2. For every variable \code{<var>} collect two PNGs  
#'         (case-insensitive, punctuation-agnostic matching):  
#'         \itemize{
#'           \item A (trend): \code{<base>-trends/<var>.png}  
#'           \item B (rate) : \code{<base>-trends-rate/<var>.png}  
#'         }  
#'         If \code{base_dir == "auto"} both \code{"m2"} and \code{"mc"}
#'         directories are tried.
#'   \item 3. Build an A | B montage for each variable with generous spacing.
#'   \item 4. Stack \code{per_page} montages per page.
#'   \item 5. Trim, down-scale, and vertically crop overflow.
#'   \item 6. Composite onto a white US Letter page  
#'         (8.5 x 11 in, 1 in margins).
#'   \item 7. Export a multipage PDF, a matching DOCX, and page-level PNGs
#'         named \code{-p01}, \code{-p02}, ...
#' }
#'
#' \strong{Highlight rule}  
#' If the CSV includes \code{pd_slope}, any variable with  
#' \code{\{pd_slope >= 90\}} receives a dark-blue strip along the left edge.
#'
#' \strong{Layout and styling}
#' \itemize{
#'   \item Each A <-> B pair is slightly down-scaled to add whitespace.
#'   \item The gutter between A and B, as well as row spacing, is widened.
#'   \item The full column is scaled to 96\% of the content width and centred.
#'   \item Excess height beyond the content area is cropped from the bottom.
#' }
#'
#' \strong{CRAN compliance}  
#' All hard-coded paths have been removed; the project root is obtained via
#' \code{rENM_project_dir()}.
#'
#' \strong{Inputs}
#' \itemize{
#'   \item Variable CSV (with optional \code{pd_slope}) as described above.
#'   \item Trend and rate PNGs residing in the two
#'         \code{<base>-trends*} folders under
#'         \code{<rENM_project_dir()>/data/merra/}.
#' }
#'
#' \strong{Outputs}
#' \itemize{
#'   \item \code{<alpha_code>-Variable-Trend-Maps.pdf}
#'   \item \code{<alpha_code>-Variable-Trend-Maps.docx}
#'   \item \code{<alpha_code>-Variable-Trend-Maps-pXX.png}
#'   \item A processing summary appended to
#'         \code{runs/<alpha_code>/_log.txt}
#' }
#'
#' @param alpha_code Character. Species code (e.g., "CASP"). Used for input and
#'   output file naming.
#' @param base_dir Character. One of "auto", "m2", "mc"; controls which MERRA-2
#'   directories are searched for trend images.
#' @param base_home Character. Deprecated. Ignored and retained only for
#'   backward compatibility; the project root is taken from
#'   \code{rENM_project_dir()}.
#' @param vars_csv_subpath Character or NULL. Optional path (relative to
#'   \code{runs/<alpha_code>}) to the variable list CSV; when NULL the default
#'   path described in \strong{Pipeline steps} is used.
#' @param per_page Integer. Number of variables (rows) per output page.
#' @param dpi Numeric. Rendering resolution in dots per inch.
#' @param page_width_in Numeric. Page width in inches.
#' @param page_height_in Numeric. Page height in inches.
#' @param margin_in Numeric. Page margin in inches on all sides.
#' @param trim_fuzz Numeric. Fuzz parameter passed to
#'   \code{magick::image_trim()} (in pixels).
#'
#' @return A \code{list} with the following elements:
#' \itemize{
#'   \item \code{png}: Character vector of page-level PNG paths.
#'   \item \code{pdf}: Character string giving the PDF path.
#'   \item \code{docx}: Character string giving the DOCX path.
#' }  
#' Side effects:
#' \itemize{
#'   \item Files are written to
#'         \code{runs/<alpha_code>/Summaries/maps/}.
#'   \item A log block is appended to
#'         \code{runs/<alpha_code>/_log.txt}.
#' }
#'
#' @importFrom magick image_trim image_read image_info image_scale image_border
#' @importFrom magick image_join image_append image_blank image_write
#' @importFrom magick image_composite image_crop
#' @importFrom officer read_docx prop_section page_size page_mar
#' @importFrom officer body_set_default_section external_img body_add_break fpar
#' @importFrom officer fp_par body_add_fpar
#' @importFrom utils read.csv
#' @importFrom tools file_path_sans_ext
#' @importFrom stats setNames
#'
#' @examples
#' \dontrun{
#' gather_top_variable_trend_maps("CASP")           # auto-detect bases
#' gather_top_variable_trend_maps("CASP", "m2")     # force "m2"
#' gather_top_variable_trend_maps("CASP", per_page = 3)
#' }
#'
#' @export
gather_top_variable_trend_maps <- function(alpha_code,
                                           base_dir = c("auto", "m2", "mc"),
                                           base_home = "~",      # kept for legacy signature
                                           vars_csv_subpath = NULL,
                                           per_page = 5,
                                           dpi = 300,
                                           page_width_in = 8.5,
                                           page_height_in = 11,
                                           margin_in = 1,
                                           trim_fuzz = 8) {
  t_start <- Sys.time()
  
  # ---- Dependency checks ----------------------------------------------------
  for (pkg in c("magick", "officer")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required.", pkg), call. = FALSE)
    }
  }
  magick  <- asNamespace("magick")
  officer <- asNamespace("officer")
  
  # ---- Basic path setup -----------------------------------------------------
  if (!exists("rENM_project_dir", mode = "function")) {
    stop("Function 'rENM_project_dir()' not found - make sure the package ",
         "environment is loaded and up-to-date.", call. = FALSE)
  }
  project_dir <- rENM_project_dir()           # <-- NEW: single project root
  if (!dir.exists(project_dir)) {
    stop("Project directory returned by rENM_project_dir() does not exist: ",
         project_dir, call. = FALSE)
  }
  
  code      <- toupper(alpha_code)
  base_dir  <- match.arg(base_dir)
  
  runs_dir  <- file.path(project_dir, "runs",  code)
  
  # Variable CSV: default subpath unless overridden
  if (is.null(vars_csv_subpath)) {
    vars_csv_subpath <- file.path(
      "Trends", "variables",
      sprintf("%s-Variable-Contributions-BR-Stats.csv", code)
    )
  }
  vars_csv <- file.path(runs_dir, vars_csv_subpath)
  if (!file.exists(vars_csv)) {
    stop("Variable contributions CSV not found: ", vars_csv)
  }
  
  # Source roots for variable map PNGs (A = trends, B = trends-rate)
  root_merra <- file.path(project_dir, "data", "merra")
  dir_for <- function(b, rate = FALSE) {
    file.path(root_merra, paste0(b, if (rate) "-trends-rate" else "-trends"))
  }
  
  # Output directory and file targets
  out_dir  <- file.path(runs_dir, "Summaries", "maps")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pdf_out  <- file.path(out_dir, sprintf("%s-Variable-Trend-Maps.pdf",  code))
  docx_out <- file.path(out_dir, sprintf("%s-Variable-Trend-Maps.docx", code))
  
  # ---- Read CSV and prepare lookups ----------------------------------------
  df <- utils::read.csv(vars_csv, stringsAsFactors = FALSE)
  if (!"Variable" %in% names(df)) {
    stop("CSV missing 'Variable' column.")
  }
  
  # Helper to normalise keys
  norm_key <- function(x) tolower(gsub("[^a-z0-9]+", "", trimws(x)))
  
  vars <- unique(trimws(df$Variable))
  vars <- vars[nzchar(vars)]
  message(
    "gather_top_variable_trend_maps(): ",
    length(vars), " variables detected: ",
    paste(vars, collapse = ", ")
  )
  
  slope_col <- names(df)[tolower(names(df)) == "pd_slope"]
  slope_lookup <- NULL
  if (length(slope_col) == 1L) {
    vkeys <- norm_key(df$Variable)
    pd    <- suppressWarnings(as.numeric(df[[slope_col]]))
    slope_lookup <- stats::setNames(pd, vkeys)
  } else {
    message("Note: 'pd_slope' column not found; left-edge highlights skipped.")
  }
  
  # ---- Page geometry --------------------------------------------------------
  full_w_px    <- round(page_width_in  * dpi)
  full_h_px    <- round(page_height_in * dpi)
  content_w_px <- round((page_width_in  - 2 * margin_in) * dpi)
  content_h_px <- round((page_height_in - 2 * margin_in) * dpi)
  offset_y_px  <- round(margin_in * dpi)
  
  content_scale <- 0.96
  target_w_px   <- round(content_w_px * content_scale)
  offset_x_px   <- round(margin_in * dpi + (content_w_px - target_w_px) / 2)
  
  trim_img <- function(img) magick$image_trim(img, fuzz = trim_fuzz)
  
  # List PNGs helper
  list_pngs <- function(dir_path) {
    if (!dir.exists(dir_path)) {
      return(data.frame(path = character(), key = character(),
                        stringsAsFactors = FALSE))
    }
    files <- list.files(
      dir_path,
      pattern     = "\\.png$",
      full.names  = TRUE,
      recursive   = TRUE,
      ignore.case = TRUE
    )
    if (!length(files)) {
      return(data.frame(path = character(), key = character(),
                        stringsAsFactors = FALSE))
    }
    data.frame(
      path = files,
      key  = norm_key(tools::file_path_sans_ext(basename(files))),
      stringsAsFactors = FALSE
    )
  }
  
  # Resolve A/B images for one base
  resolve_in_base <- function(v, base) {
    a_tbl <- list_pngs(dir_for(base, FALSE))
    r_tbl <- list_pngs(dir_for(base, TRUE))
    target <- norm_key(v)
    a_hit <- which(a_tbl$key == target)
    if (!length(a_hit)) a_hit <- grep(target, a_tbl$key)
    r_hit <- which(r_tbl$key == target)
    if (!length(r_hit)) r_hit <- grep(target, r_tbl$key)
    if (length(a_hit) && length(r_hit)) {
      list(A = a_tbl$path[a_hit[1]], R = r_tbl$path[r_hit[1]], base = base)
    } else {
      list(A = NA_character_, R = NA_character_)
    }
  }
  
  base_order <- switch(
    base_dir,
    m2   = c("m2", "mc"),
    mc   = c("mc", "m2"),
    auto = c("m2", "mc")
  )
  
  # Build one A | B pair
  read_pair <- function(v) {
    for (b in base_order) {
      a <- resolve_in_base(v, b)
      if (!is.na(a$A) && !is.na(a$R)) {
        message(sprintf("  [OK] %s from %s", v, b))
        
        A <- trim_img(magick$image_read(a$A))
        B <- trim_img(magick$image_read(a$R))
        
        ia <- magick$image_info(A)
        ib <- magick$image_info(B)
        shrink_factor <- 0.88
        target_h <- round(max(ia$height, ib$height) * shrink_factor)
        A2 <- magick$image_scale(A, paste0("x", target_h))
        B2 <- magick$image_scale(B, paste0("x", target_h))
        
        A2  <- magick$image_border(A2, color = "white", geometry = "24x0")
        pair <- magick$image_append(magick$image_join(A2, B2), stack = FALSE)
        pair <- magick$image_border(pair, color = "white", geometry = "12x12")
        pair <- trim_img(pair)
        
        add_left_highlight <- FALSE
        if (!is.null(slope_lookup)) {
          key <- norm_key(v)
          s   <- suppressWarnings(as.numeric(slope_lookup[[key]]))
          add_left_highlight <- is.finite(s) && s >= 90
        }
        if (add_left_highlight) {
          pair <- magick$image_border(pair, color = "white", geometry = "24x0")
          border_w <- 6
          border_h <- magick$image_info(pair)$height
          blue_strip <- magick$image_blank(
            width  = border_w,
            height = border_h,
            color  = "darkblue"
          )
          pair <- magick$image_append(magick$image_join(blue_strip, pair),
                                      stack = FALSE)
        }
        return(pair)
      }
    }
    message(sprintf("  - Skipping variable '%s' (missing A/B).", v))
    NULL
  }
  
  build_res <- lapply(vars, function(v) {
    img <- read_pair(v)
    list(var = v, img = img, kept = !is.null(img))
  })
  
  kept_idx     <- vapply(build_res, `[[`, logical(1), "kept")
  kept_vars    <- vapply(build_res[kept_idx], `[[`, character(1), "var")
  skipped_vars <- vapply(build_res[!kept_idx], `[[`, character(1), "var")
  
  pairs <- lapply(build_res[kept_idx], `[[`, "img")
  
  n_vars_total <- length(vars)
  n_vars_kept  <- length(pairs)
  if (!n_vars_kept) {
    stop("No complete A/B pairs found in m2/mc trend folders.")
  }
  per_page <- max(1L, per_page)
  n_pages  <- ceiling(n_vars_kept / per_page)
  message("[OK] ", n_vars_kept, " variables across ", n_pages, " page(s).")
  
  # ---- Assemble pages -------------------------------------------------------
  build_page <- function(pairs_slice) {
    add_h_gutter <- function(im) magick$image_border(im, color = "white",
                                                     geometry = "0x36")
    column <- Reduce(
      function(a, b) magick$image_append(magick$image_join(a, b), stack = TRUE),
      lapply(pairs_slice, add_h_gutter)
    )
    column <- trim_img(column)
    
    info       <- magick$image_info(column)
    scaled_h   <- round(info$height * (target_w_px / info$width))
    column_sc  <- magick$image_scale(column,
                                     sprintf("%dx%d", target_w_px, scaled_h))
    column_cr  <- if (scaled_h > content_h_px) {
      magick$image_crop(column_sc,
                        geometry = sprintf("%dx%d+0+0",
                                           target_w_px, content_h_px))
    } else column_sc
    
    page <- magick$image_blank(width = full_w_px,
                               height = full_h_px,
                               color = "white")
    magick$image_composite(
      page,
      column_cr,
      offset = sprintf("+%d+%d", offset_x_px, offset_y_px)
    )
  }
  
  pages <- lapply(split(pairs, ceiling(seq_along(pairs) / per_page)), build_page)
  
  # ---- Write outputs --------------------------------------------------------
  magick$image_write(magick$image_join(pages), pdf_out, format = "pdf")
  
  png_paths <- character(length(pages))
  for (i in seq_along(pages)) {
    png_i <- file.path(out_dir,
                       sprintf("%s-Variable-Trend-Maps-p%02d.png", code, i))
    magick$image_write(pages[[i]], png_i, format = "png")
    png_paths[i] <- png_i
  }
  message("[OK] PNG pages saved: ",
          paste(basename(png_paths), collapse = ", "))
  
  doc <- officer$read_docx()
  sect <- officer$prop_section(
    page_size    = officer$page_size(
      orient = "portrait",
      width  = page_width_in,
      height = page_height_in
    ),
    page_margins = officer$page_mar(
      top    = margin_in,
      bottom = margin_in,
      left   = margin_in,
      right  = margin_in
    )
  )
  doc <- officer$body_set_default_section(doc, sect)
  tmp_png <- tempfile(fileext = ".png")
  on.exit(if (file.exists(tmp_png)) unlink(tmp_png), add = TRUE)
  
  for (i in seq_along(pages)) {
    content <- magick$image_crop(
      pages[[i]],
      geometry = sprintf("%dx%d+%d+%d",
                         target_w_px, content_h_px, offset_x_px, offset_y_px)
    )
    magick$image_write(content, tmp_png)
    img_block <- officer$external_img(
      src    = tmp_png,
      width  = target_w_px / dpi,
      height = magick$image_info(content)$height / dpi
    )
    if (i > 1) doc <- officer$body_add_break(doc)
    doc <- officer$body_add_fpar(
      doc,
      officer$fpar(img_block,
                   fp_p = officer$fp_par(text.align = "center"))
    )
  }
  print(doc, target = docx_out)
  
  # ---- Logging --------------------------------------------------------------
  t_end   <- Sys.time()
  elapsed <- difftime(t_end, t_start, units = "secs")
  s       <- as.numeric(elapsed)
  hh      <- sprintf("%02d", s %/% 3600); s <- s %% 3600
  mm      <- sprintf("%02d", s %/% 60);   ss <- sprintf("%02d", round(s %% 60))
  fmt_elapsed <- paste0(hh, ":", mm, ":", ss)
  
  log_file <- file.path(runs_dir, "_log.txt")
  ts_str   <- format(t_end, "%Y-%m-%d %H:%M:%S %Z")
  sep_line <- paste(rep("-", 72), collapse = "")
  
  f <- function(k, v) sprintf("%-22s : %s", k, v)
  
  outputs <- c(png_paths, pdf_out, docx_out)
  log_block <- c(
    "",
    sep_line,
    "Processing summary (gather_top_variable_trend_maps)",
    f("Timestamp",          ts_str),
    f("Alpha code",         code),
    f("Base search",        paste(base_order, collapse = " -> ")),
    f("Variables total",    n_vars_total),
    f("Variables kept",     n_vars_kept),
    f("Variables skipped",  length(skipped_vars)),
    if (length(skipped_vars))
      paste0("  skipped: ", paste(skipped_vars, collapse = ", ")),
    f("Per-page rows",      per_page),
    f("DPI",                dpi),
    f("Margins (in)",       margin_in),
    f("Content width (px)", target_w_px),
    f("Content height (px)", content_h_px),
    f("Pages written",      length(pages)),
    f("Outputs saved",      length(outputs)),
    vapply(outputs, function(x) paste0(" - ", x), character(1)),
    f("Total elapsed",      fmt_elapsed),
    ""
  )
  
  try({
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    cat(paste0(log_block, collapse = "\n"),
        file = log_file, append = TRUE)
  }, silent = TRUE)
  
  message("[OK] Saved:")
  message("  - ", paste(png_paths, collapse = "\n  - "))
  message("  - ", pdf_out)
  message("  - ", docx_out)
  message("Processing summary appended to: ", log_file)
  
  invisible(list(png = png_paths, pdf = pdf_out, docx = docx_out))
}