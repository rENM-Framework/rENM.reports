utils::globalVariables(c(
  "Variable", "sig_mark", "Variable_out", "Points",
  "Slope", "CI Low", "CI High", "PD", "ROPE %",
  ".data"
))

#' @importFrom rENM.core rENM_project_dir get_species_info show_species show_variables
NULL

# Render a gt table via gtsave, giving chromote extra headroom to start
# Chrome under load (avoids "Chrome debugging port not open after N seconds").
.gt_save_with_timeout <- function(gt_tbl, path, ..., timeout = 60) {
  old <- options(chromote.timeout = timeout)
  on.exit(options(old), add = TRUE)
  gt::gtsave(gt_tbl, path, ...)
}
