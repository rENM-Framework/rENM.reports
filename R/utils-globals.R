if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Variable", "sig_mark", "Variable_out", "Points",
    "Slope", "CI Low", "CI High", "PD", "ROPE %",
    ".data"
  ))
}

#' @importFrom rENM.core rENM_project_dir get_species_info show_species show_variables
NULL