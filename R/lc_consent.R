# Internal environment to track consent within a session
.ukbioprepr_env <- new.env(parent = emptyenv())
.ukbioprepr_env$lc_consent <- FALSE

check_lc_consent <- function() {
  if (isTRUE(.ukbioprepr_env$lc_consent)) return(invisible(TRUE))
  # Skip prompt in non-interactive sessions or during testing
  if (!interactive() || testthat::is_testing()) {
    .ukbioprepr_env$lc_consent <- TRUE
    return(invisible(TRUE))
  }
  message(
    "\n--- Land Cover Data Licence Agreement ---\n",
    "The land cover data products accessed by this function are derived\n",
    "from UKCEH Land Cover Maps and are subject to the following conditions:\n\n",
    "  1. Use is restricted to NON-COMMERCIAL applications only.\n",
    "  2. You must cite the ukbioprepr package in any publications.\n",
    "  3. You must cite the original UKCEH source datasets for each\n",
    "     year used. Full citation details are available at:\n",
    "     https://github.com/crrush7/ukbioprepr/blob/main/CITATIONS.md\n\n",
    "Do you confirm that your use is non-commercial and that you agree\n",
    "to cite the original data sources? (Y/N): ",
    appendLF = FALSE
  )

  response <- trimws(readline())

  if (toupper(response) == "Y") {
    .ukbioprepr_env$lc_consent <- TRUE
    message("Thank you. You will not be asked again this session.\n")
    return(invisible(TRUE))
  } else {
    stop(
      "Land cover data access requires confirmation of non-commercial use ",
      "and agreement to cite original sources. ",
      "Please review the licence terms at: ",
      "https://github.com/crrush7/ukbioprepr/blob/main/CITATIONS.md",
      call. = FALSE
    )
  }
}
