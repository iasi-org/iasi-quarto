#' Validate an IASI Quarto workspace
#'
#' Checks whether `path` appears to be an IASI Quarto publication or a
#' multiproject containing one or more IASI Quarto publications.
#'
#' A directory is recognised as an IASI Quarto publication only when it
#' contains both `_quarto.yml` and `iasi.yml`. If the directory itself is not
#' a publication, `validate()` searches its subdirectories recursively.
#'
#' This function only recognises the workspace. It does not read `iasi.yml`,
#' inspect the publication structure, or validate its contents.
#'
#' @param path Directory to inspect.
#'
#' @return Invisibly returns an object of class `iasi_quarto_plan` when an
#'   IASI Quarto publication or multiproject is found. Returns `NULL`
#'   otherwise.
#'
#' @export
validate = function(path = ".") {
  plan = .validate_path(path)

  .report_validation(path = path, plan = plan)

  invisible(plan)
}
