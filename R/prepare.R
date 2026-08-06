#' Build an IASI Quarto publication
#'
#' Discovers and validates a project when necessary, generates any artifacts
#' required by its model, and returns an independent publication object.
#' It does not invoke Quarto.
#'
#' @param project An optional `iasi_quarto_project`.
#' @param path Directory containing `_quarto.yml`, used when `project` is NULL.
#'
#' @return Invisibly returns an `iasi_quarto_publication`.
#' @export
prepare = function(project = NULL, path = ".") {
   
  if (is.null(project)) project = discover(path)

  .assert_project(project)

  if (is.null(project$valid)) project = .validate_project(project)
  
  .assert_valid_project(project)

  project = switch(project$type,
                   structured = .prepare_structured(project),
                   regular = .prepare_regular(project),
                   direct = .prepare_direct(project),
                   incoherent = stop("An incoherent project cannot be built.",call. = FALSE),
                   stop(sprintf("Unsupported publication type '%s'.", project$type),call. = FALSE )
  )

  .assert_publication(project$publication)
  invisible(project$publication)
}
