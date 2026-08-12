#' Build and publish IASI Quarto publications
#'
#' Runs [build()] followed by [publish()] as a single deployment operation.
#'
#' `deploy()` is intended for the moment when the current sources are ready to
#' become the published artifact. During normal authoring, [build()] can be used
#' independently to generate and inspect the outputs without modifying
#' `publish/`.
#'
#' [publish()] is only executed when [build()] completes successfully. If the
#' build fails, the existing `publish/` directory is left untouched.
#'
#' @return Invisibly returns the result of [publish()]. Returns `NULL` when
#'   [build()] does not find an IASI Quarto workspace.
#'
#' @seealso [build()], [publish()]
#'
#' @examples
#' \dontrun{
#' deploy()
#' }
#'
#' @export
deploy = function() {
  plan = build()
  if (is.null(plan)) return(invisible(NULL))
  result = publish()
  invisible(result)
}
