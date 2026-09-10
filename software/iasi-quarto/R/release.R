#' Release an IASI Quarto repository
#'
#' Materialises the current release for the Git repository containing `path`.
#'
#' The repository root is discovered automatically. Release configuration is
#' read from the IASI project declaration associated with that repository.
#'
#' @param source Optional source to release. When `NULL`, the configured or
#'   default source is used.
#' @param path Directory from which to resolve the IASI project and Git
#'   repository.
#' @param force When `TRUE`, performs the release even when the current release
#'   is already up to date. Defaults to `FALSE`.
#'
#' @return Invisibly returns the completed release plan.
#'
#' @export
release = function(source = NULL, path = ".", force = FALSE) {
   started_at = Sys.time()
   
   plan = .release_plan(source = source,path = path)
   
   if (is.null(plan)) return(invisible(NULL))
   
   message("Liberando...")
   
   plan = .release(plan = plan, force = force)
   
   plan$elapsed = as.numeric(difftime(Sys.time(), started_at, units = "secs"))
   
   .report_release(plan)
   
   invisible(plan)
}