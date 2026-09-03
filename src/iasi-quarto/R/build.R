#' Build IASI Quarto publications or R packages
#'
#' `build()` is the public build entry point. It identifies what can be built
#' from `path`, delegates the work to the appropriate private builder, and
#' places the resulting artifacts in their configured output location.
#'
#' When `format` is `NULL` or `"all"`, the build type is detected from
#' `path`. An R package is recognised by its `DESCRIPTION` file; otherwise the
#' path is handled by the existing IASI Quarto build pipeline.
#'
#' @param book Publication or publications to build when `path` identifies an
#'   IASI Quarto workspace. A publication can be selected by its complete
#'   directory name, its name without the numeric prefix, or its numeric
#'   prefix. Use `"all"` or `NULL` to build every publication.
#' @param format Output format or formats. For R packages use `"r"` to build
#'   both source and binary packages, `"r-source"` for source only, or
#'   `"r-binary"` for binary only. Use `"all"` or `NULL` to detect the build
#'   type from `path`. Other values are delegated to the IASI Quarto builder.
#' @param path Directory to build.
#' @param outputs Optional output directory name or path. Relative values are
#'   searched from `path` upwards. When `NULL`, build results are not moved.
#' @param releases Optional release directory name or path. Relative values are
#'   searched from `path` upwards and attached to the resolved project.
#' @param iasi When `TRUE`, applies the IASI default for `outputs` when no
#'   explicit value is supplied. Defaults to `FALSE`.
#' @param force Explicitly requests a complete build. Defaults to `FALSE`.
#'
#' @return Invisibly returns the result produced by the selected builder after
#'   its artifacts have been placed in their output location.
#'
#' @export
build = function(book = NULL, format = NULL, path = ".", outputs = NULL, releases = NULL, iasi = FALSE, force = FALSE) {
  parameters = .resolve_public_parameters(iasi = iasi, parameters = list(outputs = outputs, releases = releases), path = path)
  type = .resolve_build_type(path = path, format = format)

  built = switch(type,
    quarto = .build_quarto(book = book, format = format, path = path, force = force),
    package = .build_package(book = book, format = format, path = path, force = force),
    stop(sprintf("Unsupported build type '%s'.", type), call. = FALSE)
  )

  built$projects = lapply(built$projects, .apply_project_parameters, parameters = parameters)
  if (identical(type, "quarto")) .record_quarto_build_results(built$projects, built$outputs)
  built$outputs = .move_build_results(result = built$outputs, type = type, projects = built$projects)
  invisible(built)
}
