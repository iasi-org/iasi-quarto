.normalise_project_path = function(path) {
  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}

.relative_path = function(path, root) {
  path = .normalise_project_path(path)
  root = .normalise_project_path(root)

  if (identical(path, root)) {
    return(".")
  }

  prefix = paste0(
    root,
    "/"
  )

  if (!startsWith(
    tolower(path),
    tolower(prefix)
  )) {
    stop(
      sprintf(
        "Path '%s' is outside root '%s'.",
        path,
        root
      ),
      call. = FALSE
    )
  }

  substring(
    path,
    nchar(prefix) + 1L
  )
}