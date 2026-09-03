.IASI_PARAMETER_DEFAULTS = list(
  outputs = "outputs",
  releases = "releases"
)

.validate_iasi_parameter = function(iasi) {
  if (!is.logical(iasi) || length(iasi) != 1L || is.na(iasi)) {
    stop("`iasi` must be TRUE or FALSE.", call. = FALSE)
  }

  iasi
}

.resolve_public_parameters = function(iasi = FALSE, parameters = list(), path = ".") {
  iasi = .validate_iasi_parameter(iasi)

  for (name in names(parameters)) {
    value = parameters[[name]]

    if (iasi && is.null(value)) {
      value = .IASI_PARAMETER_DEFAULTS[[name]]
    }

    if (!is.null(value) && !.valid_iasi_path_value(value)) {
      stop(sprintf("`%s` must be NULL or one non-empty directory path.", name), call. = FALSE)
    }

    parameters[name] = list(if (is.null(value)) NULL else .resolve_path_upwards(path, value, name))
  }

  parameters
}

.apply_project_parameters = function(project, parameters) {
  if (is.null(project$paths)) project$paths = list()

  for (name in names(parameters)) {
    value = parameters[[name]]

    project$paths[name] = list(value)
  }

  if ("outputs" %in% names(parameters)) {
    project$paths$docs = if (is.null(project$paths$outputs)) NULL else file.path(project$paths$outputs, "docs")
  }

  if ("releases" %in% names(parameters)) {
    project$paths$publish = if (is.null(project$paths$releases)) file.path(project$path, "publish") else file.path(project$paths$releases, "publish")
  }

  project
}

.apply_plan_parameters = function(plan, parameters) {
  plan$projects = lapply(plan$projects, .apply_project_parameters, parameters = parameters)
  plan
}

.resolve_path_upwards = function(path, value, name) {
  if (.is_absolute_path(value)) {
    if (!dir.exists(value)) {
      stop(sprintf("Configured `%s` directory does not exist: %s.", name, value), call. = FALSE)
    }
    return(normalizePath(value, winslash = "/", mustWork = TRUE))
  }

  current = .normalise_project_path(path)

  repeat {
    candidate = file.path(current, value)

    if (dir.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }

    parent = dirname(current)
    if (identical(parent, current)) break
    current = parent
  }

  stop(sprintf("Could not locate `%s` directory '%s' above '%s'.", name, value, path), call. = FALSE)
}
