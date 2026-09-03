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

.resolve_output_dir = function(quarto, profile_quarto, profile) {
  output_dir = .yaml_field(
    .yaml_section(profile_quarto, "project"),
    "output-dir"
  )

  if (.valid_output_dir(output_dir)) {
    return(output_dir)
  }

  output_root = .yaml_field(
    .yaml_section(quarto, "project"),
    "output-dir"
  )

  if (.valid_output_dir(output_root)) {
    return(file.path(output_root, profile))
  }

  NULL
}

.profile_config = function(project, profile) {
  profile_path = file.path(
    project$path,
    sprintf("_quarto-%s.yml", profile)
  )

  profile_quarto = if (file.exists(profile_path)) {
    .read_yaml_file(profile_path)
  } else {
    list()
  }

  output_dir = .resolve_output_dir(
    quarto = project$quarto,
    profile_quarto = profile_quarto,
    profile = profile
  )

  output_path = NULL

  if (.valid_output_dir(output_dir)) {
    output_path = if (.is_absolute_path(output_dir)) {
      output_dir
    } else {
      file.path(project$path, output_dir)
    }
  }

  list(
    profile = profile,
    type = .project_format_type(project, profile),
    quarto = profile_quarto,
    output_dir = output_dir,
    output_path = output_path
  )
}

.build_output_config = function(project, name) {
  output_dir = .resolve_output_dir(
    quarto = project$quarto,
    profile_quarto = list(),
    profile = name
  )

  output_path = NULL

  if (.valid_output_dir(output_dir)) {
    output_path = if (.is_absolute_path(output_dir)) {
      output_dir
    } else {
      file.path(project$path, output_dir)
    }
  }

  list(
    name = name,
    output_dir = output_dir,
    output_path = output_path
  )
}

.profile_output_dir_value = function(path, profile) {
  quarto_path = file.path(path, "_quarto.yml")
  profile_path = file.path(
    path,
    sprintf("_quarto-%s.yml", profile)
  )

  quarto = if (file.exists(quarto_path)) {
    .read_yaml_file(quarto_path)
  } else {
    list()
  }

  profile_quarto = if (file.exists(profile_path)) {
    .read_yaml_file(profile_path)
  } else {
    list()
  }

  .resolve_output_dir(
    quarto = quarto,
    profile_quarto = profile_quarto,
    profile = profile
  )
}

.profile_output_dir = function(path, profile) {
  output_dir = .profile_output_dir_value(path, profile)

  if (!.valid_output_dir(output_dir)) {
    return(NULL)
  }

  if (.is_absolute_path(output_dir)) {
    return(output_dir)
  }

  file.path(path, output_dir)
}

.valid_output_dir = function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.is_absolute_path = function(path) {
  grepl("^(/|[A-Za-z]:[/\\\\])", path)
}

.resolve_project_paths = function(project_path, paths) {
  values = list(
    inputs = .yaml_field(paths, "inputs"),
    orchestration = .yaml_field(paths, "orchestration"),
    outputs = .yaml_field(paths, "outputs"),
    releases = .yaml_field(paths, "releases")
  )

  resolved = lapply(
    values,
    function(value) .resolve_project_owned_path(project_path, value)
  )

  resolved$docs = if (is.null(resolved$outputs)) {
    NULL
  } else {
    file.path(resolved$outputs, "docs")
  }

  resolved$publish = if (is.null(resolved$releases)) {
    NULL
  } else {
    file.path(resolved$releases, "publish")
  }

  resolved
}

.resolve_project_owned_path = function(project_path, value) {
  if (!.valid_iasi_path_value(value)) {
    return(NULL)
  }

  if (.is_absolute_path(value)) {
    return(value)
  }

  file.path(project_path, value)
}

.valid_iasi_path_value = function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.default_project_paths = function(project_path) {
  .resolve_project_paths(
    project_path,
    list(
      inputs = "inputs",
      orchestration = "orchestration",
      outputs = .IASI_PARAMETER_DEFAULTS$outputs,
      releases = .IASI_PARAMETER_DEFAULTS$releases
    )
  )
}

.project_output_source = function(project, source = NULL) {
  if (!is.null(source)) {
    if (!is.character(source) || length(source) != 1L || is.na(source) || !nzchar(source)) {
      stop("`source` must be one non-empty directory path.", call. = FALSE)
    }

    if (.is_absolute_path(source)) return(source)
    return(file.path(project$path, source))
  }

  if (!is.null(project$paths$docs)) return(project$paths$docs)

  .natural_project_output_source(project)
}

.natural_project_output_source = function(project) {
  formats = project$render_formats
  if (is.null(formats) || !length(formats)) formats = .project_declared_formats(project)
  if (is.null(formats) || !length(formats)) return(project$path)

  output_paths = lapply(formats, function(format) .profile_config(project, format)$output_path)
  configured = !vapply(output_paths, is.null, logical(1))

  if (!any(configured)) return(project$path)

  if (!all(configured)) {
    stop("Quarto outputs do not share one publication root; use `source` or configure a common output directory.", call. = FALSE)
  }

  parents = unique(vapply(output_paths, dirname, character(1)))
  if (length(parents) != 1L) {
    stop("Quarto outputs do not share one publication root; use `source` or configure a common output directory.", call. = FALSE)
  }

  parents[[1L]]
}
