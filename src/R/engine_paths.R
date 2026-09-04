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

.project_output_paths = function(project, formats = NULL) {
  if (is.null(formats)) {
    formats = .project_declared_formats(project)
  }

  if (!length(formats)) {
    return(character())
  }

  paths = vapply(
    formats,
    function(format) {
      path = .profile_config(project, format)$output_path

      if (is.null(path)) {
        return(NA_character_)
      }

      normalizePath(
        path,
        winslash = "/",
        mustWork = FALSE
      )
    },
    character(1)
  )

  names(paths) = formats
  paths[!is.na(paths) & nzchar(paths)]
}

.project_output_root = function(project, formats = NULL) {
  paths = .project_output_paths(project, formats)

  if (!length(paths)) {
    return(NULL)
  }

  parents = unique(dirname(paths))

  if (length(parents) != 1L) {
    stop(
      "Quarto profile outputs do not share one publication root; use `source` explicitly or configure sibling output directories.",
      call. = FALSE
    )
  }

  parents[[1L]]
}

.project_output_source = function(project, source = NULL) {
  if (!is.null(source)) {
    if (!is.character(source) ||
        length(source) != 1L ||
        is.na(source) ||
        !nzchar(source)) {
      stop("`source` must be one non-empty directory path.", call. = FALSE)
    }

    if (.is_absolute_path(source)) {
      return(source)
    }

    return(file.path(project$path, source))
  }

  root = .project_output_root(project)

  if (is.null(root)) {
    stop(
      "Could not infer a publication output root from the declared Quarto profiles; configure `project.output-dir` or pass `source` explicitly.",
      call. = FALSE
    )
  }

  root
}
