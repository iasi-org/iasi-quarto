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
