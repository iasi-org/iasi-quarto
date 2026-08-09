.render = function(publication, format, type = publication$type) {
  .render_profile(
    path = publication$path,
    profile = format,
    type = type
  )

  publication$rendered = TRUE

  publication$profiles = unique(c(
    publication$profiles,
    format
  ))

  publication
}

.render_profile = function(path, profile, type = NULL) {
  previous_directory = setwd(path)

  on.exit(
    setwd(previous_directory),
    add = TRUE
  )

  active_profile = profile

  if (!is.null(type)) {
    runtime_profile = .create_quarto_type_profile(
      path = path,
      type = type
    )

    on.exit(
      unlink(runtime_profile$file),
      add = TRUE
    )

    active_profile = paste(
      runtime_profile$name,
      profile,
      sep = ","
    )
  }

  status = system2(
    command = "quarto",
    args = c(
      "render",
      "--profile",
      active_profile,
      "--to",
      profile
    )
  )

  if (!identical(status, 0L)) {
    stop(
      sprintf(
        "Quarto rendering failed for profile '%s' with status %s.",
        profile,
        status
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.create_quarto_type_profile = function(path, type) {
  file = tempfile(
    pattern = "_quarto-iasi-runtime-",
    tmpdir = path,
    fileext = ".yml"
  )

  yaml::write_yaml(
    list(
      project = list(
        type = type
      )
    ),
    file
  )

  name = sub(
    "^_quarto-(.*)\\.yml$",
    "\\1",
    basename(file)
  )

  list(
    name = name,
    file = file
  )
}
