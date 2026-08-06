.render = function(publication, format) {
  .render_profile(
    path = publication$path,
    profile = format
  )

  publication$rendered = TRUE

  publication$profiles = unique(c(
    publication$profiles,
    format
  ))

  publication
}

.render_profile = function(path, profile) {
  previous_directory = setwd(path)

  on.exit(
    setwd(previous_directory),
    add = TRUE
  )

  status = system2(
    command = "quarto",
    args = c(
      "render",
      "--profile",
      profile,
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
