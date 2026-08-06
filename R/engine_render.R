.render_profile = function(path, profile) {
  old = setwd(path)
  on.exit(setwd(old), add = TRUE)

  status = system2(command = "quarto",
                   args = c("render",
                            "--profile", profile,
                            "--to",      profile   )
                 )

  if (!identical(status, 0L)) {
    stop(sprintf("Quarto rendering failed for profile '%s' with status %s.", profile, status ),
         call. = FALSE)
  }

  invisible(TRUE)
}