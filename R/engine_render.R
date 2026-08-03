.render_html = function() {

  .render_profile("html")

}


.render_pdf = function() {

  .render_profile("pdf")

}


.render_profile = function(profile) {

  status = system2(
    command = "quarto",
    args = c(
      "render",
      "--profile",
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