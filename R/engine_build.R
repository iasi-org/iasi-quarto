.supported_formats = c(
  "html",
  "pdf"
)

.resolve_build_formats = function(format = NULL) {
  selection = .normalise_build_selection(
    value = format,
    argument = "format"
  )

  if (identical(selection, "all")) {
    return(.supported_formats)
  }

  invalid = setdiff(
    selection,
    .supported_formats
  )

  if (length(invalid)) {
    stop(
      sprintf(
        "Unsupported format%s: %s. Supported formats are: %s.",
        if (length(invalid) == 1L) {
          ""
        } else {
          "s"
        },
        paste(
          sprintf(
            '"%s"',
            invalid
          ),
          collapse = ", "
        ),
        paste(
          sprintf(
            '"%s"',
            .supported_formats
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  selection
}

.project_supported_formats = function(project) {
  switch(
    project$type,
    website = "html",
    book = .supported_formats,
    stop(
      sprintf(
        "Unsupported Quarto project type '%s'.",
        .display_checked_value(project$type)
      ),
      call. = FALSE
    )
  )
}

.resolve_project_build_formats = function(project, formats) {
  supported = .project_supported_formats(project)
  resolved = intersect(formats, supported)

  if (!length(resolved)) {
    stop(
      sprintf(
        "Project '%s' [%s] does not support requested format%s: %s. Supported formats are: %s.",
        project$name,
        project$type,
        if (length(formats) == 1L) "" else "s",
        paste(
          sprintf('"%s"', formats),
          collapse = ", "
        ),
        paste(
          sprintf('"%s"', supported),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resolved
}

.select_build_books = function(plan, book = NULL) {
  selection = .normalise_build_selection(
    value = book,
    argument = "book"
  )

  if (isTRUE(plan$current)) {
    if (!identical(selection, "all")) {
      warning(
        paste0(
          "The selected path is an IASI Quarto publication. ",
          "The `book` selection will be ignored."
        ),
        call. = FALSE
      )
    }

    plan$selected_books = plan$books

    return(plan)
  }

  if (identical(selection, "all")) {
    plan$selected_books = plan$books

    return(plan)
  }

  candidates = plan$books

  relative_paths = unname(vapply(
    candidates,
    .relative_path,
    character(1),
    root = plan$path
  ))

  directory_names = basename(candidates)

  book_names = sub(
    "^[0-9]+-",
    "",
    directory_names
  )

  number_prefixes = sub(
    "-.*$",
    "",
    directory_names
  )

  resolved = character()
  missing = character()

  for (requested in selection) {
    matches = requested == relative_paths |
      requested == directory_names |
      requested == book_names

    if (grepl(
      "^[0-9]+$",
      requested
    )) {
      matches = matches |
        suppressWarnings(
          as.integer(number_prefixes)
        ) == as.integer(requested)
    }

    if (!any(matches)) {
      missing = c(
        missing,
        requested
      )

      next
    }

    resolved = c(
      resolved,
      candidates[matches]
    )
  }

  if (length(missing)) {
    warning(
      sprintf(
        "Books not found: %s.",
        paste(
          sprintf(
            '"%s"',
            missing
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  resolved = unique(resolved)

  if (!length(resolved)) {
    stop(
      "No IASI Quarto publications were selected.",
      call. = FALSE
    )
  }

  plan$books = resolved
  plan$selected_books = resolved

  plan
}

.normalise_build_selection = function(value, argument) {
  if (is.null(value)) {
    return("all")
  }

  value = unique(
    as.character(value)
  )

  if (
    !length(value) ||
      anyNA(value) ||
      any(!nzchar(value))
  ) {
    stop(
      sprintf(
        "`%s` must contain at least one non-empty value.",
        argument
      ),
      call. = FALSE
    )
  }

  if (
    "all" %in% value &&
      length(value) > 1L
  ) {
    stop(
      sprintf(
        '`%s = "all"` cannot be combined with other values.',
        argument
      ),
      call. = FALSE
    )
  }

  value
}

.render_build_project = function(project, formats) {
  publication = project$publication
  project_formats = .resolve_project_build_formats(
    project = project,
    formats = formats
  )

  for (format in project_formats) {
    message(sprintf(
      "Rendering '%s' as %s...",
      project$name,
      toupper(format)
    ))

    publication = .render(
      publication = publication,
      format = format
    )
  }

  project$publication = publication
  project$render_formats = project_formats
  project
}

.report_build = function(plan) {
  renders = sum(vapply(
    plan$projects,
    function(project) {
      length(project$render_formats)
    },
    integer(1)
  ))

  message("")
  message("IASI Quarto build")
  message("-----------------")
  message(sprintf(
    "Status  : %s",
    if (isTRUE(plan$rendered)) {
      "COMPLETED"
    } else {
      "INCOMPLETE"
    }
  ))
  message(sprintf(
    "Projects: %d",
    length(plan$projects)
  ))
  message(sprintf(
    "Renders : %d",
    renders
  ))
  message(sprintf(
    "Formats : %s",
    paste(
      plan$formats,
      collapse = ", "
    )
  ))
  message(sprintf(
    "Elapsed : %.2f seconds",
    plan$elapsed
  ))

  invisible(plan)
}
