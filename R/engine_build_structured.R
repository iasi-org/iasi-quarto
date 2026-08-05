.build_structured <- function(project) {

  if (!inherits(project, "iasi_quarto_project")) {
    stop(
      "'project' must be an object returned by discover().",
      call. = FALSE
    )
  }

  if (!isTRUE(project$valid)) {
    stop(
      "Project validation failed.",
      call. = FALSE
    )
  }

  output <- file.path(
    project$path,
    "_book-structure.yml"
  )

  content <- .structured_yaml(project)

  changed <- .write_if_changed(
    content = content,
    path = output
  )

  chapters <- .structured_chapters(project)

  project$built <- TRUE
  project$publication <- .new_publication(
    path = project$path,
    type = "book",
    source = project$type,
    chapters = chapters,
    artifacts = "_book-structure.yml",
    changed = changed
  )

  project
}


.structured_yaml <- function(project) {

  lines <- c(
    "book:",
    "  chapters:",
    '    - "index.qmd"'
  )

  root_documents <- .numbered_root_documents(project)

  if (length(root_documents) > 0L) {
    lines <- c(
      lines,
      sprintf('    - "%s"', root_documents)
    )
  }

  for (folder in project$folders) {

    if (identical(folder$strategy, "as-is")) {

      documents <- folder$documents[
        !tolower(basename(folder$documents)) %in%
          c("index.qmd", "00-index.qmd")
      ]

      if (length(documents) > 0L) {
        lines <- c(
          lines,
          sprintf(
            '    - "%s"',
            .relative_path(documents, project$path)
          )
        )
      }

      next
    }

    part_index <- .relative_path(
      file.path(folder$path, "index.qmd"),
      project$path
    )

    documents <- folder$documents[
      tolower(basename(folder$documents)) != "index.qmd"
    ]

    lines <- c(
      lines,
      sprintf('    - part: "%s"', part_index)
    )

    if (length(documents) > 0L) {
      lines <- c(
        lines,
        "      chapters:",
        sprintf(
          '        - "%s"',
          .relative_path(documents, project$path)
        )
      )
    }
  }

  lines
}


.structured_chapters <- function(project) {

  chapters <- c(
    "index.qmd",
    .numbered_root_documents(project)
  )

  for (folder in project$folders) {

    if (identical(folder$strategy, "as-is")) {

      documents <- folder$documents[
        !tolower(basename(folder$documents)) %in%
          c("index.qmd", "00-index.qmd")
      ]

      if (length(documents) > 0L) {
        chapters <- c(
          chapters,
          .relative_path(documents, project$path)
        )
      }

      next
    }

    part_index <- .relative_path(
      file.path(folder$path, "index.qmd"),
      project$path
    )

    documents <- folder$documents[
      tolower(basename(folder$documents)) != "index.qmd"
    ]

    chapters <- c(
      chapters,
      part_index,
      .relative_path(documents, project$path)
    )
  }

  chapters
}


.numbered_root_documents <- function(project) {

  documents <- project$root_documents[
    grepl(
      "^[0-9]+-.*\\.qmd$",
      basename(project$root_documents),
      ignore.case = TRUE
    )
  ]

  .relative_path(
    sort(documents),
    project$path
  )
}
