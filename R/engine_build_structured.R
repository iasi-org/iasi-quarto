.build_structured <- function(project) {
  .assert_valid_project(project)

  chapters <- .structured_chapters(project)
  content <- .structured_yaml(project)
  output <- file.path(project$path, "_book-structure.yml")
  changed <- .write_if_changed(content, output)

  project$publication <- .new_publication(
    path = project$path,
    type = "book",
    source = "structured",
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

  if (length(root_documents)) {
    lines <- c(lines, sprintf('    - "%s"', root_documents))
  }

  for (folder in project$folders) {
    if (identical(folder$strategy, "direct")) {
      documents <- .direct_folder_documents(folder, project$path)

      if (length(documents)) {
        lines <- c(lines, sprintf('    - "%s"', documents))
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

    lines <- c(lines, sprintf('    - part: "%s"', part_index))

    if (length(documents)) {
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
    if (identical(folder$strategy, "direct")) {
      chapters <- c(
        chapters,
        .direct_folder_documents(folder, project$path)
      )
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
