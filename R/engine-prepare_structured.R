.prepare_structured_project = function(project) {
  chapters = .structured_chapters(project)

  structure_path = file.path(
    project$path,
    "_book-structure.yml"
  )

  changed = .write_if_changed(
    .structured_book_yaml(project),
    structure_path
  )

  project$publication = .new_publication(
    path = project$path,
    type = project$type,
    strategy = project$strategy,
    chapters = chapters,
    artifacts = "_book-structure.yml",
    changed = changed
  )

  project
}

.structured_chapters = function(project) {
  chapters = "index.qmd"
  items = .book_top_level_items(project)

  for (item in items) {
    if (identical(item$kind, "document")) {
      chapters = c(
        chapters,
        .relative_path(
          item$path,
          project$path
        )
      )

      next
    }

    folder = item$folder

    chapters = c(
      chapters,
      .relative_path(
        file.path(
          folder$path,
          "index.qmd"
        ),
        project$path
      ),
      .relative_paths(
        folder$documents,
        project$path
      )
    )
  }

  chapters
}

.structured_book_yaml = function(project) {
  lines = c(
    "book:",
    "  chapters:",
    '    - "index.qmd"'
  )

  items = .book_top_level_items(project)

  for (item in items) {
    if (identical(item$kind, "document")) {
      lines = c(
        lines,
        sprintf(
          '    - "%s"',
          .relative_path(
            item$path,
            project$path
          )
        )
      )

      next
    }

    folder = item$folder
    part = .relative_path(
      file.path(
        folder$path,
        "index.qmd"
      ),
      project$path
    )

    lines = c(
      lines,
      sprintf(
        '    - part: "%s"',
        part
      )
    )

    documents = .relative_paths(
      folder$documents,
      project$path
    )

    if (length(documents)) {
      lines = c(
        lines,
        "      chapters:",
        sprintf(
          '        - "%s"',
          documents
        )
      )
    }
  }

  lines
}
