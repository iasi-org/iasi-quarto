.prepare_parted_project = function(project) {
  chapters = .parted_chapters(project)

  structure_path = file.path(
    project$path,
    "_book-structure.yml"
  )

  changed = .write_if_changed(
    .parted_book_yaml(project),
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

.parted_chapters = function(project) {
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
      .parted_folder_chapters(
        folder = folder,
        project = project
      )
    )
  }

  chapters
}

.parted_folder_chapters = function(folder, project) {
  chapters = character()

  if (!is.null(folder$intro)) {
    chapters = c(
      chapters,
      .relative_path(
        folder$intro,
        project$path
      )
    )
  }

  for (item in folder$items) {
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

    chapter = item$chapter

    chapters = c(
      chapters,
      .relative_path(
        file.path(
          chapter$path,
          "index.qmd"
        ),
        project$path
      ),
      .relative_paths(
        chapter$documents,
        project$path
      )
    )
  }

  chapters
}

.parted_book_yaml = function(project) {
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

    chapters = .parted_folder_chapters(
      folder = folder,
      project = project
    )

    if (length(chapters)) {
      lines = c(
        lines,
        "      chapters:",
        sprintf(
          '        - "%s"',
          chapters
        )
      )
    }
  }

  lines
}
