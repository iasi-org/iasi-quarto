.prepare_regular <- function(project) {
  .assert_valid_project(project)

  chapters <- c(
    "index.qmd",
    .numbered_root_documents(project)
  )

  artifacts <- character()
  changed <- FALSE

  for (folder in project$folders) {
    if (identical(folder$strategy, "direct")) {
      chapters <- c(
        chapters,
        .direct_folder_documents(folder, project$path)
      )
      next
    }

    if (!identical(folder$strategy, "regular")) {
      next
    }

    source_files <- sort(list.files(
      path = folder$path,
      pattern = "\\.qmd$",
      full.names = FALSE,
      recursive = FALSE,
      ignore.case = TRUE
    ))

    source_files <- source_files[
      tolower(source_files) != "index.qmd"
    ]

    index_path <- file.path(folder$path, "index.qmd")
    index_changed <- .write_if_changed(
      .include_lines(source_files),
      index_path
    )

    changed <- changed || index_changed

    relative_index <- .relative_path(
      index_path,
      project$path
    )

    chapters <- c(chapters, relative_index)
    artifacts <- c(artifacts, relative_index)
  }

  structure_path <- file.path(
    project$path,
    "_book-structure.yml"
  )

  structure_changed <- .write_if_changed(
    .book_yaml(chapters),
    structure_path
  )

  changed <- changed || structure_changed
  artifacts <- unique(c(artifacts, "_book-structure.yml"))

  project$publication <- .new_publication(
    path = project$path,
    type = "book",
    source = "regular",
    chapters = chapters,
    artifacts = artifacts,
    changed = changed
  )

  project
}
