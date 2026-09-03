.prepare_regular_project = function(project) {
  results = lapply(
    project$folders,
    .prepare_regular_folder
  )

  folder_indexes = vapply(
    results,
    `[[`,
    character(1),
    "path"
  )

  index_changed = vapply(
    results,
    `[[`,
    logical(1),
    "changed"
  )

  names(results) = vapply(
    project$folders,
    `[[`,
    character(1),
    "path"
  )

  items = .book_top_level_items(project)

  ordered_chapters = unlist(
    lapply(
      items,
      function(item) {
        if (identical(item$kind, "document")) {
          return(
            .relative_path(
              item$path,
              project$path
            )
          )
        }

        .relative_path(
          results[[item$path]]$path,
          project$path
        )
      }
    ),
    use.names = FALSE
  )

  chapters = c(
    "index.qmd",
    ordered_chapters
  )

  structure_path = file.path(
    project$path,
    "_book-structure.yml"
  )

  structure_changed = .write_if_changed(
    .book_yaml(chapters),
    structure_path
  )

  artifacts = c(
    .relative_paths(
      folder_indexes,
      project$path
    ),
    "_book-structure.yml"
  )

  project$publication = .new_publication(
    path = project$path,
    type = project$type,
    strategy = project$strategy,
    chapters = chapters,
    artifacts = artifacts,
    changed = any(index_changed) || structure_changed
  )

  project
}

.prepare_regular_folder = function(folder) {
  target = file.path(
    folder$path,
    "index.qmd"
  )

  front_matter = .read_regular_front_matter(
    folder$path
  )

  content = character()

  if (length(front_matter)) {
    content = c(
      content,
      front_matter,
      ""
    )
  }

  content = c(
    content,
    .generated_header,
    ""
  )

  includes = .include_lines(
    basename(folder$documents)
  )

  if (length(includes)) {
    content = c(
      content,
      includes
    )
  } else {
    content = utils::head(
      content,
      -1L
    )
  }

  list(
    path = target,
    changed = .write_if_changed(
      content,
      target
    )
  )
}

.read_regular_front_matter = function(path) {
  front_matter_path = file.path(
    path,
    "front-matter.quarto"
  )

  if (!file.exists(front_matter_path)) {
    return(character())
  }

  readLines(
    front_matter_path,
    warn = FALSE,
    encoding = "UTF-8"
  )
}
