.prepare_direct_project = function(project) {
  items = .book_top_level_items(project)

  documents = unlist(
    lapply(
      items,
      function(item) {
        if (identical(item$kind, "document")) {
          return(item$path)
        }

        item$folder$documents
      }
    ),
    use.names = FALSE
  )

  chapters = c(
    "index.qmd",
    .relative_paths(
      documents,
      project$path
    )
  )

  structure_path = file.path(
    project$path,
    "_book-structure.yml"
  )

  changed = .write_if_changed(
    .book_yaml(chapters),
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
