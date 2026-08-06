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

  chapters = c(
    "index.qmd",
    .relative_paths(
      folder_indexes,
      project$path
    )
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

  content = c(
    .generated_header,
    "",
    sprintf(
      "# %s",
      .folder_title(folder$name)
    ),
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

.folder_title = function(name) {
  title = sub(
    "^[0-9]+-",
    "",
    name
  )

  title = gsub(
    "[-_]+",
    " ",
    title
  )

  title = trimws(title)

  if (!nzchar(title)) {
    return(name)
  }

  tools::toTitleCase(title)
}
