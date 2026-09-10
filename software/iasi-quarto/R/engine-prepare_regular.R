.prepare_regular_project = function(project) {
  front_matter = .book_section_plan(
    section = project$front_matter,
    project = project,
    position = "front"
  )

  back_matter = .book_section_plan(
    section = project$back_matter,
    project = project,
    position = "back"
  )

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
      project$root_documents,
      project$path
    ),
    .relative_paths(
      .book_section_entry_paths(front_matter$entries),
      project$path
    ),
    .relative_paths(
      folder_indexes,
      project$path
    ),
    .relative_paths(
      .book_section_entry_paths(back_matter$entries),
      project$path
    ),
    .relative_paths(
      back_matter$appendices,
      project$path
    )
  )

  structure_path = .book_structure_path(project)

  structure_changed = .write_if_changed(
    c(
      .book_yaml(utils::head(chapters, length(chapters) - length(back_matter$appendices))),
      .book_appendix_lines(
        back_matter$appendices,
        project$path
      )
    ),
    structure_path
  )

  artifacts = c(
    .relative_paths(
      folder_indexes,
      project$path
    ),
    .book_structure_relative_path(),
    .navbar_left_relative_path()
  )

  navbar_changed = .prepare_navbar_left(project)

  project$publication = .new_publication(
    path = project$path,
    type = project$type,
    strategy = project$strategy,
    chapters = chapters,
    artifacts = artifacts,
    changed = any(index_changed) || structure_changed || navbar_changed
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
