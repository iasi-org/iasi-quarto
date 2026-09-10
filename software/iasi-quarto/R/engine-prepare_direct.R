.prepare_direct_project = function(project) {
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

  documents = unlist(
    lapply(
      project$folders,
      function(folder) {
        folder$documents
      }
    ),
    use.names = FALSE
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
      documents,
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

  changed = .write_if_changed(
    c(
      .book_yaml(utils::head(chapters, length(chapters) - length(back_matter$appendices))),
      .book_appendix_lines(
        back_matter$appendices,
        project$path
      )
    ),
    structure_path
  )

  navbar_changed = .prepare_navbar_left(project)

  project$publication = .new_publication(
    path = project$path,
    type = project$type,
    strategy = project$strategy,
    chapters = chapters,
    artifacts = c(
      .book_structure_relative_path(),
      .navbar_left_relative_path()
    ),
    changed = changed || navbar_changed
  )

  project
}
