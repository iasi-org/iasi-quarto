.prepare_structured_project = function(project) {
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

  chapters = .structured_chapters(
    project = project,
    front_matter = front_matter,
    back_matter = back_matter
  )

  structure_path = .book_structure_path(project)

  changed = .write_if_changed(
    .structured_book_yaml(
      project = project,
      front_matter = front_matter,
      back_matter = back_matter
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

.structured_chapters = function(project, front_matter, back_matter) {
  chapters = c(
    "index.qmd",
    .relative_paths(
      project$root_documents,
      project$path
    ),
    .relative_paths(
      .book_section_entry_paths(front_matter$entries),
      project$path
    )
  )

  for (folder in project$folders) {
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

  c(
    chapters,
    .relative_paths(
      .book_section_entry_paths(back_matter$entries),
      project$path
    ),
    .relative_paths(
      back_matter$appendices,
      project$path
    )
  )
}

.structured_book_yaml = function(project, front_matter, back_matter) {
  lines = c(
    "book:",
    "  chapters:",
    '    - "index.qmd"',
    .book_chapter_lines(
      project$root_documents,
      project$path
    ),
    .book_section_entry_lines(
      front_matter$entries,
      project$path
    )
  )

  for (folder in project$folders) {
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

  c(
    lines,
    .book_section_entry_lines(
      back_matter$entries,
      project$path
    ),
    .book_appendix_lines(
      back_matter$appendices,
      project$path
    )
  )
}
