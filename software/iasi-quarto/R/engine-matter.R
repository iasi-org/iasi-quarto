.discover_book_section = function(path, project) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path) ||
      !dir.exists(path) ||
      .is_excluded_directory(path, project)
  ) {
    return(list(
      path = NULL,
      items = list()
    ))
  }

  list(
    path = .normalise_project_path(path),
    items = .discover_book_section_items(
      path = path,
      project = project
    )
  )
}

.discover_book_section_items = function(path, project) {
  documents = list.files(
    path = path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  )

  documents = documents[
    tolower(basename(documents)) != "index.qmd"
  ]

  directories = list.dirs(
    path = path,
    recursive = FALSE,
    full.names = TRUE
  )

  directories = .filter_excluded_directories(
    directories,
    project
  )

  if (length(directories)) {
    directories = directories[
      !startsWith(
        basename(directories),
        "."
      )
    ]
  }

  document_items = lapply(
    documents,
    function(document) {
      list(
        kind = "document",
        name = basename(document),
        path = .normalise_project_path(document)
      )
    }
  )

  folder_items = lapply(
    directories,
    function(directory) {
      index = file.path(
        directory,
        "index.qmd"
      )

      list(
        kind = "folder",
        name = basename(directory),
        path = .normalise_project_path(directory),
        index = if (file.exists(index)) {
          .normalise_project_path(index)
        } else {
          NULL
        },
        items = .discover_book_section_items(
          path = directory,
          project = project
        )
      )
    }
  )

  items = c(
    document_items,
    folder_items
  )

  if (!length(items)) {
    return(items)
  }

  names = vapply(
    items,
    `[[`,
    character(1),
    "name"
  )

  items[
    order(
      tolower(names),
      names
    )
  ]
}

.book_section_plan = function(section, project, position) {
  if (
    is.null(section) ||
      !is.list(section) ||
      !length(section$items)
  ) {
    return(.empty_book_section_plan())
  }

  .plan_book_section_items(
    items = section$items,
    project = project,
    position = position
  )
}

.empty_book_section_plan = function() {
  list(
    entries = list(),
    appendices = character()
  )
}

.combine_book_section_plans = function(...) {
  plans = list(...)

  if (!length(plans)) {
    return(.empty_book_section_plan())
  }

  list(
    entries = unlist(
      lapply(
        plans,
        `[[`,
        "entries"
      ),
      recursive = FALSE
    ),
    appendices = unlist(
      lapply(
        plans,
        `[[`,
        "appendices"
      ),
      use.names = FALSE
    )
  )
}

.plan_book_section_items = function(items, project, position) {
  result = .empty_book_section_plan()

  for (item in items) {
    if (identical(item$kind, "document")) {
      result$entries = c(
        result$entries,
        list(list(
          kind = "document",
          path = item$path
        ))
      )

      next
    }

    result = .combine_book_section_plans(
      result,
      .plan_book_section_folder(
        folder = item,
        project = project,
        position = position
      )
    )
  }

  result
}

.plan_book_section_folder = function(folder, project, position) {
  has_index = !is.null(folder$index)

  if (identical(position, "back") && !has_index) {
    result = .empty_book_section_plan()

    for (item in folder$items) {
      if (identical(item$kind, "document")) {
        result$appendices = c(
          result$appendices,
          item$path
        )

        next
      }

      result = .combine_book_section_plans(
        result,
        .plan_book_section_folder(
          folder = item,
          project = project,
          position = position
        )
      )
    }

    return(result)
  }

  if (identical(project$strategy, "parted") && has_index) {
    result = .empty_book_section_plan()
    chapters = .book_section_part_chapters(
      folder = folder,
      position = position
    )

    result$entries = list(list(
      kind = "part",
      path = folder$index,
      chapters = chapters
    ))

    nested = .book_section_nested_folder_plan(
      items = folder$items,
      project = project,
      position = position
    )

    return(
      .combine_book_section_plans(
        result,
        nested
      )
    )
  }

  result = .empty_book_section_plan()

  if (has_index) {
    result$entries = list(list(
      kind = "document",
      path = folder$index
    ))
  }

  .combine_book_section_plans(
    result,
    .plan_book_section_items(
      items = folder$items,
      project = project,
      position = position
    )
  )
}

.book_section_part_chapters = function(folder, position) {
  chapters = character()

  for (item in folder$items) {
    if (identical(item$kind, "document")) {
      chapters = c(
        chapters,
        item$path
      )

      next
    }

    if (!is.null(item$index)) {
      next
    }

    if (identical(position, "back")) {
      next
    }

    chapters = c(
      chapters,
      .book_section_unindexed_documents(item)
    )
  }

  chapters
}

.book_section_unindexed_documents = function(folder) {
  documents = character()

  for (item in folder$items) {
    if (identical(item$kind, "document")) {
      documents = c(
        documents,
        item$path
      )

      next
    }

    if (is.null(item$index)) {
      documents = c(
        documents,
        .book_section_unindexed_documents(item)
      )
    }
  }

  documents
}

.book_section_nested_folder_plan = function(items, project, position) {
  result = .empty_book_section_plan()

  for (item in items) {
    if (!identical(item$kind, "folder")) {
      next
    }

    if (is.null(item$index) && identical(position, "front")) {
      result = .combine_book_section_plans(
        result,
        .book_section_nested_folder_plan(
          items = item$items,
          project = project,
          position = position
        )
      )

      next
    }

    result = .combine_book_section_plans(
      result,
      .plan_book_section_folder(
        folder = item,
        project = project,
        position = position
      )
    )
  }

  result
}

.book_section_entry_paths = function(entries) {
  paths = character()

  for (entry in entries) {
    paths = c(
      paths,
      entry$path
    )

    if (identical(entry$kind, "part") && length(entry$chapters)) {
      paths = c(
        paths,
        entry$chapters
      )
    }
  }

  paths
}

.book_section_entry_lines = function(entries, root) {
  lines = character()

  for (entry in entries) {
    path = .relative_path(
      entry$path,
      root
    )

    if (identical(entry$kind, "document")) {
      lines = c(
        lines,
        sprintf(
          '    - "%s"',
          path
        )
      )

      next
    }

    lines = c(
      lines,
      sprintf(
        '    - part: "%s"',
        path
      )
    )

    chapters = .relative_paths(
      entry$chapters,
      root
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

.book_appendix_lines = function(paths, root) {
  appendices = .relative_paths(
    paths,
    root
  )

  if (!length(appendices)) {
    return(character())
  }

  c(
    "  appendices:",
    sprintf(
      '    - "%s"',
      appendices
    )
  )
}
