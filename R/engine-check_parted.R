.check_parted_folder = function(path, project) {
  folder = .describe_parted_folder(
    path = path,
    project = project
  )

  errors = character()
  warnings = character()

  if (!folder$has_index) {
    errors = c(
      errors,
      sprintf(
        "Parted part is missing index.qmd: %s.",
        folder$name
      )
    )
  }

  if (length(folder$chapters)) {
    for (chapter in folder$chapters) {
      if (chapter$has_index) {
        next
      }

      errors = c(
        errors,
        sprintf(
          "Parted chapter is missing index.qmd: %s/%s.",
          folder$name,
          chapter$name
        )
      )
    }
  }

  errors = c(
    errors,
    .check_parted_item_numbering(
      folder = folder,
      project = project
    )
  )

  list(
    folder = folder,
    errors = errors,
    warnings = warnings
  )
}

.describe_parted_folder = function(path, project) {
  all_documents = sort(list.files(
    path = path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  intro = all_documents[
    tolower(basename(all_documents)) == "intro.qmd"
  ]

  if (length(intro)) {
    intro = .normalise_project_path(intro[[1L]])
  } else {
    intro = NULL
  }

  documents = all_documents[
    !tolower(basename(all_documents)) %in% c(
      "index.qmd",
      "intro.qmd"
    )
  ]

  if (isTRUE(project$numbered)) {
    documents = documents[
      grepl(
        "^[0-9]+-.*\\.qmd$",
        basename(documents),
        ignore.case = TRUE
      )
    ]
  }

  chapter_paths = list.dirs(
    path = path,
    recursive = FALSE,
    full.names = TRUE
  )

  if (length(chapter_paths)) {
    chapter_paths = chapter_paths[
      !startsWith(
        basename(chapter_paths),
        "."
      )
    ]
  }

  if (isTRUE(project$numbered) && length(chapter_paths)) {
    chapter_paths = chapter_paths[
      grepl(
        "^[0-9]+-",
        basename(chapter_paths)
      )
    ]
  }

  chapter_paths = sort(chapter_paths)

  chapters = lapply(
    chapter_paths,
    .describe_parted_chapter,
    project = project
  )

  folder = list(
    name = basename(path),
    path = .normalise_project_path(path),
    has_index = file.exists(
      file.path(
        path,
        "index.qmd"
      )
    ),
    intro = intro,
    documents = if (length(documents)) {
      unname(vapply(
        documents,
        .normalise_project_path,
        character(1)
      ))
    } else {
      character()
    },
    chapters = chapters
  )

  folder$items = .parted_folder_items(
    folder = folder,
    project = project
  )

  folder
}

.describe_parted_chapter = function(path, project) {
  all_documents = sort(list.files(
    path = path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  documents = all_documents[
    tolower(basename(all_documents)) != "index.qmd"
  ]

  if (isTRUE(project$numbered)) {
    documents = documents[
      grepl(
        "^[0-9]+-.*\\.qmd$",
        basename(documents),
        ignore.case = TRUE
      )
    ]
  }

  list(
    name = basename(path),
    path = .normalise_project_path(path),
    has_index = file.exists(
      file.path(
        path,
        "index.qmd"
      )
    ),
    documents = if (length(documents)) {
      unname(vapply(
        documents,
        .normalise_project_path,
        character(1)
      ))
    } else {
      character()
    }
  )
}

.parted_folder_items = function(folder, project) {
  document_items = lapply(
    folder$documents,
    function(path) {
      list(
        kind = "document",
        name = basename(path),
        path = path
      )
    }
  )

  chapter_items = lapply(
    folder$chapters,
    function(chapter) {
      list(
        kind = "chapter",
        name = chapter$name,
        path = chapter$path,
        chapter = chapter
      )
    }
  )

  items = c(
    document_items,
    chapter_items
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

  if (isTRUE(project$numbered)) {
    prefixes = vapply(
      items,
      function(item) {
        .numbered_prefix(item$name)
      },
      integer(1)
    )

    return(
      items[
        order(
          prefixes,
          tolower(names),
          names
        )
      ]
    )
  }

  items[
    order(
      tolower(names),
      names
    )
  ]
}

.check_parted_item_numbering = function(folder, project) {
  if (!isTRUE(project$numbered) || !length(folder$items)) {
    return(character())
  }

  prefixes = vapply(
    folder$items,
    function(item) {
      .numbered_prefix(item$name)
    },
    integer(1)
  )

  duplicated_prefixes = unique(
    prefixes[duplicated(prefixes)]
  )

  if (!length(duplicated_prefixes)) {
    return(character())
  }

  vapply(
    duplicated_prefixes,
    function(prefix) {
      names = vapply(
        folder$items[prefixes == prefix],
        `[[`,
        character(1),
        "name"
      )

      sprintf(
        "Duplicate numeric prefix %d inside parted part '%s': %s.",
        prefix,
        folder$name,
        paste(
          names,
          collapse = ", "
        )
      )
    },
    character(1)
  )
}
