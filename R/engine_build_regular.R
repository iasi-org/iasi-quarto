.build_regular <- function(project) {

  if (!inherits(project, "iasi_quarto_project")) {
    stop(
      "'project' must be an object returned by discover().",
      call. = FALSE
    )
  }

  if (!isTRUE(project$valid)) {
    stop(
      "Project validation failed.",
      call. = FALSE
    )
  }

  artifacts <- character()
  changed <- FALSE

  regular_folders <- Filter(
    function(folder) identical(folder$strategy, "regular"),
    project$folders
  )

  chapter_indexes <- vapply(
    regular_folders,
    function(folder) {

      qmd_files <- sort(list.files(
        path = folder$path,
        pattern = "\\.qmd$",
        full.names = FALSE,
        recursive = FALSE,
        ignore.case = TRUE
      ))

      source_files <- qmd_files[
        tolower(qmd_files) != "index.qmd"
      ]

      index_content <- .include_lines(source_files)

      index_path <- file.path(
        folder$path,
        "index.qmd"
      )

      index_changed <- .write_if_changed(
        content = index_content,
        path = index_path
      )

      changed <<- changed || index_changed

      relative_index <- .relative_path(
        index_path,
        project$path
      )

      artifacts <<- c(
        artifacts,
        relative_index
      )

      relative_index
    },
    character(1)
  )

  chapters <- c(
    "index.qmd",
    .numbered_root_documents(project),
    chapter_indexes
  )

  structure <- c(
    "book:",
    "  chapters:",
    sprintf('    - "%s"', chapters)
  )

  structure_path <- file.path(
    project$path,
    "_book-structure.yml"
  )

  structure_changed <- .write_if_changed(
    content = structure,
    path = structure_path
  )

  changed <- changed || structure_changed

  artifacts <- unique(c(
    artifacts,
    "_book-structure.yml"
  ))

  project$built <- TRUE
  project$publication <- .new_publication(
    path = project$path,
    type = "book",
    source = project$type,
    chapters = chapters,
    artifacts = artifacts,
    changed = changed
  )

  project
}


.include_lines <- function(files) {

  if (length(files) == 0L) {
    return(character())
  }

  lines <- unlist(
    lapply(
      files,
      function(file) {
        c(
          sprintf("{{< include %s >}}", file),
          ""
        )
      }
    ),
    use.names = FALSE
  )

  head(lines, -1L)
}
