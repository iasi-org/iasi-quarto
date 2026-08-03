.discover_project = function(root = "chapters") {

  if (!dir.exists(root)) {
    stop(
      sprintf(
        "Directory '%s' does not exist.",
        root
      ),
      call. = FALSE
    )
  }

  list(
    root = root,
    chapters = .discover_chapters(root)
  )

}


.discover_chapters = function(root) {

  directories = list.dirs(
    path = root,
    recursive = FALSE,
    full.names = TRUE
  )

  directories = sort(directories)

  lapply(
    directories,
    .discover_documents
  )

}


.discover_documents = function(path) {

  documents = list.files(
    path = path,
    pattern = "\\.qmd$",
    full.names = TRUE
  )

  documents = sort(documents)

  list(
    name = basename(path),
    path = path,
    documents = documents
  )

}