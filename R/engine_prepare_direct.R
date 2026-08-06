.prepare_direct <- function(project) {
  .assert_valid_project(project)

  root_documents <- .relative_path(
    project$root_documents,
    project$path
  )

  folder_documents <- unlist(
    lapply(
      project$folders,
      function(folder) {
        if (!identical(folder$strategy, "direct")) {
          return(character())
        }

        .direct_folder_documents(folder, project$path)
      }
    ),
    use.names = FALSE
  )

  project$publication <- .new_publication(
    path = project$path,
    type = "book",
    source = "direct",
    chapters = c(root_documents, folder_documents),
    artifacts = character(),
    changed = FALSE
  )

  project
}

.direct_folder_documents <- function(folder, project_path) {
  documents <- folder$documents[
    !tolower(basename(folder$documents)) %in%
      c("index.qmd", "00-index.qmd")
  ]

  .relative_path(documents, project_path)
}
