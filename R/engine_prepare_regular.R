.prepare_regular = function(project) {
  .assert_valid_project(project)

  chapters = c("index.qmd", .numbered_root_documents(project)  )

  for (folder in project$folders) {
      if (identical(folder$strategy, "regular")) {
          chapters = c( chapters, .regular_folder_documents(folder, project$path))
          next
      }
      if (identical(folder$strategy, "direct")) {  
          chapters = c(chapters, .direct_folder_documents(folder, project$path))
      }
  }

  structure_path = file.path(project$path, "_book-structure.yml" )
  changed        = .write_if_changed(.book_yaml(chapters), structure_path)

  project$publication = .new_publication(
    path = project$path,
    type = "book",
    source = "regular",
    chapters = chapters,
    artifacts = "_book-structure.yml",
    changed = changed
  )

  project
}
.regular_folder_documents = function(folder, project_path) {
  source_files = sort(list.files(
    path = folder$path,
    pattern = "^[0-9]+-.*\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  unname(vapply(
    source_files,
    function(path) {
      .relative_path(
        path,
        project_path
      )
    },
    character(1)
  ))
}