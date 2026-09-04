.write_if_changed = function(content, path) {
  current = if (file.exists(path)) {
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )
  } else {
    character()
  }

  if (identical(current, content)) {
    return(FALSE)
  }

  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  writeLines(
    content,
    path,
    useBytes = TRUE
  )

  TRUE
}
