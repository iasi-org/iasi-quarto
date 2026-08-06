.write_if_changed <- function(content, path) {
  current <- if (file.exists(path)) {
    readLines(path, warn = FALSE, encoding = "UTF-8")
  } else {
    character()
  }

  if (identical(current, content)) return(FALSE)

  writeLines(content, path, useBytes = TRUE)
  TRUE
}

.relative_path <- function(path, root) {
  if (!length(path)) {
    return(character())
  }

  root <- .slash(normalizePath(root, winslash = "/", mustWork = TRUE))
  paths <- .slash(normalizePath(path, winslash = "/", mustWork = FALSE))
  prefix <- paste0(root, "/")

  vapply(
    paths,
    function(item) {
      if (startsWith(item, prefix)) {
        substring(item, nchar(prefix) + 1L)
      } else {
        item
      }
    },
    character(1),
    USE.NAMES = FALSE
  )
}

.slash <- function(path) {
  gsub("\\\\", "/", path)
}
