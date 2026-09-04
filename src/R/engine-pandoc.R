.create_pandoc = function(html_path, pandoc_path) {
  .remove_pandoc(pandoc_path)
  .create_pandoc_dir(pandoc_path)
  .copy_html_to_pandoc(html_path, pandoc_path)
  .normalize_pandoc(pandoc_path)

  invisible(pandoc_path)
}

.remove_pandoc = function(path) {
  if (!is.null(path) && dir.exists(path)) {
    unlink(
      path,
      recursive = TRUE,
      force = TRUE
    )
  }

  invisible(TRUE)
}

.create_pandoc_dir = function(path) {
  if (is.null(path) || !nzchar(path)) {
    stop(
      "Pandoc preparation requires a resolved Pandoc output directory.",
      call. = FALSE
    )
  }

  dir.create(
    path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  invisible(path)
}

.copy_html_to_pandoc = function(source, target) {
  if (is.null(source) || !dir.exists(source)) {
    stop(
      sprintf(
        "Pandoc preparation requires the rendered HTML output directory%s.",
        if (is.null(source)) "" else sprintf(" '%s'", source)
      ),
      call. = FALSE
    )
  }

  source = normalizePath(
    source,
    winslash = "/",
    mustWork = TRUE
  )

  entries = list.files(
    source,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = TRUE,
    include.dirs = TRUE
  )

  if (!length(entries)) {
    return(invisible(TRUE))
  }

  relative = substring(
    entries,
    nchar(source) + 2L
  )

  info = file.info(entries)
  directories = !is.na(info$isdir) & info$isdir

  if (any(directories)) {
    for (path in file.path(target, relative[directories])) {
      dir.create(
        path,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
  }

  files = !directories

  if (any(files)) {
    destinations = file.path(
      target,
      relative[files]
    )

    for (directory in unique(dirname(destinations))) {
      dir.create(
        directory,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }

    copied = file.copy(
      entries[files],
      destinations,
      overwrite = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )

    if (!all(copied)) {
      stop(
        "Could not copy the rendered HTML output to the Pandoc input directory.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

.normalize_pandoc = function(path) {
  .normalize_pandoc_ids(path)

  invisible(TRUE)
}

.normalize_pandoc_ids = function(path) {
  files = list.files(
    path,
    pattern = "\\.html?$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )

  for (file in files) {
    .normalize_pandoc_file_ids(file)
  }

  invisible(TRUE)
}

.normalize_pandoc_file_ids = function(file) {
  content = readLines(
    file,
    warn = FALSE,
    encoding = "UTF-8"
  )

  normalized = .remove_pandoc_toc_title_id(content)

  if (!identical(content, normalized)) {
    writeLines(
      normalized,
      file,
      useBytes = TRUE
    )
  }

  invisible(TRUE)
}

.remove_pandoc_toc_title_id = function(content) {
  content = gsub(
    ' id="toc-title"',
    "",
    content,
    fixed = TRUE
  )

  gsub(
    " id='toc-title'",
    "",
    content,
    fixed = TRUE
  )
}
