.state_version = 2L

.state_path = function(project_path) {
  file.path(
    project_path,
    ".iasi",
    "quarto",
    "state.yml"
  )
}

.empty_state = function() {
  list(
    version = .state_version
  )
}

.read_state = function(project_path) {
  path = .state_path(project_path)

  if (!file.exists(path)) {
    return(.empty_state())
  }

  state = tryCatch(
    yaml::read_yaml(path),
    error = function(error) {
      stop(
        sprintf(
          "Invalid IASI Quarto state file '%s': %s",
          path,
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )

  if (is.null(state) || !is.list(state)) {
    return(.empty_state())
  }

  version = suppressWarnings(as.integer(state$version))

  if (length(version) != 1L || is.na(version) || version != .state_version) {
    return(.empty_state())
  }

  state
}

.write_state = function(project_path, state) {
  path = .state_path(project_path)
  directory = dirname(path)

  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  state$version = .state_version

  temporary = tempfile(
    "state-",
    tmpdir = directory,
    fileext = ".yml"
  )

  on.exit(
    unlink(temporary, force = TRUE),
    add = TRUE
  )

  yaml::write_yaml(
    state,
    temporary
  )

  if (file.exists(path)) {
    current = readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    )

    candidate = readLines(
      temporary,
      warn = FALSE,
      encoding = "UTF-8"
    )

    if (identical(current, candidate)) {
      return(invisible(FALSE))
    }
  }

  if (!file.copy(
    temporary,
    path,
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = FALSE
  )) {
    stop(
      sprintf(
        "Could not write IASI Quarto state file '%s'.",
        path
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.state_timestamp = function() {
  format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%OS6Z"
  )
}

.text_fingerprint = function(lines) {
  path = tempfile("iasi-quarto-fingerprint-")

  on.exit(
    unlink(path, force = TRUE),
    add = TRUE
  )

  writeLines(
    enc2utf8(lines),
    path,
    useBytes = TRUE
  )

  unname(tools::md5sum(path)[[1L]])
}

.tree_files = function(path, exclude_directories = character()) {
  root = normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  files = character()
  relative = character()
  visited = character()

  walk = function(directory, prefix = "") {
    key = tolower(normalizePath(
      directory,
      winslash = "/",
      mustWork = TRUE
    ))

    if (key %in% visited) {
      return(invisible(NULL))
    }

    visited <<- c(visited, key)

    entries = list.files(
      directory,
      recursive = FALSE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )

    for (entry in entries) {
      name = basename(entry)
      entry_relative = if (nzchar(prefix)) {
        file.path(prefix, name)
      } else {
        name
      }

      entry_relative = gsub(
        "\\\\",
        "/",
        entry_relative
      )

      if (dir.exists(entry)) {
        if (name %in% exclude_directories || file.exists(file.path(entry, ".publish"))) {
          next
        }

        walk(
          entry,
          entry_relative
        )

        next
      }

      if (!file.exists(entry)) {
        next
      }

      files <<- c(files, entry)
      relative <<- c(relative, entry_relative)
    }

    invisible(NULL)
  }

  walk(root)

  list(
    files = files,
    relative = relative
  )
}

.tree_fingerprint = function(path, exclude_directories = character()) {
  if (!dir.exists(path)) {
    return(NULL)
  }

  entries = .tree_files(
    path,
    exclude_directories = exclude_directories
  )

  files = entries$files
  relative = entries$relative

  if (!length(files)) {
    return(.text_fingerprint(character()))
  }

  order = order(relative)
  files = files[order]
  relative = relative[order]

  hashes = unname(
    tools::md5sum(files)
  )

  if (anyNA(hashes)) {
    stop(
      sprintf(
        "Could not fingerprint all files below '%s'.",
        path
      ),
      call. = FALSE
    )
  }

  .text_fingerprint(
    paste(
      enc2utf8(relative),
      hashes,
      sep = "\t"
    )
  )
}

.project_source_exclusions = function(project) {
  paths = .project_output_paths(project)
  pandoc = .build_output_config(project, "pandoc")$output_path

  if (!is.null(pandoc)) {
    paths = c(paths, pandoc = normalizePath(pandoc, winslash = "/", mustWork = FALSE))
  }

  if (!length(paths)) {
    return(character())
  }

  root = normalizePath(project$path, winslash = "/", mustWork = TRUE)
  prefix = paste0(root, "/")
  local = paths[startsWith(tolower(paths), tolower(prefix))]

  if (!length(local)) {
    return(character())
  }

  relative = substring(local, nchar(prefix) + 1L)
  unique(vapply(
    strsplit(relative, "/", fixed = TRUE),
    `[[`,
    character(1),
    1L
  ))
}

.source_fingerprint = function(project) {
  .tree_fingerprint(
    project$path,
    exclude_directories = unique(c(
      ".git",
      ".iasi",
      ".quarto",
      ".Rproj.user",
      "_freeze",
      "node_modules",
      .project_source_exclusions(project)
    ))
  )
}

.profile_output_fingerprint = function(project, format) {
  path = .profile_config(project, format)$output_path

  if (is.null(path)) {
    return(NULL)
  }

  .tree_fingerprint(path)
}

.publish_source_fingerprint = function(project, source = NULL) {
  path = .project_output_source(project, source)
  .tree_fingerprint(path)
}

.record_build_state = function(project) {
  formats = project$render_formats

  if (!length(formats) || !isTRUE(project$publication$rendered)) {
    return(invisible(project))
  }

  source = .source_fingerprint(project)
  state = .read_state(project$path)
  format_state = state$build$formats

  if (is.null(format_state) || !is.list(format_state)) {
    format_state = list()
  }

  for (format in formats) {
    output = .profile_output_fingerprint(project, format)

    if (is.null(output)) {
      stop(
        sprintf(
          "Build completed for '%s' but the output for profile '%s' does not exist.",
          project$name,
          format
        ),
        call. = FALSE
      )
    }

    format_state[[format]] = list(
      source = source,
      output = output
    )
  }

  state$build = list(
    formats = format_state,
    timestamp = .state_timestamp()
  )

  .write_state(
    project$path,
    state
  )

  invisible(project)
}

.record_publish_state = function(project, source = NULL, dest = "_publish") {
  if (!isTRUE(project$published)) {
    return(invisible(project))
  }

  output = .publish_source_fingerprint(
    project,
    source = source
  )

  if (is.null(output)) {
    stop(
      sprintf(
        "Published source does not exist for '%s'.",
        project$name
      ),
      call. = FALSE
    )
  }

  dest = .validate_publish_dest(dest)
  publish_path = .publish_destination_path(project, dest)
  published = .tree_fingerprint(publish_path)

  if (is.null(published)) {
    stop(
      sprintf(
        "Published destination does not exist for '%s'.",
        project$name
      ),
      call. = FALSE
    )
  }

  state = .read_state(project$path)
  state$publish = list(
    build = output,
    dest = dest,
    output = published,
    timestamp = .state_timestamp()
  )

  .write_state(
    project$path,
    state
  )

  invisible(project)
}

.build_required = function(project, formats) {
  if (!length(formats)) {
    return(FALSE)
  }

  state = .read_state(project$path)
  source = .source_fingerprint(project)
  format_state = state$build$formats

  if (is.null(format_state) || !is.list(format_state)) {
    return(TRUE)
  }

  !all(vapply(
    formats,
    function(format) {
      recorded = format_state[[format]]

      if (is.null(recorded) || !is.list(recorded)) {
        return(FALSE)
      }

      current_output = .profile_output_fingerprint(project, format)

      !is.null(current_output) &&
        identical(recorded$source, source) &&
        identical(recorded$output, current_output)
    },
    logical(1)
  ))
}

.publish_required = function(project, source = NULL, dest = "_publish") {
  current_output = .publish_source_fingerprint(
    project,
    source = source
  )

  if (is.null(current_output)) {
    return(TRUE)
  }

  dest = .validate_publish_dest(dest)
  publish_path = .publish_destination_path(project, dest)
  current_publish = .tree_fingerprint(publish_path)

  if (is.null(current_publish)) {
    return(TRUE)
  }

  state = .read_state(project$path)
  published_build = state$publish$build
  published_dest = state$publish$dest
  published_output = state$publish$output

  is.null(published_build) ||
    is.null(published_dest) ||
    is.null(published_output) ||
    !identical(dest, published_dest) ||
    !identical(current_output, published_build) ||
    !identical(current_publish, published_output)
}
