.copy_test_fixture = function(context, name) {
  source = testthat::test_path(
    "fixtures",
    context,
    name
  )

  target = tempfile(
    paste0(
      "iasi-",
      context,
      "-",
      name,
      "-"
    )
  )

  dir.create(
    target,
    recursive = TRUE
  )

  entries = list.files(
    source,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = TRUE,
    no.. = TRUE
  )

  if (!length(entries)) {
    return(
      normalizePath(
        target,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  relative = substring(
    entries,
    nchar(source) + 2L
  )

  directories = dir.exists(entries)

  if (any(directories)) {
    for (directory in file.path(
      target,
      relative[directories]
    )) {
      dir.create(
        directory,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
  }

  files = entries[!directories]

  if (length(files)) {
    destinations = file.path(
      target,
      relative[!directories]
    )

    for (directory in unique(dirname(destinations))) {
      dir.create(
        directory,
        recursive = TRUE,
        showWarnings = FALSE
      )
    }

    copied = file.copy(
      files,
      destinations,
      overwrite = TRUE
    )

    if (!all(copied)) {
      stop(
        "Could not copy test fixture.",
        call. = FALSE
      )
    }
  }

  normalizePath(
    target,
    winslash = "/",
    mustWork = TRUE
  )
}
