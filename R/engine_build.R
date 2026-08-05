# Parse command-line selections ---------------------------------------------

.parse_command_line <- function(book = "all", format = "all") {
    books = .normalise_selection(value = book, argument = "book" )
    formats = .normalise_selection(value = format, argument = "format",choices = .supported_formats)

    if (identical(formats, "all")) formats = .supported_formats

    plan = list(current = FALSE, books = books, formats = formats)
    class(plan) = c("iasi_quarto_plan", "list")
    plan
}

.normalise_selection <- function(value, argument, choices = NULL) {
  if (is.null(value)) {
    return("all")
  }

  value <- unique(as.character(value))

  if (
    length(value) == 0L ||
      anyNA(value) ||
      any(!nzchar(value))
  ) {
    stop(
      sprintf(
        "`%s` must contain at least one non-empty value.",
        argument
      ),
      call. = FALSE
    )
  }

  if ("all" %in% value && length(value) > 1L) {
    stop(
      sprintf(
        "`%s = \"all\"` cannot be combined with other values.",
        argument
      ),
      call. = FALSE
    )
  }

  if (!is.null(choices)) {
    invalid_values <- setdiff(value, choices)

    if (length(invalid_values) > 0L) {
      stop(
        sprintf(
          "`%s` contains invalid value%s: %s. Valid values are: %s.",
          argument,
          if (length(invalid_values) == 1L) "" else "s",
          paste(
            sprintf('"%s"', invalid_values),
            collapse = ", "
          ),
          paste(
            sprintf('"%s"', choices),
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }
  }

  value
}

.resolve_current_project = function(plan) {
  if (!file.exists("_quarto.yml")) return(plan)

  warning(
    paste0(
      "A _quarto.yml file was found in the current directory. ",
      "The `book` selection will be ignored."
    ),
    call. = FALSE
  )

  plan$current = TRUE
  plan$books =  normalizePath(getwd(), winslash = "/", mustWork = TRUE)


  plan
}

# Busca los subdirectorios que existen con formato numero-texto
# La entrada puede ser el numero, el texto o todo
# selecciona los que matchean la entrada y los dubdirectorios
#
# Si la entrada es all, no mira nada, los pega todos

.resolve_books = function(plan) {
  if (plan$current) {
    plan$books = getwd()
    return(plan)
  }

  directories = list.dirs(
    path = ".",
    recursive = FALSE,
    full.names = TRUE
  )

  directory_names = basename(directories)

  valid_format = grepl(
    "^\\d+-.+$",
    directory_names
  )

  number_prefix = sub(
    "-.*$",
    "",
    directory_names
  )

  valid = valid_format &
    grepl("[1-9]", number_prefix)

  directories = directories[valid]
  directory_names = directory_names[valid]
  number_prefix = number_prefix[valid]

  book_names = sub(
    "^\\d+-",
    "",
    directory_names
  )

  if (identical(plan$books, "all")) {
    plan$books = directories
    return(plan)
  }

  resolved = character()
  missing = character()

  for (book in plan$books) {
    matches = directory_names == book |
      book_names == book

    if (grepl("^\\d+$", book)) {
      matches = matches |
        as.numeric(number_prefix) == as.numeric(book)
    }

    if (!any(matches)) {
      missing = c(missing, book)
      next
    }

    resolved = c(
      resolved,
      directories[matches]
    )
  }

  if (length(missing) > 0L) {
    warning(
      sprintf(
        "Books not found: %s.",
        paste(
          sprintf('"%s"', missing),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  plan$books = unique(resolved)
  plan
}

.process_book = function(book, formats,change_directory) {
  if (change_directory) {
      previous_directory = setwd(book)
      on.exit(setwd(previous_directory), add = TRUE)
  }

  lapply(formats,.process_format, book=book_name)
}

.process_format = function(format, book) {
   message(sprintf("Rendering '%s' as %s...", book, toupper(format)))

  render(profile = format)
}

.summarise_process = function(plan, results, started_at) {
  elapsed = difftime(Sys.time(), started_at, units = "secs" )

  message("")
  message("Build completed")
  message("---------------")
  message("Books  : ", length(results))
  message("Renders: ", sum(lengths(results)))
  message("Formats: ", paste(plan$formats, collapse = ", "))
  message("Elapsed: ", round(as.numeric(elapsed), 2), " seconds" )

  invisible(results)
}