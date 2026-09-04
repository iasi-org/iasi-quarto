#' Discover IASI Quarto publications
#'
#' Reads the Quarto and IASI configuration of every publication previously
#' recognised by [validate()].
#'
#' Missing IASI configuration keys are completed with their default values.
#' Malformed YAML files produce an error that identifies the affected file.
#' Values that are valid YAML but invalid for IASI are preserved and must be
#' checked by the semantic validation step.
#'
#' @param plan An `iasi_quarto_plan` returned by [validate()].
#'
#' @return Invisibly returns the same `iasi_quarto_plan`, enriched with a
#'   `projects` list containing one `iasi_quarto_project` per publication.
#'
.discover = function(plan) {
  .assert_plan(plan)

  plan$projects = lapply(
    plan$books,
    .discover_project
  )

  .report_discovery(plan)

  invisible(plan)
}

.discover_project = function(path) {
  project_path = .normalise_project_path(path)

  quarto_file = file.path(
    project_path,
    "_quarto.yml"
  )

  iasi_file = .iasi_config_file(
    project_path,
    required = TRUE
  )

  quarto = .read_yaml_file(quarto_file)
  iasi_source = .read_yaml_file(iasi_file)

  quarto_project = .yaml_section(
    quarto,
    "project"
  )

  type = .yaml_field(
    quarto_project,
    "type"
  )

  iasi = .normalise_iasi_config(
    source = iasi_source,
    type = type
  )

  publication = .yaml_section(
    iasi,
    "publication"
  )

  project = list(
    name = basename(project_path),
    path = project_path,
    quarto_file = .normalise_project_path(quarto_file),
    iasi_file = .normalise_project_path(iasi_file),
    type = type,
    strategy = .yaml_field(publication, "strategy"),
    version = .yaml_field(publication, "version"),
    content_dir = .yaml_field(publication, "content-dir"),
    content_path = .content_path(
      project_path,
      .yaml_field(publication, "content-dir")
    ),
    numbered = .yaml_field(publication, "numbered"),
    html_landing_page = .yaml_field(
      .yaml_section(publication, "html"),
      "landing-page"
    ),
    quarto = quarto,
    iasi = iasi,
    iasi_source = iasi_source
  )

  class(project) = c(
    "iasi_quarto_project",
    "list"
  )

  project
}

.read_yaml_file = function(path) {
  tryCatch(
    {
      contents = yaml::read_yaml(path)

      if (is.null(contents)) {
        return(list())
      }

      contents
    },
    error = function(error) {
      stop(
        sprintf(
          "Invalid YAML file '%s': %s",
          path,
          conditionMessage(error)
        ),
        call. = FALSE
      )
    }
  )
}

.normalise_iasi_config = function(source, type) {
  is_book = identical(type, "book")

  defaults = list(
    publication = list(
      strategy = "regular",
      `content-dir` = if (is_book) "chapters" else NULL,
      numbered = if (is_book) TRUE else NULL,
      html = list(
        `landing-page` = FALSE
      )

    )
  )

  .merge_iasi_defaults(source, defaults)
}

.merge_iasi_defaults = function(source, defaults) {
  if (!is.list(source)) {
    return(source)
  }

  result = source

  for (name in names(defaults)) {
    default = defaults[[name]]
    value = result[[name]]

    if (is.null(value)) {
      result[[name]] = default
      next
    }

    if (is.list(default) && is.list(value)) {
      result[[name]] = .merge_iasi_defaults(value, default)
    }
  }

  result
}

.project_format_type = function(project, format) {
  if (
    identical(format, "html") &&
      identical(project$type, "book") &&
      isTRUE(project$html_landing_page)
  ) {
    return("website")
  }

  project$type
}

.yaml_section = function(source, name) {
  value = .yaml_field(
    source,
    name
  )

  if (!is.list(value)) {
    return(list())
  }

  value
}

.yaml_field = function(source, name) {
  if (
    !is.list(source) ||
      is.null(names(source)) ||
      !name %in% names(source)
  ) {
    return(NULL)
  }

  source[[name]]
}

.value_or_default = function(value, default) {
  if (is.null(value)) {
    return(default)
  }

  value
}

.content_path = function(project_path, content_dir) {
  if (
    !is.character(content_dir) ||
      length(content_dir) != 1L ||
      is.na(content_dir) ||
      !nzchar(content_dir)
  ) {
    return(NULL)
  }

  file.path(
    project_path,
    content_dir
  )
}

.report_discovery = function(plan) {
  message("IASI Quarto discovery")
  message("---------------------")
  message(sprintf(
    "Projects: %d",
    length(plan$projects)
  ))

  for (project in plan$projects) {
    message(sprintf(
      "- %s [%s, %s]",
      project$name,
      .display_discovered_value(project$type),
      .display_discovered_value(project$strategy)
    ))
  }

  invisible(plan)
}

.display_discovered_value = function(value) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    return("not declared")
  }

  value = as.character(value)

  if (!nzchar(value)) {
    return("not declared")
  }

  value
}
