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

  iasi_file = file.path(
    project_path,
    "iasi.yml"
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

  project = list(
    name = basename(project_path),
    path = project_path,
    quarto_file = .normalise_project_path(quarto_file),
    iasi_file = .normalise_project_path(iasi_file),
    type = type,
    strategy = iasi$strategy,
    content_dir = iasi$content_dir,
    content_path = .content_path(
      project_path,
      iasi$content_dir
    ),
    numbered = iasi$numbered,
    quarto = quarto,
    iasi = iasi_source
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
  publication = .yaml_section(
    source,
    "publication"
  )

  is_book = identical(type, "book")

  list(
    strategy = .value_or_default(
      .yaml_field(
        publication,
        "strategy"
      ),
      "regular"
    ),
    content_dir = .value_or_default(
      .yaml_field(
        publication,
        "content-dir"
      ),
      if (is_book) "chapters" else NULL
    ),
    numbered = .value_or_default(
      .yaml_field(
        publication,
        "numbered"
      ),
      if (is_book) TRUE else NULL
    )
  )
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
