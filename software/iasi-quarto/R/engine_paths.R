.default_output_root = "_outputs"
.publish_dir_name = "_publish"

.normalise_project_path = function(path) {
  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}

.relative_path = function(path, root) {
  path = .normalise_project_path(path)
  root = .normalise_project_path(root)

  if (identical(path, root)) {
    return(".")
  }

  prefix = paste0(
    root,
    "/"
  )

  if (!startsWith(
    tolower(path),
    tolower(prefix)
  )) {
    stop(
      sprintf(
        "Path '%s' is outside root '%s'.",
        path,
        root
      ),
      call. = FALSE
    )
  }

  substring(
    path,
    nchar(prefix) + 1L
  )
}

# Resolve the output directory for one logical format.
#
# Precedence is deliberate:
# 1. profile-specific project.output-dir;
# 2. common project.output-dir plus the profile name;
# 3. the IASI default `_outputs/<profile>`.
#
# The default keeps build artifacts generated, local to the publication, and
# separated by format without requiring repetitive Quarto configuration.
.resolve_output_dir = function(quarto, profile_quarto, profile) {
  output_dir = .yaml_field(
    .yaml_section(profile_quarto, "project"),
    "output-dir"
  )

  if (.valid_output_dir(output_dir)) {
    return(output_dir)
  }

  output_root = .yaml_field(
    .yaml_section(quarto, "project"),
    "output-dir"
  )

  if (.valid_output_dir(output_root)) {
    return(
      file.path(
        output_root,
        profile
      )
    )
  }

  file.path(
    .default_output_root,
    profile
  )
}

# Convert a configured output directory into an absolute filesystem path while
# preserving absolute paths supplied explicitly by the author.
.resolve_output_path = function(project_path, output_dir) {
  if (!.valid_output_dir(output_dir)) {
    return(NULL)
  }

  if (.is_absolute_path(output_dir)) {
    return(output_dir)
  }

  file.path(
    project_path,
    output_dir
  )
}

.profile_config = function(project, profile) {
  profile_path = file.path(
    project$path,
    sprintf(
      "_quarto-%s.yml",
      profile
    )
  )

  profile_quarto = if (file.exists(profile_path)) {
    .read_yaml_file(profile_path)
  } else {
    list()
  }

  output_dir = .resolve_output_dir(
    quarto = project$quarto,
    profile_quarto = profile_quarto,
    profile = profile
  )

  list(
    profile = profile,
    type = .project_format_type(
      project,
      profile
    ),
    quarto = profile_quarto,
    output_dir = output_dir,
    output_path = .resolve_output_path(
      project_path = project$path,
      output_dir = output_dir
    )
  )
}

# Internal build artifacts such as the Pandoc staging tree follow the same
# output policy as public formats.
.build_output_config = function(project, name) {
  output_dir = .resolve_output_dir(
    quarto = project$quarto,
    profile_quarto = list(),
    profile = name
  )

  list(
    name = name,
    output_dir = output_dir,
    output_path = .resolve_output_path(
      project_path = project$path,
      output_dir = output_dir
    )
  )
}

.valid_output_dir = function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.is_absolute_path = function(path) {
  grepl(
    "^(/|[A-Za-z]:[/\\\\])",
    path
  )
}

.project_output_paths = function(project, formats = NULL) {
  if (is.null(formats)) {
    formats = .project_declared_formats(project)
  }

  if (!length(formats)) {
    return(character())
  }

  paths = vapply(
    formats,
    function(format) {
      path = .profile_config(
        project,
        format
      )$output_path

      if (is.null(path)) {
        return(NA_character_)
      }

      normalizePath(
        path,
        winslash = "/",
        mustWork = FALSE
      )
    },
    character(1)
  )

  names(paths) = formats

  paths[
    !is.na(paths) &
      nzchar(paths)
  ]
}

# Publish consumes one build root containing sibling format directories. If an
# author deliberately scatters profile outputs across different parents, the
# source must be supplied explicitly rather than guessed.
.project_output_root = function(project, formats = NULL) {
  paths = .project_output_paths(
    project,
    formats
  )

  if (!length(paths)) {
    return(NULL)
  }

  parents = unique(
    dirname(paths)
  )

  if (length(parents) != 1L) {
    stop(
      paste(
        "Quarto profile outputs do not share one publication root;",
        "use `source` explicitly or configure sibling output directories."
      ),
      call. = FALSE
    )
  }

  parents[[1L]]
}

# Resolve the tree consumed by publish(). Explicit sources are relative to the
# publication unless absolute; otherwise the common build root is inferred.
.project_output_source = function(project, source = NULL) {
  if (!is.null(source)) {
    if (!is.character(source) ||
        length(source) != 1L ||
        is.na(source) ||
        !nzchar(source)) {
      stop(
        "`source` must be one non-empty directory path.",
        call. = FALSE
      )
    }

    if (.is_absolute_path(source)) {
      return(source)
    }

    return(
      file.path(
        project$path,
        source
      )
    )
  }

  root = .project_output_root(project)

  if (is.null(root)) {
    stop(
      "Could not infer a publication output root; pass `source` explicitly.",
      call. = FALSE
    )
  }

  root
}
