# .pdf_publish_marker = "IASIPUBLISHMARKER"

.render_html = function(publication, type = publication$type) .render(publication, "html", "html", type)
.render_pdf = function(publication, type = publication$type) .render(publication, "pdf", "pdf", type)
.render_epub = function(publication, type = publication$type) .render(publication, "epub", "epub", type)
.render_odt = function(publication, type = publication$type, profile = "odt") .render(publication, profile, "odt", type)
.render_doc = function(publication, type = publication$type) {
  .render_alias(publication, alias = "doc", format = "odt", type = type, renderer = .render_odt)
}

.render_git = function(publication, type = "default") {
  publication = .render(publication, "git", "commonmark", "default")
  .prepare_gitbook(publication$path)
  publication
}

.prepare_gitbook = function(path) {
  output = file.path(path, "_outputs", "git")
  if (!dir.exists(output)) {
    stop("GitBook renderer did not create '_outputs/git'.", call. = FALSE)
  }

  index = file.path(output, "index.md")
  readme = file.path(output, "README.md")

  if (file.exists(index)) {
    if (!file.copy(index, readme, overwrite = TRUE)) {
      stop("Could not create GitBook README.md from index.md.", call. = FALSE)
    }
  } else if (!file.exists(readme)) {
    stop("GitBook renderer did not create an index page.", call. = FALSE)
  }

  .write_gitbook_summary(path, output)
  .write_gitbook_config(output)

  invisible(TRUE)
}

.write_gitbook_config = function(output) {
  content = c(
    "root: ./",
    "structure:",
    "  readme: README.md",
    "  summary: SUMMARY.md"
  )

  .write_if_changed(content, file.path(output, ".gitbook.yaml"))
  invisible(TRUE)
}

.write_gitbook_summary = function(project_path, output) {
  structure_path = file.path(project_path, "_book-structure.yml")
  if (!file.exists(structure_path)) {
    stop("GitBook requires the generated '_book-structure.yml'.", call. = FALSE)
  }

  structure = .read_yaml_file(structure_path)
  chapters = .yaml_field(.yaml_section(structure, "book"), "chapters")

  if (is.null(chapters) || !length(chapters)) {
    stop("GitBook requires at least one book chapter.", call. = FALSE)
  }

  lines = c("# Summary", "")
  lines = c(lines, .gitbook_summary_entries(chapters, project_path, output, depth = 0L))

  .write_if_changed(lines, file.path(output, "SUMMARY.md"))
  invisible(TRUE)
}

.gitbook_summary_entries = function(entries, project_path, output, depth) {
  lines = character()

  for (entry in entries) {
    if (is.character(entry) && length(entry) == 1L) {
      lines = c(lines, .gitbook_summary_page(entry, project_path, output, depth))
      next
    }

    if (!is.list(entry)) next

    part = .yaml_field(entry, "part")
    children = .yaml_field(entry, "chapters")

    if (is.character(part) && length(part) == 1L) {
      lines = c(lines, .gitbook_summary_page(part, project_path, output, depth))
    }

    if (!is.null(children) && length(children)) {
      lines = c(lines, .gitbook_summary_entries(children, project_path, output, depth + 1L))
    }
  }

  lines
}

.gitbook_summary_page = function(source, project_path, output, depth) {
  target = .gitbook_markdown_path(source)
  rendered = file.path(output, target)
  title = .gitbook_page_title(rendered, file.path(project_path, source))
  indent = paste(rep("    ", depth), collapse = "")

  sprintf("%s* [%s](%s)", indent, title, target)
}

.gitbook_markdown_path = function(path) {
  path = gsub("\\\\", "/", path)
  path = sub("\\.qmd$", ".md", path, ignore.case = TRUE)
  if (identical(tolower(path), "index.md")) "README.md" else path
}

.gitbook_page_title = function(rendered, source) {
  if (file.exists(rendered)) {
    lines = readLines(rendered, warn = FALSE, encoding = "UTF-8")
    heading = grep("^# +[^[:space:]]", lines, value = TRUE)
    if (length(heading)) return(trimws(sub("^# +", "", heading[[1L]])))
  }

  title = .gitbook_source_title(source)
  if (!is.null(title)) return(title)

  tools::file_path_sans_ext(basename(source))
}

.gitbook_source_title = function(path) {
  if (!file.exists(path)) return(NULL)

  lines = readLines(path, warn = FALSE, encoding = "UTF-8")
  if (!length(lines) || trimws(lines[[1L]]) != "---") return(NULL)

  closing = which(trimws(lines[-1L]) %in% c("---", "..."))
  if (!length(closing)) return(NULL)

  end = closing[[1L]] + 1L
  if (end <= 2L) return(NULL)

  front = paste(lines[2L:(end - 1L)], collapse = "\n")
  metadata = tryCatch(yaml::yaml.load(front), error = function(error) NULL)
  if (!is.list(metadata)) return(NULL)

  title = metadata$title
  if (!is.character(title) || length(title) != 1L || is.na(title) || !nzchar(title)) return(NULL)
  title
}


.render_alias = function(publication, alias, format, type, renderer) {
  outputs = file.path(publication$path, "_outputs")
  source = file.path(outputs, format)
  target = file.path(outputs, alias)
  backup = NULL

  if (dir.exists(source)) {
    dir.create(outputs, recursive = TRUE, showWarnings = FALSE)
    backup = tempfile(paste0(".iasi-", format, "-"), tmpdir = outputs)
    if (!file.rename(source, backup)) stop(sprintf("Could not preserve existing output directory '%s'.", source), call. = FALSE)
  }

  restore = function() {
    if (dir.exists(source)) unlink(source, recursive = TRUE, force = TRUE)
    if (!is.null(backup) && dir.exists(backup)) file.rename(backup, source)
  }
  on.exit(restore(), add = TRUE)

  publication = renderer(publication, type = type, profile = alias)
  if (!dir.exists(source)) stop(sprintf("Alias renderer '%s' did not create expected output directory '%s'.", alias, source), call. = FALSE)
  if (dir.exists(target)) unlink(target, recursive = TRUE, force = TRUE)
  if (!file.rename(source, target)) stop(sprintf("Could not rename output directory '%s' to '%s'.", source, target), call. = FALSE)

  publication
}

.render = function(publication, profile, to, type = publication$type) {
  .render_profile(publication$path, profile, to, type)
  publication$rendered = TRUE
  publication$profiles = unique(c(publication$profiles, profile))
  publication
}

.render_profile = function(path, profile, to, type = NULL) {
  previous_directory = setwd(path)
  on.exit(setwd(previous_directory), add = TRUE)

  active_profiles = profile

  if (!is.null(type)) {
    runtime_profile = .create_quarto_type_profile(path, type)
    on.exit(unlink(runtime_profile$file), add = TRUE)
    active_profiles = c(runtime_profile$name, active_profiles)
  }

  # if (identical(profile, "pdf")) {
  #   marker_profile = .create_pdf_publish_marker_profile(path)
  #   on.exit(unlink(marker_profile$file), add = TRUE)
  #   active_profiles = c(active_profiles, marker_profile$name)
  # }

  active_profile = paste(active_profiles, collapse = ",")
  status = system2("quarto", c("render", "--profile", active_profile, "--to", to))
  if (!identical(status, 0L)) stop(sprintf("Quarto rendering failed for profile '%s' with status %s.", profile, status), call. = FALSE)

  invisible(TRUE)
}

.create_quarto_type_profile = function(path, type) {
  file = tempfile("_quarto-iasi-runtime-", tmpdir = path, fileext = ".yml")
  yaml::write_yaml(list(project = list(type = type)), file)
  name = sub("^_quarto-(.*)\\.yml$", "\\1", basename(file))
  list(name = name, file = file)
}

.create_pdf_publish_marker_profile = function(path) {
  file = tempfile("_quarto-iasi-publish-", tmpdir = path, fileext = ".yml")

  writeLines(
    c(
      "format:",
      "  pdf:",
      "    include-in-header:",
      "      - text: |",
      "          \\usepackage{xcolor}",
      "          \\usepackage[footsepline]{scrlayer-scrpage}",
      sprintf(
        "          \\cfoot{{\\footnotesize\\color{white}%s}}",
        .pdf_publish_marker
      )
    ),
    file,
    useBytes = TRUE
  )

  name = sub("^_quarto-(.*)\\.yml$", "\\1", basename(file))
  list(name = name, file = file)
}

