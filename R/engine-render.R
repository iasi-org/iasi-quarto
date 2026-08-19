.render_html = function(publication, type = publication$type) .render(publication, "html", "html", type)
.render_pdf = function(publication, type = publication$type) .render(publication, "pdf", "typst", type)
.render_typst = function(publication, type = publication$type) .render(publication, "typst", "typst", type)
.render_epub = function(publication, type = publication$type) .render(publication, "epub", "epub", type)
.render_odt = function(publication, type = publication$type, profile = "odt") .render(publication, profile, "odt", type)
.render_gfm = function(publication, type = publication$type, profile = "gfm") .render(publication, profile, "gfm", type)

.render_doc = function(publication, type = publication$type) {
  .render_alias(publication, alias = "doc", format = "odt", type = type, renderer = .render_odt)
}

.render_git = function(publication, type = publication$type) {
  .render_alias(publication, alias = "git", format = "gfm", type = type, renderer = .render_gfm)
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

  active_profile = profile

  if (!is.null(type)) {
    runtime_profile = .create_quarto_type_profile(path, type)
    on.exit(unlink(runtime_profile$file), add = TRUE)
    active_profile = paste(runtime_profile$name, profile, sep = ",")
  }

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
