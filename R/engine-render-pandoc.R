.render_docx = function(publication, config) {
  .render_pandoc_output(
    publication = publication,
    config = config,
    extension = "docx",
    options = list(
      to = "docx"
    )
  )
}

.render_odt = function(publication, config) {
  .render_pandoc_output(
    publication = publication,
    config = config,
    extension = "odt",
    options = list(
      to = "odt"
    )
  )
}

.render_single = function(publication, config) {
  if (!identical(publication$type, "book")) {
    stop("Single renderer requires a Quarto book publication.", call. = FALSE)
  }

  .render_pandoc_output(
    publication = publication,
    config = config,
    extension = "html",
    options = list(
      to = "html5",
      standalone = TRUE,
      embed_resources = TRUE,
      title = .single_title(publication$path),
      toc = .single_toc(publication$path)
    )
  )
}

.render_pandoc_output = function(publication,
                                  config,
                                  extension,
                                  options = list()) {
  if (!identical(publication$type, "book")) {
    stop("Pandoc renderer requires a Quarto book publication.", call. = FALSE)
  }

  if (!length(publication$chapters)) {
    stop("Pandoc renderer requires at least one book chapter.", call. = FALSE)
  }

  html_path = config$html$output_path

  if (is.null(html_path) || !dir.exists(html_path)) {
    stop(
      sprintf(
        "Pandoc renderer requires the rendered HTML output directory%s.",
        if (is.null(html_path)) "" else sprintf(" '%s'", html_path)
      ),
      call. = FALSE
    )
  }

  inputs = .pandoc_html_inputs(
    publication,
    html_path
  )

  output_path = config$output_path

  if (is.null(output_path)) {
    output_path = publication$path
  }

  dir.create(
    output_path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output = file.path(
    output_path,
    .profile_output_file(
      publication$path,
      profile = config$profile,
      extension = extension
    )
  )

  filter = .pandoc_main_filter()
  on.exit(unlink(filter, force = TRUE), add = TRUE)

  .pandoc(
    inputs = inputs,
    output = output,
    resource_path = .pandoc_resource_path(inputs, html_path),
    filter = filter,
    options = options
  )

  if (!file.exists(output)) {
    stop(
      sprintf(
        "Pandoc renderer did not create expected output file '%s'.",
        output
      ),
      call. = FALSE
    )
  }

  publication$rendered = TRUE
  publication$profiles = unique(
    c(publication$profiles, config$profile)
  )

  publication
}

.profile_output_file = function(path, profile, extension) {
  quarto = .read_yaml_file(
    file.path(path, "_quarto.yml")
  )

  profile_quarto = .read_yaml_file(
    file.path(path, sprintf("_quarto-%s.yml", profile))
  )

  candidates = list(
    .yaml_field(
      .yaml_section(profile_quarto, "book"),
      "output-file"
    ),
    .yaml_field(
      .yaml_section(quarto, "book"),
      "output-file"
    ),
    basename(path)
  )

  output = basename(path)

  for (candidate in candidates) {
    if (
      is.character(candidate) &&
        length(candidate) == 1L &&
        !is.na(candidate) &&
        nzchar(candidate)
    ) {
      output = candidate
      break
    }
  }

  output = sub(
    sprintf("\\.%s$", extension),
    "",
    output,
    ignore.case = TRUE
  )

  paste0(output, ".", extension)
}

.pandoc_html_inputs = function(publication, html_path) {
  relative = paste0(
    tools::file_path_sans_ext(publication$chapters),
    ".html"
  )

  inputs = file.path(
    html_path,
    relative
  )

  missing = inputs[!file.exists(inputs)]

  if (length(missing)) {
    stop(
      sprintf(
        "Pandoc renderer could not find generated HTML page%s: %s.",
        if (length(missing) == 1L) "" else "s",
        paste(
          sprintf("'%s'", missing),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  normalizePath(
    inputs,
    winslash = "/",
    mustWork = TRUE
  )
}

.pandoc_resource_path = function(inputs, html_path) {
  paths = unique(
    c(
      normalizePath(
        html_path,
        winslash = "/",
        mustWork = TRUE
      ),
      dirname(inputs)
    )
  )

  paste(
    paths,
    collapse = .Platform$path.sep
  )
}

.pandoc_main_filter = function() {
  path = tempfile(
    "iasi-pandoc-",
    fileext = ".lua"
  )

  writeLines(
    c(
      "local function collect_main(blocks, output)",
      "  for _, block in ipairs(blocks) do",
      "    if block.t == 'Div' then",
      "      local role = block.attributes['role']",
      "      if block.identifier == 'quarto-document-content' or role == 'main' then",
      "        output:extend(block.content)",
      "      else",
      "        collect_main(block.content, output)",
      "      end",
      "    end",
      "  end",
      "end",
      "",
      "function Pandoc(doc)",
      "  local content = pandoc.List()",
      "  collect_main(doc.blocks, content)",
      "",
      "  if #content > 0 then",
      "    doc.blocks = content",
      "  end",
      "",
      "  return doc",
      "end"
    ),
    path,
    useBytes = TRUE
  )

  path
}

.pandoc = function(inputs,
                    output,
                    resource_path,
                    filter,
                    options = list()) {
  pandoc = Sys.which("pandoc")

  if (!nzchar(pandoc)) {
    stop(
      "Pandoc renderer requires Pandoc, but 'pandoc' was not found in PATH.",
      call. = FALSE
    )
  }

  args = c(
    vapply(inputs, shQuote, character(1)),
    "--from=html"
  )

  to = options$to
  if (
    is.character(to) &&
      length(to) == 1L &&
      !is.na(to) &&
      nzchar(to)
  ) {
    args = c(args, sprintf("--to=%s", to))
  }

  if (isTRUE(options$standalone)) {
    args = c(args, "--standalone")
  }

  if (isTRUE(options$embed_resources)) {
    args = c(args, "--embed-resources")
  }

  args = c(
    args,
    "--lua-filter",
    shQuote(filter),
    "--resource-path",
    shQuote(resource_path)
  )

  title = options$title
  if (
    is.character(title) &&
      length(title) == 1L &&
      !is.na(title) &&
      nzchar(title)
  ) {
    args = c(
      args,
      "--metadata",
      shQuote(sprintf("pagetitle=%s", title))
    )
  }

  if (isTRUE(options$toc)) {
    args = c(args, "--toc")
  }

  extra = options$args
  if (!is.null(extra)) {
    args = c(args, as.character(extra))
  }

  args = c(
    args,
    "--output",
    shQuote(output)
  )

  status = system2(
    pandoc,
    args
  )

  if (!identical(status, 0L)) {
    stop(
      sprintf(
        "Pandoc rendering failed with status %s.",
        status
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.single_title = function(path) {
  quarto = .read_yaml_file(
    file.path(path, "_quarto.yml")
  )

  title = .yaml_field(
    .yaml_section(quarto, "book"),
    "title"
  )

  if (
    is.character(title) &&
      length(title) == 1L &&
      !is.na(title) &&
      nzchar(title)
  ) {
    return(title)
  }

  basename(path)
}

.single_toc = function(path) {
  profile = .read_yaml_file(
    file.path(path, "_quarto-single.yml")
  )

  format = .yaml_section(profile, "format")
  html = .yaml_section(format, "html")

  isTRUE(
    .yaml_field(html, "toc")
  )
}
