.render_single = function(publication, type = publication$type) {
  if (!identical(publication$type, "book")) {
    stop("Single renderer requires a Quarto book publication.", call. = FALSE)
  }

  if (!length(publication$chapters)) {
    stop("Single renderer requires at least one book chapter.", call. = FALSE)
  }

  html_path = .single_profile_output_dir(
    publication$path,
    profile = "html",
    default = "_outputs/html"
  )

  if (!dir.exists(html_path)) {
    stop(
      sprintf(
        "Single renderer requires the HTML output directory '%s'.",
        html_path
      ),
      call. = FALSE
    )
  }

  inputs = .single_html_inputs(
    publication,
    html_path
  )

  output_path = .single_profile_output_dir(
    publication$path,
    profile = "single",
    default = "_outputs/single"
  )

  dir.create(
    output_path,
    recursive = TRUE,
    showWarnings = FALSE
  )

  output = file.path(
    output_path,
    .single_output_file(publication$path)
  )

  pandoc = Sys.which("pandoc")

  if (!nzchar(pandoc)) {
    stop(
      "Single renderer requires Pandoc, but 'pandoc' was not found in PATH.",
      call. = FALSE
    )
  }

  filter = .single_pandoc_filter()
  on.exit(unlink(filter, force = TRUE), add = TRUE)

  .render_single_pandoc(
    pandoc = pandoc,
    inputs = inputs,
    output = output,
    resource_path = .single_resource_path(inputs, html_path),
    filter = filter,
    title = .single_title(publication$path),
    toc = .single_toc(publication$path)
  )

  if (!file.exists(output)) {
    stop(
      sprintf(
        "Single renderer did not create expected output file '%s'.",
        output
      ),
      call. = FALSE
    )
  }

  publication$rendered = TRUE
  publication$profiles = unique(
    c(publication$profiles, "single")
  )

  publication
}

.single_profile_output_dir = function(path, profile, default) {
  profile_path = file.path(
    path,
    sprintf("_quarto-%s.yml", profile)
  )

  profile_quarto = if (file.exists(profile_path)) {
    .read_yaml_file(profile_path)
  } else {
    list()
  }

  output_dir = .yaml_field(
    .yaml_section(profile_quarto, "project"),
    "output-dir"
  )

  if (
    !is.character(output_dir) ||
      length(output_dir) != 1L ||
      is.na(output_dir) ||
      !nzchar(output_dir)
  ) {
    output_dir = default
  }

  if (.is_absolute_path(output_dir)) {
    return(output_dir)
  }

  file.path(path, output_dir)
}

.single_html_inputs = function(publication, html_path) {
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
        "Single renderer could not find generated HTML page%s: %s.",
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

.single_output_file = function(path) {
  quarto = .read_yaml_file(
    file.path(path, "_quarto.yml")
  )

  profile = .read_yaml_file(
    file.path(path, "_quarto-single.yml")
  )

  candidates = list(
    .yaml_field(
      .yaml_section(profile, "book"),
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

  paste0(
    sub("\\.html$", "", output, ignore.case = TRUE),
    ".html"
  )
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

.single_resource_path = function(inputs, html_path) {
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

.single_pandoc_filter = function() {
  path = tempfile(
    "iasi-single-",
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

.render_single_pandoc = function(pandoc,
                                  inputs,
                                  output,
                                  resource_path,
                                  filter,
                                  title,
                                  toc = FALSE) {
  args = c(
    vapply(inputs, shQuote, character(1)),
    "--from=html",
    "--to=html5",
    "--standalone",
    "--embed-resources",
    "--lua-filter",
    shQuote(filter),
    "--resource-path",
    shQuote(resource_path),
    "--metadata",
    shQuote(sprintf("pagetitle=%s", title))
  )

  if (isTRUE(toc)) {
    args = c(args, "--toc")
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
        "Pandoc rendering failed for profile 'single' with status %s.",
        status
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}
