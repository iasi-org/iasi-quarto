.export_formats = c(single = "HTML", pdf = "PDF", pdfua = "PDF/UA", epub = "EPUB", docx = "DOCX", odt = "ODT", git = "GitBook")

.validate_publish_dest = function(dest) {
  if (!is.character(dest) ||
      length(dest) != 1L ||
      is.na(dest) ||
      !nzchar(dest)) {
    stop("`dest` must be one non-empty relative directory path.", call. = FALSE)
  }

  dest = gsub("\\\\", "/", dest)

  if (.is_absolute_path(dest) || startsWith(dest, "~")) {
    stop("`dest` must be relative to each Quarto project.", call. = FALSE)
  }

  parts = strsplit(dest, "/", fixed = TRUE)[[1L]]
  parts = parts[nzchar(parts)]

  if (!length(parts) || any(parts %in% c(".", ".."))) {
    stop("`dest` must stay inside each Quarto project.", call. = FALSE)
  }

  if (!startsWith(parts[[1L]], "_") || identical(parts[[1L]], "_")) {
    stop("`dest` must start with an underscore, for example '_publish'.", call. = FALSE)
  }

  do.call(file.path, as.list(parts))
}

.publish_destination_path = function(project, dest) {
  file.path(project$path, .validate_publish_dest(dest))
}

.publish_project = function(project, source = NULL, dest = "_publish") {
  destination = .publish_destination_path(project, dest)
  .publish_project_to(project, source, destination, clean = TRUE)
}

.publish_project_to = function(project, source, destination, clean = TRUE) {
  source_path = .publish_source_path(project, source)
  .check_publish_tree(source_path, destination, project$name)

  if (clean) {
    work_path = paste0(destination, ".work")
    if (dir.exists(work_path)) unlink(work_path, recursive = TRUE, force = TRUE)
    dir.create(work_path, recursive = TRUE, showWarnings = FALSE)
    completed = FALSE
    on.exit(if (!completed && dir.exists(work_path)) unlink(work_path, recursive = TRUE, force = TRUE), add = TRUE)

    .prepare_publish_tree(source_path, work_path, project)
    .replace_publish_tree(work_path, destination)
    completed = TRUE
  } else {
    dir.create(destination, recursive = TRUE, showWarnings = FALSE)
    .prepare_publish_tree(source_path, destination, project)
  }

  project$publish_path = .normalise_project_path(destination)
  project$publish_source = .normalise_project_path(source_path)
  project$publish_outputs = .published_directories(source_path)
  project$published = TRUE
  project
}

.prepare_publish_tree = function(source, destination, project) {
   message("- Preparando árbol de publicación...")
   .copy_directory_contents(source, destination)

   formats = .published_directories(destination)
   publication = .publication_info(project)

   message(sprintf("- Normalizando salida [%s]...", project$strategy))
   .normalise_publish_tree(destination, project, formats)

   message("- Organizando formatos...")
   .move_publish_html_to_root(destination)
   .normalise_publish_exports(destination)
   .sync_export_anchors(destination)

   message("- Aplicando metadatos...")
   .normalise_publication(destination, project, formats, publication)
   .write_publish_timestamp(destination, publication$stamp)
   message("- Publicación preparada.")

   invisible(TRUE)
}

.replace_publish_tree = function(work, destination) {
  if (dir.exists(destination)) {
    unlink(destination, recursive = TRUE, force = TRUE)
  }

  if (!file.rename(work, destination)) {
    stop(sprintf("Could not replace publish directory '%s'.", destination), call. = FALSE)
  }

  invisible(TRUE)
}

.publish_source_path = function(project, source = NULL) {
  .project_output_source(project, source)
}

.check_publish_tree = function(source, destination, project) {
  if (!dir.exists(source)) stop(sprintf("Publish source does not exist for project '%s': %s.", project, source), call. = FALSE)

  source_path = normalizePath(source, winslash = "/", mustWork = TRUE)
  destination_path = normalizePath(destination, winslash = "/", mustWork = FALSE)
  source_key = tolower(source_path)
  destination_key = tolower(destination_path)
  overlaps = identical(source_key, destination_key) || startsWith(paste0(source_key, "/"), paste0(destination_key, "/")) || startsWith(paste0(destination_key, "/"), paste0(source_key, "/"))
  if (overlaps) stop(sprintf("Publish source overlaps destination for project '%s'.", project), call. = FALSE)

  invisible(TRUE)
}

.published_directories = function(path) {
  entries = list.dirs(path, recursive = FALSE, full.names = FALSE)
  entries[nzchar(entries)]
}

.sync_export_anchors = function(publish_path) {
  indexes = list.files(publish_path, pattern = "^index\\.html$", recursive = TRUE, full.names = TRUE)
  if (!length(indexes)) return(invisible(TRUE))

  targets = .discover_export_targets(publish_path)
  lapply(indexes, .sync_export_anchor, publish_path = publish_path, targets = targets)
  invisible(TRUE)
}

.discover_export_targets = function(publish_path) {
  targets = lapply(names(.export_formats), .discover_export_target, publish_path = publish_path)
  names(targets) = names(.export_formats)
  targets[!vapply(targets, is.null, logical(1))]
}

.discover_export_target = function(format, publish_path) {
  path = file.path(publish_path, format)
  if (!dir.exists(path)) return(NULL)

  if (identical(format, "git")) {
    readme = file.path(path, "README.md")
    if (file.exists(readme)) return(readme)
    return(NULL)
  }

  extension = switch(format, single = "html", pdf = "pdf", pdfua = "pdf", epub = "epub", docx = "docx", odt = "odt")
  files = list.files(path, pattern = paste0("\\.", extension, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(NULL)

  files[[1L]]
}

.sync_export_anchor = function(index, publish_path, targets) {
  html = paste(readLines(index, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  anchors = c("<!-- IASI_EXPORT -->", '<div class="iasi-export"></div>', '<div class="iasi-export-anchor"></div>')
  anchor = anchors[vapply(anchors, function(value) grepl(value, html, fixed = TRUE), logical(1))]
  if (!length(anchor)) return(invisible(FALSE))

  block = .export_block(index, targets)
  html = gsub(anchor[[1L]], block, html, fixed = TRUE)
  writeLines(html, index, useBytes = TRUE)
  invisible(TRUE)
}

.export_block = function(index, targets) {
  if (!length(targets)) return("")

  items = Map(function(format, target) {
    label = unname(.export_formats[[format]])
    href = utils::URLencode(.relative_href(dirname(index), target), reserved = FALSE)
    sprintf('      <li><a class="dropdown-item" href="%s">%s</a></li>', href, label)
  }, names(targets), targets)

  paste(c(
    '<div class="dropdown iasi-export">',
    '  <a class="quarto-navigation-tool px-1 dropdown-toggle" href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false" aria-label="Export">Export</a>',
    '  <ul class="dropdown-menu dropdown-menu-end">',
    unlist(items, use.names = FALSE),
    '  </ul>',
    '</div>'
  ), collapse = "\n")
}

.relative_href = function(from, to) {
  from = strsplit(normalizePath(from, winslash = "/", mustWork = TRUE), "/", fixed = TRUE)[[1L]]
  to = strsplit(normalizePath(to, winslash = "/", mustWork = TRUE), "/", fixed = TRUE)[[1L]]
  limit = min(length(from), length(to))
  common = 0L

  while (common < limit && tolower(from[[common + 1L]]) == tolower(to[[common + 1L]])) common = common + 1L
  tail = if (common < length(to)) to[(common + 1L):length(to)] else character()
  parts = c(rep("..", length(from) - common), tail)
  if (!length(parts)) return(".")
  paste(parts, collapse = "/")
}

.move_publish_html_to_root = function(path) {
  html = file.path(path, "html")
  if (!dir.exists(html)) return(invisible(TRUE))

  entries = list.files(html, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  targets = file.path(path, basename(entries))
  conflicts = targets[file.exists(targets) | dir.exists(targets)]
  if (length(conflicts)) stop(sprintf("HTML publication conflicts with another published output: %s.", paste(basename(conflicts), collapse = ", ")), call. = FALSE)

  if (length(entries)) {
    moved = file.rename(entries, targets)
    if (!all(moved)) stop(sprintf("Could not move all HTML publication files to '%s'.", path), call. = FALSE)
  }

  unlink(html, recursive = TRUE, force = TRUE)
  invisible(TRUE)
}

.normalise_publish_exports = function(path) {
  exports = file.path(path, "exports.json")
  if (!file.exists(exports)) return(invisible(TRUE))

  content = readLines(exports, warn = FALSE, encoding = "UTF-8")
  normalised = gsub('"href": "../', '"href": "', content, fixed = TRUE)

  if (!identical(content, normalised)) {
    writeLines(normalised, exports, useBytes = TRUE)
  }

  invisible(TRUE)
}

.copy_directory_contents = function(from, to) {
  entries = list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(entries)) return(invisible(TRUE))

  copied = file.copy(entries, to, recursive = TRUE, overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE)
  if (!all(copied)) stop(sprintf("Could not copy all publication files from '%s' to '%s'.", from, to), call. = FALSE)
  invisible(TRUE)
}

.publication_info = function(project, timestamp = Sys.time()) {
   version = project$version

   if (
      is.null(version) ||
      length(version) != 1L ||
      is.list(version) ||
      is.na(version) ||
      !nzchar(as.character(version))
   ) {
      version = NULL
   } else {
      version = as.character(version)
   }

   stamp = format(
      timestamp,
      tz = "UTC",
      format = "%Y-%m-%dT%H:%M:%OS6Z"
   )

   publish_date = format(timestamp, "%d/%m/%Y")

   text = if (is.null(version)) {
      sprintf("Publicado: %s", publish_date)
   } else {
      sprintf("v%s · Publicado: %s", version, publish_date)
   }

   list(
      timestamp = timestamp,
      stamp = stamp,
      date = publish_date,
      version = version,
      text = text
   )
}

.normalise_publication = function(path, project, formats, publication) {
   for (format in formats) {
      name = paste0(".normalise_publication_", format)

      if (!exists(name, mode = "function")) next

      fn = get(name, mode = "function")
      fn(
         path = path,
         project = project,
         publication = publication
      )
   }

   invisible(TRUE)
}

.normalise_publication_html = function(path, project, publication) {
   files = list.files(
      path,
      pattern = "\\.html$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
   )

   if (!length(files)) return(invisible(TRUE))

   placeholder = '<span id="iasi-publish-date"></span>'
   replacement = sprintf(
      '<span id="iasi-publish-date">%s</span>',
      publication$text
   )

   for (file in files) {
      html = paste(
         readLines(file, warn = FALSE, encoding = "UTF-8"),
         collapse = "\n"
      )

      if (!grepl(placeholder, html, fixed = TRUE)) next

      html = gsub(
         placeholder,
         replacement,
         html,
         fixed = TRUE
      )

      writeLines(html, file, useBytes = TRUE)
   }

   invisible(TRUE)
}

.normalise_publication_pdf = function(path, project, publication) {
   pdf_path = file.path(path, "pdf")

   if (!dir.exists(pdf_path)) return(invisible(TRUE))

   files = list.files(
      pdf_path,
      pattern = "\\.pdf$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
   )

   if (!length(files)) return(invisible(TRUE))

   # .require_pdf_publication_tools()

   # for (file in files) {
   #    .stamp_pdf_publication_info(
   #       file,
   #       publication$text
   #    )
   # }

   invisible(TRUE)
}

.require_pdf_publication_tools = function() {
   required = c("pdftools", "qpdf")
   missing = required[
      !vapply(
         required,
         requireNamespace,
         logical(1),
         quietly = TRUE
      )
   ]

   if (length(missing)) {
      stop(
         sprintf(
            "PDF publication normalisation requires R package%s: %s.",
            if (length(missing) == 1L) "" else "s",
            paste(missing, collapse = ", ")
         ),
         call. = FALSE
      )
   }

   invisible(TRUE)
}

.stamp_pdf_publication_info = function(file, text) {
   markers = .pdf_publication_markers(file)

   if (!nrow(markers)) {
      warning(
         sprintf(
            "PDF publication marker was not found in '%s'. Rebuild the PDF before publishing it.",
            basename(file)
         ),
         call. = FALSE
      )

      return(invisible(FALSE))
   }

   signatures = sprintf(
      "%.2f|%.2f|%.2f|%.2f|%.2f|%.2f",
      markers$page_width,
      markers$page_height,
      markers$x,
      markers$y,
      markers$width,
      markers$height
   )

   groups = split(seq_len(nrow(markers)), signatures)
   current = file
   temporary = character()

   on.exit(
      if (length(temporary)) unlink(temporary, force = TRUE),
      add = TRUE
   )

   for (group in groups) {
      marker = markers[group[[1L]], , drop = FALSE]
      stamp = tempfile("iasi-publish-stamp-", fileext = ".pdf")
      output = tempfile("iasi-publish-pdf-", fileext = ".pdf")
      temporary = c(temporary, stamp, output)

      .create_pdf_publication_stamp(
         stamp,
         page_width = marker$page_width[[1L]],
         page_height = marker$page_height[[1L]],
         marker = marker,
         text = text
      )

      qpdf::pdf_overlay_stamp(
         input = current,
         stamp = stamp,
         output = output,
         pages = markers$page[group]
      )

      current = output
   }

   if (!file.copy(current, file, overwrite = TRUE)) {
      stop(
         sprintf(
            "Could not replace published PDF '%s' after normalisation.",
            file
         ),
         call. = FALSE
      )
   }

   invisible(TRUE)
}

.pdf_publication_markers = function(file) {
   pages = suppressMessages(
      pdftools::pdf_data(file)
   )

   sizes = suppressMessages(
      pdftools::pdf_pagesize(file)
   )

   records = vector("list", length(pages))

   for (page in seq_along(pages)) {
      data = pages[[page]]

      if (!nrow(data) || !"text" %in% names(data)) next

      match = which(data$text == .pdf_publish_marker)
      if (!length(match)) next

      marker = data[match[[1L]], , drop = FALSE]

      records[[page]] = data.frame(
         page = page,
         x = as.numeric(marker$x[[1L]]),
         y = as.numeric(marker$y[[1L]]),
         width = as.numeric(marker$width[[1L]]),
         height = as.numeric(marker$height[[1L]]),
         page_width = as.numeric(sizes$width[[page]]),
         page_height = as.numeric(sizes$height[[page]]),
         stringsAsFactors = FALSE
      )
   }

   records = records[!vapply(records, is.null, logical(1))]

   if (!length(records)) {
      return(
         data.frame(
            page = integer(),
            x = numeric(),
            y = numeric(),
            width = numeric(),
            height = numeric(),
            page_width = numeric(),
            page_height = numeric()
         )
      )
   }

   do.call(rbind, records)
}

.create_pdf_publication_stamp = function(path,
                                          page_width,
                                          page_height,
                                          marker,
                                          text) {
   pointsize = max(
      6,
      min(12, as.numeric(marker$height[[1L]]))
   )

   grDevices::pdf(
      path,
      width = page_width / 72,
      height = page_height / 72,
      onefile = TRUE,
      paper = "special",
      pointsize = pointsize,
      useDingbats = FALSE
   )

   on.exit(grDevices::dev.off(), add = TRUE)

   graphics::par(
      mar = c(0, 0, 0, 0),
      xaxs = "i",
      yaxs = "i"
   )

   graphics::plot.new()
   graphics::plot.window(
      xlim = c(0, page_width),
      ylim = c(0, page_height),
      xaxs = "i",
      yaxs = "i"
   )

   x = as.numeric(marker$x[[1L]]) +
      as.numeric(marker$width[[1L]]) / 2

   y = page_height -
      as.numeric(marker$y[[1L]]) -
      as.numeric(marker$height[[1L]]) / 2

   graphics::text(
      x,
      y,
      labels = text,
      adj = c(0.5, 0.5),
      cex = 1,
      xpd = NA
   )

   invisible(path)
}

.write_publish_timestamp = function(path, stamp = NULL) {
   if (is.null(stamp)) {
      stamp = format(
         Sys.time(),
         tz = "UTC",
         format = "%Y-%m-%dT%H:%M:%OS6Z"
      )
   }

   writeLines(
      as.character(stamp),
      file.path(path, ".publish"),
      useBytes = TRUE
   )

   invisible(TRUE)
}

.report_publish = function(plan) {
  report_root = if (isTRUE(plan$current) && grepl("^[0-9]+-", basename(plan$path))) dirname(plan$path) else plan$path

  message("IASI Quarto publish")
  message("-------------------")
  message(sprintf("Status  : %s", if (isTRUE(plan$published)) "PUBLISHED" else "NOT PUBLISHED"))
  message(sprintf("Projects: %d", length(plan$projects)))

  for (project in plan$projects) {
    message(sprintf("- %s [%s] -> %s", project$name, paste(project$publish_outputs, collapse = ", "), .relative_path(project$publish_path, report_root)))
  }

  invisible(plan)
}

#########################################################################
# Bloque de post proceso
# Por cada formato, ajustamos la salida a nuestros intereses
#########################################################################

.normalise_publish_tree = function(path, project, formats) {
   name = paste0(".normalise_", project$strategy)

   if (!exists(name, mode = "function")) return(invisible(TRUE))

   fn = get(name, mode = "function")
   fn(path = path, project = project, formats = formats)

   invisible(TRUE)
}


.normalise_regular = function(path, project, formats) {
   for (format in formats) {
      name = paste0(".normalise_regular_", format)

      if (!exists(name, mode = "function")) next

      fn = get(name, mode = "function")
      fn(path = path, project = project)
   }

   invisible(TRUE)
}

.normalise_regular_html = function(path, project) {
   html_path = file.path(path, "html")

   if (!dir.exists(html_path)) return(invisible(TRUE))

   files = list.files(
      html_path,
      pattern = "\\.html$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
   )

   for (file in files) {
      document = xml2::read_html(file)
      document = .normalise_regular_html_sidebar(document, file)
      xml2::write_html(document, file)
   }

   invisible(TRUE)
}

.normalise_regular_html_sidebar = function(document, file) {
   chapters = xml2::xml_find_all(
      document,
      "//*[@id='quarto-sidebar']/div[contains(@class,'sidebar-menu-container')]/ul/li"
   )

   for (chapter in chapters) {
      .normalise_regular_html_sidebar_chapter(chapter, file)
   }

   document
}

.normalise_regular_html_sidebar_chapter = function(chapter, file) {
   link = xml2::xml_find_first(
      chapter,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//a[@href] | ./a[@href]"
   )

   if (inherits(link, "xml_missing")) return(chapter)

   href = xml2::xml_attr(link, "href")
   if (is.na(href) || !nzchar(href) || startsWith(href, "#")) return(chapter)

   target_href = sub("[?#].*$", "", href)
   target = file.path(dirname(file), utils::URLdecode(target_href))
   if (!file.exists(target)) return(chapter)

   target_document = xml2::read_html(target)
   sections = xml2::xml_find_first(target_document, "//*[@id='TOC']/ul")
   if (inherits(sections, "xml_missing")) return(chapter)

   links = xml2::xml_find_all(sections, ".//a[@href]")

   for (section_link in links) {
      section_href = xml2::xml_attr(section_link, "href")

      if (!is.na(section_href) && startsWith(section_href, "#")) {
         xml2::xml_attr(section_link, "href") = paste0(target_href, section_href)
      }
   }

   section_id = paste0(
      "iasi-sidebar-",
      gsub("[^A-Za-z0-9_-]+", "-", target_href)
   )

   chapter_class = xml2::xml_attr(chapter, "class")
   if (is.na(chapter_class)) chapter_class = ""

   xml2::xml_attr(chapter, "class") = trimws(
      paste(chapter_class, "sidebar-item-section")
   )

   container = xml2::xml_find_first(
      chapter,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]"
   )

   if (inherits(container, "xml_missing")) return(chapter)

   toggle = xml2::xml_add_child(container, "a")
   xml2::xml_attr(toggle, "class") = "sidebar-item-toggle text-start collapsed"
   xml2::xml_attr(toggle, "data-bs-toggle") = "collapse"
   xml2::xml_attr(toggle, "data-bs-target") = paste0("#", section_id)
   xml2::xml_attr(toggle, "role") = "navigation"
   xml2::xml_attr(toggle, "aria-expanded") = "false"
   xml2::xml_attr(toggle, "aria-controls") = section_id
   xml2::xml_attr(toggle, "aria-label") = "Toggle section"

   icon = xml2::xml_add_child(toggle, "i")
   xml2::xml_attr(icon, "class") = "bi bi-chevron-right ms-2"

   xml2::xml_attr(sections, "id") = section_id
   xml2::xml_attr(sections, "class") = "collapse list-unstyled sidebar-section depth2"

   sections = .normalise_regular_html_sidebar_sections(
      sections,
      prefix = section_id,
      depth = 2L
   )

   xml2::xml_add_child(chapter, sections, .copy = TRUE)

   chapter
}

.normalise_regular_html_sidebar_sections = function(sections, prefix, depth) {
   items = xml2::xml_find_all(sections, "./li")

   for (i in seq_along(items)) {
      item = items[[i]]
      children = xml2::xml_find_first(item, "./ul")

      if (inherits(children, "xml_missing")) next

      id = paste0(prefix, "-", i)

      item_class = xml2::xml_attr(item, "class")
      if (is.na(item_class)) item_class = ""

      xml2::xml_attr(item, "class") = trimws(
         paste(item_class, "sidebar-item-section")
      )

      toggle = xml2::xml_add_child(item, "a")
      xml2::xml_attr(toggle, "class") = "sidebar-item-toggle text-start collapsed"
      xml2::xml_attr(toggle, "data-bs-toggle") = "collapse"
      xml2::xml_attr(toggle, "data-bs-target") = paste0("#", id)
      xml2::xml_attr(toggle, "role") = "navigation"
      xml2::xml_attr(toggle, "aria-expanded") = "false"
      xml2::xml_attr(toggle, "aria-controls") = id
      xml2::xml_attr(toggle, "aria-label") = "Toggle section"

      icon = xml2::xml_add_child(toggle, "i")
      xml2::xml_attr(icon, "class") = "bi bi-chevron-right ms-2"

      xml2::xml_attr(children, "id") = id
      xml2::xml_attr(children, "class") = paste0(
         "collapse list-unstyled sidebar-section depth",
         depth + 1L
      )

      .normalise_regular_html_sidebar_sections(
         children,
         prefix = id,
         depth = depth + 1L
      )
   }

   sections
}

.normalise_structured = function(path, project, formats) {
   for (format in formats) {
      name = paste0(".normalise_structured_", format)

      if (!exists(name, mode = "function")) next

      fn = get(name, mode = "function")
      fn(path = path, project = project)
   }

   invisible(TRUE)
}

.normalise_structured_html = function(path, project) {
   html_path = file.path(path, "html")

   if (!dir.exists(html_path)) return(invisible(TRUE))

   files = list.files(html_path, pattern = "\\.html$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

   for (file in files) {
      document = xml2::read_html(file)

      document = .normalise_structured_html_sidebar(document, project, file)
      document = .normalise_structured_html_content(document, project, file)

      xml2::write_html(document, file)
   }

   invisible(TRUE)
}


.normalise_structured_html_content = function(document, project, file) {
   active = xml2::xml_find_first(
      document,
      "//*[@id='quarto-sidebar']//a[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-link ') and contains(concat(' ', normalize-space(@class), ' '), ' active ')]"
   )

   if (inherits(active, "xml_missing")) return(document)

   node = xml2::xml_find_first(active, "ancestor::li[1]")
   if (inherits(node, "xml_missing")) return(document)

   .normalise_structured_html_content_node(document, node)
}

.normalise_structured_html_content_node = function(document, node) {
   link = xml2::xml_find_first(
      node,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//a[@href] | ./a[@href]"
   )

   if (!inherits(link, "xml_missing")) {
      chapter_number_node = xml2::xml_find_first(
         link,
         ".//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
      )

      section_number_node = xml2::xml_find_first(
         link,
         ".//span[contains(concat(' ', normalize-space(@class), ' '), ' header-section-number ')]"
      )

      if (!inherits(chapter_number_node, "xml_missing")) {
         number = trimws(xml2::xml_text(chapter_number_node))

         target_number_node = xml2::xml_find_first(
            document,
            "//*[@id='title-block-header']//h1[contains(concat(' ', normalize-space(@class), ' '), ' title ')]//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
         )

         if (!inherits(target_number_node, "xml_missing")) {
            xml2::xml_text(target_number_node) = number
         }
      } else if (!inherits(section_number_node, "xml_missing")) {
         href = xml2::xml_attr(link, "href")

         if (!is.na(href) && grepl("#", href, fixed = TRUE)) {
            id = sub("^[^#]*#", "", href)
            id = utils::URLdecode(id)
            number = trimws(xml2::xml_text(section_number_node))

            document = .normalise_structured_html_content_section(
               document = document,
               id = id,
               number = number
            )
         }
      }
   }

   children = xml2::xml_find_first(
      node,
      "./ul[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-section ')]"
   )

   if (!inherits(children, "xml_missing")) {
      items = xml2::xml_find_all(children, "./li")

      for (item in items) {
         document = .normalise_structured_html_content_node(
            document = document,
            node = item
         )
      }
   }

   document
}

.normalise_structured_html_content_section = function(document, id, number) {
   candidates = xml2::xml_find_all(document, "//main//*[@id]")
   if (!length(candidates)) return(document)

   ids = xml2::xml_attr(candidates, "id")
   index = which(ids == id)
   if (!length(index)) return(document)

   section = candidates[[index[[1L]]]]
   heading = xml2::xml_find_first(section, "./h1 | ./h2 | ./h3 | ./h4 | ./h5 | ./h6")
   if (inherits(heading, "xml_missing")) return(document)

   number_node = xml2::xml_find_first(
      heading,
      ".//span[contains(concat(' ', normalize-space(@class), ' '), ' header-section-number ')]"
   )

   if (!inherits(number_node, "xml_missing")) {
      xml2::xml_text(number_node) = number
   }

   document
}

.normalise_structured_html_sidebar = function(document, project, file) {
   document = .normalise_structured_html_sidebar_structure(document, project)

   parts = xml2::xml_find_all(
      document,
      "//*[@id='quarto-sidebar']//li[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-section ')]"
   )

   for (part in parts) {
      chapters = xml2::xml_find_all(
         part,
         "./ul[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-section ')]/li"
      )

      for (chapter in chapters) {
         .normalise_structured_html_sidebar_chapter(chapter, file)
      }
   }

   document
}

.normalise_structured_html_sidebar_structure = function(document, project) {
   root_number = xml2::xml_find_first(
      document,
      "//*[@id='quarto-sidebar']/div[contains(@class,'sidebar-menu-container')]/ul/li[not(contains(@class,'sidebar-item-section'))][1]//span[contains(@class,'chapter-number')]"
   )

   if (!inherits(root_number, "xml_missing")) {
      xml2::xml_remove(root_number)
   }

   parts = xml2::xml_find_all(
      document,
      "//*[@id='quarto-sidebar']//li[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-section ')]"
   )

   for (i in seq_along(parts)) {
      part = parts[[i]]

      title = xml2::xml_find_first(
         part,
         "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//span[contains(concat(' ', normalize-space(@class), ' '), ' menu-text ')]"
      )

      if (!inherits(title, "xml_missing")) {
         xml2::xml_text(title) = paste(as.roman(i), xml2::xml_text(title))
      }

      numbers = xml2::xml_find_all(
         part,
         "./ul[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-section ')]/li//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
      )

      if (length(numbers)) {
         xml2::xml_text(numbers) = as.character(seq_along(numbers))
      }
   }

   document
}

.normalise_structured_html_sidebar_chapter = function(chapter, file) {
   link = xml2::xml_find_first(chapter, ".//a[@href]")
   if (inherits(link, "xml_missing")) return(chapter)

   href = xml2::xml_attr(link, "href")
   if (is.na(href) || !nzchar(href) || startsWith(href, "#")) return(chapter)

   target_href = sub("[?#].*$", "", href)
   target = file.path(dirname(file), utils::URLdecode(target_href))
   if (!file.exists(target)) return(chapter)

   target_document = xml2::read_html(target)
   sections = xml2::xml_find_first(target_document, "//*[@id='TOC']/ul")
   if (inherits(sections, "xml_missing")) return(chapter)

   links = xml2::xml_find_all(sections, ".//a[@href]")

   for (section_link in links) {
      section_href = xml2::xml_attr(section_link, "href")

      if (!is.na(section_href) && startsWith(section_href, "#")) {
         xml2::xml_attr(section_link, "href") = paste0(target_href, section_href)
      }
   }

   chapter_number_node = xml2::xml_find_first(
      chapter,
      ".//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
   )

   if (inherits(chapter_number_node, "xml_missing")) return(chapter)

   chapter_number = xml2::xml_text(chapter_number_node)

   section_id = paste0(
      "iasi-sidebar-",
      gsub("[^A-Za-z0-9_-]+", "-", target_href)
   )

   chapter_class = xml2::xml_attr(chapter, "class")
   if (is.na(chapter_class)) chapter_class = ""

   xml2::xml_attr(chapter, "class") = trimws(
      paste(chapter_class, "sidebar-item-section")
   )

   container = xml2::xml_find_first(
      chapter,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]"
   )

   if (inherits(container, "xml_missing")) return(chapter)

   toggle = xml2::xml_add_child(container, "a")
   xml2::xml_attr(toggle, "class") = "sidebar-item-toggle text-start collapsed"
   xml2::xml_attr(toggle, "data-bs-toggle") = "collapse"
   xml2::xml_attr(toggle, "data-bs-target") = paste0("#", section_id)
   xml2::xml_attr(toggle, "role") = "navigation"
   xml2::xml_attr(toggle, "aria-expanded") = "false"
   xml2::xml_attr(toggle, "aria-controls") = section_id
   xml2::xml_attr(toggle, "aria-label") = "Toggle section"

   icon = xml2::xml_add_child(toggle, "i")
   xml2::xml_attr(icon, "class") = "bi bi-chevron-right ms-2"

   xml2::xml_attr(sections, "id") = section_id
   xml2::xml_attr(sections, "class") = "collapse list-unstyled sidebar-section depth2"

   sections = .normalise_structured_html_sidebar_sections(
      sections,
      prefix = section_id,
      number = chapter_number,
      depth = 2L
   )

   xml2::xml_add_child(chapter, sections, .copy = TRUE)

   chapter
}

.normalise_structured_html_sidebar_sections = function(sections, prefix, number, depth) {
   items = xml2::xml_find_all(sections, "./li")

   for (i in seq_along(items)) {
      item = items[[i]]
      item_number = paste(number, i, sep = ".")

      number_node = xml2::xml_find_first(
         item,
         "./a//span[contains(concat(' ', normalize-space(@class), ' '), ' header-section-number ')]"
      )

      if (!inherits(number_node, "xml_missing")) {
         xml2::xml_text(number_node) = item_number
      }

      children = xml2::xml_find_first(item, "./ul")

      if (inherits(children, "xml_missing")) next

      id = paste0(prefix, "-", i)

      item_class = xml2::xml_attr(item, "class")
      if (is.na(item_class)) item_class = ""

      xml2::xml_attr(item, "class") = trimws(
         paste(item_class, "sidebar-item-section")
      )

      toggle = xml2::xml_add_child(item, "a")
      xml2::xml_attr(toggle, "class") = "sidebar-item-toggle text-start collapsed"
      xml2::xml_attr(toggle, "data-bs-toggle") = "collapse"
      xml2::xml_attr(toggle, "data-bs-target") = paste0("#", id)
      xml2::xml_attr(toggle, "role") = "navigation"
      xml2::xml_attr(toggle, "aria-expanded") = "false"
      xml2::xml_attr(toggle, "aria-controls") = id
      xml2::xml_attr(toggle, "aria-label") = "Toggle section"

      icon = xml2::xml_add_child(toggle, "i")
      xml2::xml_attr(icon, "class") = "bi bi-chevron-right ms-2"

      xml2::xml_attr(children, "id") = id
      xml2::xml_attr(children, "class") = paste0(
         "collapse list-unstyled sidebar-section depth",
         depth + 1L
      )

      children = .normalise_structured_html_sidebar_sections(
         children,
         prefix = id,
         number = item_number,
         depth = depth + 1L
      )
   }

   sections
}
