.export_formats = c(pdf = "PDF", typst = "Typst", epub = "EPUB", doc = "DOC", odt = "ODT", git = "Git", gfm = "GFM")

.publish_project = function(project, source = "_outputs") {
  destination = file.path(project$path, "publish")
  .publish_project_to(project, source, destination, clean = TRUE)
}

.publish_numbered_project = function(plan, source = "_outputs") {
  project = plan$projects[[1L]]
  publish_path = file.path(dirname(plan$path), "publish")
  slug = sub("^[0-9]+-", "", basename(project$path))
  destination = if (identical(.numbered_prefix(project$path), 0L)) publish_path else file.path(publish_path, slug)

  project = .publish_project_to(project, source, destination, clean = TRUE)
  dir.create(publish_path, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(publish_path, ".nojekyll"))
  .write_publish_timestamp(publish_path)

  plan$projects = list(project)
  plan$publish_path = .normalise_project_path(publish_path)
  plan
}

.publish_multiproject = function(plan, source = "_outputs") {
  publish_path = file.path(plan$path, "publish")
  work_path = paste0(publish_path, ".work")
  prefixes = vapply(plan$projects, function(project) .numbered_prefix(project$path), integer(1))
  slugs = vapply(plan$projects, function(project) sub("^[0-9]+-", "", basename(project$path)), character(1))

  if (anyDuplicated(slugs)) stop("Publication names are not unique after removing numeric prefixes.", call. = FALSE)
  if (sum(prefixes == 0L) > 1L) stop("A multiproject repository cannot contain more than one 00-* landing project.", call. = FALSE)

  if (dir.exists(work_path)) unlink(work_path, recursive = TRUE, force = TRUE)
  dir.create(work_path, recursive = TRUE, showWarnings = FALSE)
  completed = FALSE
  on.exit(if (!completed && dir.exists(work_path)) unlink(work_path, recursive = TRUE, force = TRUE), add = TRUE)

  plan$projects = Map(function(project, prefix, slug) {
    destination = if (identical(prefix, 0L)) work_path else file.path(work_path, slug)
    .publish_project_to(project, source, destination, clean = FALSE)
  }, plan$projects, prefixes, slugs)

  file.create(file.path(work_path, ".nojekyll"))
  .write_publish_timestamp(work_path)
  .replace_publish_tree(work_path, publish_path)
  completed = TRUE

  plan$projects = Map(function(project, prefix, slug) {
    project$publish_path = .normalise_project_path(if (identical(prefix, 0L)) publish_path else file.path(publish_path, slug))
    project
  }, plan$projects, prefixes, slugs)

  plan$publish_path = .normalise_project_path(publish_path)
  plan
}

.publish_project_to = function(project, source, destination, clean = TRUE) {
  source_path = .publish_source_path(project$path, source)
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
   .copy_directory_contents(source, destination)
   
   formats = .published_directories(destination)
   .normalise_publish_tree(destination, project, formats)
   
   .move_publish_html_to_root(destination)
   .sync_export_anchors(destination)
   .write_publish_timestamp(destination)
   
   invisible(TRUE)
}

.replace_publish_tree = function(work, destination) {
  .make_publish_public(work)

  if (dir.exists(destination)) {
    .make_publish_public(destination)
    unlink(destination, recursive = TRUE, force = TRUE)
  }

  if (!file.rename(work, destination)) {
    stop(sprintf("Could not replace publish directory '%s'.", destination), call. = FALSE)
  }

  invisible(TRUE)
}

.make_publish_public = function(path) {
  if (!dir.exists(path)) {
    stop(sprintf("Publish directory does not exist: %s.", path), call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    path = normalizePath(path, winslash = "\\", mustWork = TRUE)

    status = system2(
      "icacls.exe",
      args = c(
        shQuote(path),
        "/inheritance:e",
        "/grant",
        shQuote("*S-1-5-32-545:(OI)(CI)M"),
        "/T",
        "/C",
        "/Q"
      ),
      stdout = FALSE,
      stderr = FALSE
    )

    if (!identical(status, 0L)) {
      stop(sprintf("Could not make publish directory public: %s.", path), call. = FALSE)
    }

    return(invisible(TRUE))
  }

  directories = c(path, list.dirs(path, recursive = TRUE, full.names = TRUE))
  files = list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  files = files[!dir.exists(files)]

  Sys.chmod(directories, mode = "0775", use_umask = FALSE)
  if (length(files)) Sys.chmod(files, mode = "0664", use_umask = FALSE)

  invisible(TRUE)
}

.publish_source_path = function(project_path, source) {
  if (!is.character(source) || length(source) != 1L || is.na(source) || !nzchar(source)) stop("`source` must be one non-empty directory path.", call. = FALSE)
  if (.is_absolute_path(source)) return(source)
  file.path(project_path, source)
}

.is_absolute_path = function(path) grepl("^(/|[A-Za-z]:[/\\\\])", path)

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

  extension = switch(format, pdf = "pdf", typst = "pdf", epub = "epub", doc = "odt", odt = "odt", git = "md", gfm = "md")
  files = list.files(path, pattern = paste0("\\.", extension, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(NULL)

  if (format %in% c("git", "gfm")) {
    index = files[tolower(basename(files)) == "index.md"]
    if (length(index)) return(index[[1L]])
  }

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

.copy_directory_contents = function(from, to) {
  entries = list.files(from, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (!length(entries)) return(invisible(TRUE))

  copied = file.copy(entries, to, recursive = TRUE, overwrite = TRUE, copy.mode = FALSE, copy.date = TRUE)
  if (!all(copied)) stop(sprintf("Could not copy all publication files from '%s' to '%s'.", from, to), call. = FALSE)
  invisible(TRUE)
}

.write_publish_timestamp = function(path, project_path = NULL) {
   timestamp = Sys.time()
   stamp = format(timestamp, tz = "UTC", format = "%Y-%m-%dT%H:%M:%OS6Z")
   writeLines(stamp, file.path(path, ".publish"), useBytes = TRUE)
   publish_date = format(as.Date(substr(stamp, 1L, 10L)), "%d/%m/%Y")
   version = NULL
   
   if (!is.null(project_path)) {
      iasi_path = file.path(project_path, "_iasi.yml")
      
      if (file.exists(iasi_path)) {
         iasi = yaml::read_yaml(iasi_path)
         version = iasi$version
         
         if ( is.null(version) ||
             !length(version) ||
             !nzchar(as.character(version))) {
            version = NULL
         }
      }
   }
   
   text = if (is.null(version)) {
      sprintf("Publicado: %s", publish_date)
   } else {
      sprintf("v%s · Publicado: %s", as.character(version), publish_date)
   }
   
   files = list.files(path, pattern = "\\.html$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
   
   placeholder = '<span id="iasi-publish-date"></span>'
   replacement = sprintf('<span id="iasi-publish-date">%s</span>',text)
   
   for (file in files) {
      html = paste(
         readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      
      if (!grepl(placeholder, html, fixed = TRUE)) next
      
      html = gsub(placeholder, replacement, html, fixed = TRUE)
      writeLines(html, file, useBytes = TRUE)
   }
   
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
