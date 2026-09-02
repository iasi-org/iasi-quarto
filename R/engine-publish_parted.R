.normalise_parted = function(path, project, formats) {
   for (format in formats) {
      name = paste0(".normalise_parted_", format)

      if (!exists(name, mode = "function")) next

      fn = get(name, mode = "function")
      fn(path = path, project = project)
   }

   invisible(TRUE)
}

.normalise_parted_html = function(path, project) {
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

      document = .normalise_parted_html_sidebar(
         document = document,
         project = project,
         file = file,
         html_path = html_path
      )

      document = .normalise_parted_html_content(document)

      xml2::write_html(document, file)
   }

   invisible(TRUE)
}

.normalise_parted_html_sidebar = function(document, project, file, html_path) {
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

   folders = project$folders

   for (i in seq_along(folders)) {
      folder = folders[[i]]

      if (i > length(parts)) next

      part = parts[[i]]

      title = xml2::xml_find_first(
         part,
         "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//span[contains(concat(' ', normalize-space(@class), ' '), ' menu-text ')]"
      )

      if (!inherits(title, "xml_missing")) {
         xml2::xml_text(title) = paste(as.roman(i), "-", xml2::xml_text(title))
      }

      .normalise_parted_html_sidebar_part_items(
         part = part,
         folder = folder,
         file = file
      )
   }

   document
}

.normalise_parted_html_sidebar_part_items = function(part,
                                                      folder,
                                                      file) {
   items = xml2::xml_find_all(
      part,
      "./ul[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-section ')]/li"
   )

   if (!length(items)) return(invisible(part))

   cursor = 1L

   if (!is.null(folder$intro) && cursor <= length(items)) {
      .normalise_parted_html_sidebar_page(
         node = items[[cursor]],
         file = file,
         depth = 2L
      )

      cursor = cursor + 1L
   }

   chapter_number = 0L

   for (item in folder$items) {
      if (cursor > length(items)) break

      chapter_number = chapter_number + 1L
      node = items[[cursor]]
      cursor = cursor + 1L

      .normalise_parted_html_sidebar_number(
         node,
         as.character(chapter_number)
      )

      if (identical(item$kind, "document")) {
         .normalise_parted_html_sidebar_page(
            node = node,
            file = file,
            depth = 2L
         )

         next
      }

      chapter = item$chapter
      sections = list()

      for (i in seq_along(chapter$documents)) {
         if (cursor > length(items)) break

         section = items[[cursor]]
         cursor = cursor + 1L

         .normalise_parted_html_sidebar_number(
            section,
            paste(chapter_number, i, sep = ".")
         )

         .normalise_parted_html_sidebar_page(
            node = section,
            file = file,
            depth = 3L
         )

         sections[[length(sections) + 1L]] = section
      }

      if (!length(sections)) {
         .normalise_parted_html_sidebar_page(
            node = node,
            file = file,
            depth = 2L
         )

         next
      }

      .normalise_parted_html_sidebar_group(
         node = node,
         children = sections,
         file = file,
         depth = 2L
      )
   }

   invisible(part)
}

.normalise_parted_html_sidebar_number = function(node, number) {
   number_node = xml2::xml_find_first(
      node,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')] | ./a//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
   )

   if (!inherits(number_node, "xml_missing")) {
      xml2::xml_text(number_node) = number
   }

   invisible(node)
}

.normalise_parted_html_sidebar_group = function(node, children, file, depth) {
   if (!length(children)) return(invisible(node))

   link = xml2::xml_find_first(
      node,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//a[@href] | ./a[@href]"
   )

   if (inherits(link, "xml_missing")) return(invisible(node))

   href = xml2::xml_attr(link, "href")
   if (is.na(href) || !nzchar(href)) return(invisible(node))

   section_id = paste0(
      "iasi-sidebar-",
      gsub("[^A-Za-z0-9_-]+", "-", sub("[?#].*$", "", href))
   )

   node_class = xml2::xml_attr(node, "class")
   if (is.na(node_class)) node_class = ""

   xml2::xml_attr(node, "class") = trimws(
      paste(node_class, "sidebar-item-section")
   )

   container = xml2::xml_find_first(
      node,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]"
   )

   if (inherits(container, "xml_missing")) return(invisible(node))

   .normalise_parted_html_sidebar_toggle(
      container = container,
      section_id = section_id
   )

   list = xml2::xml_add_child(node, "ul")
   xml2::xml_attr(list, "id") = section_id
   xml2::xml_attr(list, "class") = paste0(
      "collapse list-unstyled sidebar-section depth",
      depth
   )

   for (child in children) {
      xml2::xml_add_child(list, child, .copy = TRUE)
      xml2::xml_remove(child)
   }

   invisible(node)
}

.normalise_parted_html_sidebar_page = function(node, file, depth) {
   link = xml2::xml_find_first(
      node,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]//a[@href] | ./a[@href]"
   )

   if (inherits(link, "xml_missing")) return(invisible(node))

   href = xml2::xml_attr(link, "href")
   if (is.na(href) || !nzchar(href) || startsWith(href, "#")) return(invisible(node))

   target_href = sub("[?#].*$", "", href)
   target = file.path(dirname(file), utils::URLdecode(target_href))
   if (!file.exists(target)) return(invisible(node))

   target_document = xml2::read_html(target)
   sections = xml2::xml_find_first(target_document, "//*[@id='TOC']/ul")
   if (inherits(sections, "xml_missing")) return(invisible(node))

   links = xml2::xml_find_all(sections, ".//a[@href]")

   for (section_link in links) {
      section_href = xml2::xml_attr(section_link, "href")

      if (!is.na(section_href) && startsWith(section_href, "#")) {
         xml2::xml_attr(section_link, "href") = paste0(target_href, section_href)
      }
   }

   number_node = xml2::xml_find_first(
      node,
      ".//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
   )

   if (inherits(number_node, "xml_missing")) return(invisible(node))

   number = trimws(xml2::xml_text(number_node))
   if (!nzchar(number)) return(invisible(node))

   section_id = paste0(
      "iasi-sidebar-",
      gsub("[^A-Za-z0-9_-]+", "-", target_href)
   )

   node_class = xml2::xml_attr(node, "class")
   if (is.na(node_class)) node_class = ""

   xml2::xml_attr(node, "class") = trimws(
      paste(node_class, "sidebar-item-section")
   )

   container = xml2::xml_find_first(
      node,
      "./div[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-item-container ')]"
   )

   if (inherits(container, "xml_missing")) return(invisible(node))

   .normalise_parted_html_sidebar_toggle(
      container = container,
      section_id = section_id
   )

   xml2::xml_attr(sections, "id") = section_id
   xml2::xml_attr(sections, "class") = paste0(
      "collapse list-unstyled sidebar-section depth",
      depth
   )

   sections = .normalise_structured_html_sidebar_sections(
      sections,
      prefix = section_id,
      number = number,
      depth = depth
   )

   xml2::xml_add_child(node, sections, .copy = TRUE)

   invisible(node)
}

.normalise_parted_html_sidebar_toggle = function(container, section_id) {
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

   invisible(toggle)
}

.normalise_parted_html_content = function(document) {
   active = xml2::xml_find_first(
      document,
      "//*[@id='quarto-sidebar']//a[contains(concat(' ', normalize-space(@class), ' '), ' sidebar-link ') and contains(concat(' ', normalize-space(@class), ' '), ' active ')]"
   )

   if (inherits(active, "xml_missing")) return(document)

   node = xml2::xml_find_first(active, "ancestor::li[1]")
   if (inherits(node, "xml_missing")) return(document)

   number_node = xml2::xml_find_first(
      active,
      ".//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
   )

   if (!inherits(number_node, "xml_missing")) {
      number = trimws(xml2::xml_text(number_node))

      target_number_node = xml2::xml_find_first(
         document,
         "//*[@id='title-block-header']//h1[contains(concat(' ', normalize-space(@class), ' '), ' title ')]//span[contains(concat(' ', normalize-space(@class), ' '), ' chapter-number ')]"
      )

      if (!inherits(target_number_node, "xml_missing")) {
         xml2::xml_text(target_number_node) = number
      }
   }

   section_links = xml2::xml_find_all(
      node,
      ".//a[@href and .//span[contains(concat(' ', normalize-space(@class), ' '), ' header-section-number ')]]"
   )

   for (link in section_links) {
      href = xml2::xml_attr(link, "href")
      if (is.na(href) || !grepl("#", href, fixed = TRUE)) next

      id = sub("^[^#]*#", "", href)
      id = utils::URLdecode(id)

      section_number_node = xml2::xml_find_first(
         link,
         ".//span[contains(concat(' ', normalize-space(@class), ' '), ' header-section-number ')]"
      )

      if (inherits(section_number_node, "xml_missing")) next

      section_number = trimws(xml2::xml_text(section_number_node))

      document = .normalise_structured_html_content_section(
         document = document,
         id = id,
         number = section_number
      )
   }

   document
}
