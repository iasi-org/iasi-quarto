.discover_project <- function(path = ".") {
  project_path <- .normalise_project_path(path)
  quarto_file <- file.path(project_path, "_quarto.yml")

  if (!file.exists(quarto_file)) {
    stop(
      sprintf(
        "No Quarto project found in '%s': missing _quarto.yml.",
        project_path
      ),
      call. = FALSE
    )
  }

  chapters_path <- file.path(project_path, "chapters")
  folders <- if (dir.exists(chapters_path)) {
    .discover_numbered_folders(chapters_path)
  } else {
    list()
  }

  counts <- .count_strategies(folders)

  project <- list(
    path = project_path,
    quarto_file = .slash(quarto_file),
    chapters_path = .slash(chapters_path),
    type = .infer_project_type(counts, folders),
    folders = folders,
    counts = counts,
    root_documents = .discover_root_documents(project_path),
    root_marker = .discover_direct_marker(project_path)
  )

  class(project) <- "iasi_quarto_project"
  project
}

.normalise_project_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  sub("/$", "", path)
}

.discover_numbered_folders <- function(chapters_path) {
  directories <- list.dirs(
    path = chapters_path,
    recursive = FALSE,
    full.names = TRUE
  )

  directories <- directories[grepl("^[0-9]{2}-", basename(directories))]
  directories <- directories[!grepl("^00-", basename(directories))]
  directories <- sort(directories)

  lapply(directories, .discover_folder)
}

.discover_folder <- function(path) {
  documents <- sort(list.files(
    path = path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  direct_marker <- .discover_direct_marker(path)
  index_path <- file.path(path, "index.qmd")
  index_00_path <- file.path(path, "00-index.qmd")

  has_index <- file.exists(index_path)
  has_00_index <- file.exists(index_00_path)
  generated_index <- has_index && .is_generated_index(index_path)

  strategy <- if (!is.null(direct_marker)) {
    "direct"
  } else if (has_index && has_00_index && !generated_index) {
    "incoherent"
  } else if (has_00_index) {
    "regular"
  } else if (has_index) {
    "structured"
  } else {
    "unclassified"
  }

  list(
    name = basename(path),
    path = .slash(path),
    strategy = strategy,
    marker = !is.null(direct_marker),
    marker_path = if (is.null(direct_marker)) NULL else .slash(direct_marker),
    has_index = has_index,
    has_00_index = has_00_index,
    generated_index = generated_index,
    documents = .slash(documents)
  )
}

.discover_direct_marker <- function(path) {
  candidates <- file.path(path, c("index.txt", "00-index.txt"))
  existing <- candidates[file.exists(candidates)]

  if (!length(existing)) {
    return(NULL)
  }

  existing[[1L]]
}

.count_strategies <- function(folders) {
  wanted <- c(
    "structured",
    "regular",
    "direct",
    "unclassified",
    "incoherent"
  )

  counts <- stats::setNames(integer(length(wanted)), wanted)

  if (length(folders)) {
    strategies <- vapply(folders, `[[`, character(1), "strategy")
    counts[] <- as.integer(table(factor(strategies, levels = wanted)))
  }

  as.list(counts)
}

.infer_project_type <- function(counts, folders) {
  if (counts$incoherent > 0L || counts$unclassified > 0L) {
    return("incoherent")
  }

  if (counts$structured > 0L && counts$regular > 0L) {
    return("incoherent")
  }

  if (counts$structured > 0L) {
    return("structured")
  }

  if (counts$regular > 0L) {
    return("regular")
  }

  if (!length(folders) || counts$direct > 0L) {
    return("direct")
  }

  "incoherent"
}

.discover_root_documents <- function(project_path) {
  documents <- sort(list.files(
    path = project_path,
    pattern = "\\.qmd$",
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = TRUE
  ))

  .slash(documents)
}

.is_generated_index <- function(path) {
  if (!file.exists(path)) {
    return(FALSE)
  }

  first <- readLines(
    path,
    n = 1L,
    warn = FALSE,
    encoding = "UTF-8"
  )

  identical(first, .generated_header)
}
