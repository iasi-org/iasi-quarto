.write_chapter_indexes = function(project) {

  invisible(
    lapply(
      project$chapters,
      .write_chapter_index
    )
  )
}


.write_chapter_index = function(chapter) {

  documents = chapter$documents[
    basename(chapter$documents) != "index.qmd"
  ]

  includes = vapply(
    basename(documents),
    function(document) {
      sprintf(
        "{{< include %s >}}",
        document
      )
    },
    character(1)
  )

  lines = as.vector(
    rbind(
      includes,
      rep("", length(includes))
    )
  )

  if (length(lines) > 0L) {
    lines = lines[-length(lines)]
  }

  writeLines(
    text = lines,
    con = file.path(
      chapter$path,
      "index.qmd"
    ),
    useBytes = TRUE
  )

  invisible(TRUE)
}