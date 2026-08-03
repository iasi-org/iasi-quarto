.create_structure = function(project, layout) {

  switch(
    layout,

    parts = .create_parts_structure(project),

    chapters = .create_chapters_structure(project)
  )
}


.create_parts_structure = function(project) {

  chapters = lapply(
    project$chapters,
    function(chapter) {

      index = file.path(
        chapter$path,
        "index.qmd"
      )

      if (!file.exists(index)) {
        stop(
          sprintf(
            "Chapter '%s' requires an index.qmd file.",
            chapter$name
          ),
          call. = FALSE
        )
      }

      documents = chapter$documents[
        basename(chapter$documents) != "index.qmd"
      ]

      list(part = index, chapters = documents)
    }
  )

  list(
    book = list(chapters = chapters)
  )
}


.create_chapters_structure = function(project) {

  chapters = vapply(
    project$chapters,
    function(chapter) {
      file.path(
        chapter$path,
        "index.qmd"
      )
    },
    character(1)
  )

  list(
    book = list(
      chapters = chapters
    )
  )
}