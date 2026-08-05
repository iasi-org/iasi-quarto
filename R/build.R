#' Build Quarto publications
#'
#' Builds one or more Quarto publications.
#'
#' `build()` automatically adapts to the current working directory.
#'
#' When executed inside a Quarto project, the current project is built.
#' When executed from the root of a multibook workspace, one or more books
#' can be selected.
#'
#' @param book Book or books to build.
#' @param format Output format or formats.
#'
#' @return
#' A build execution plan.
#'
#' @export
build = function(book = NULL, format = NULL) {

    plan = .parse_command_line(book = book, format = format)
    plan = .resolve_current_project(plan)
    plan = .resolve_books(plan)
    
   plan$results = lapply(plan$books, .process_book,formats = plan$formats, change_directory = !plan$current)  

}