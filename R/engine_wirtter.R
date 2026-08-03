.write_structure = function(
    structure,
    output = "_book-structure.yml"
) {

  yaml::write_yaml(
    x = structure,
    file = output
  )

  invisible(output)
}