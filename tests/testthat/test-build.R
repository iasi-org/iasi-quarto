test_that(".parse_command_line() uses default selections", {
  plan = .parse_command_line()

  expect_equal(plan$books, "all")
  expect_equal(plan$formats, "all")
})

test_that(".parse_command_line() accepts one book and one format", {
  plan = .parse_command_line(
    book = "book-one",
    format = "html"
  )

  expect_equal(plan$books, "book-one")
  expect_equal(plan$formats, "html")
})

test_that(".parse_command_line() accepts multiple books and formats", {
  plan = .parse_command_line(
    book = c("book-one", "book-two"),
    format = c("html", "pdf")
  )

  expect_equal(plan$books, c("book-one", "book-two"))
  expect_equal(plan$formats, c("html", "pdf"))
})

test_that(".parse_command_line() removes duplicates preserving order", {
  plan = .parse_command_line(
    book = c("book-one", "book-two", "book-one"),
    format = c("pdf", "html", "pdf")
  )

  expect_equal(plan$books, c("book-one", "book-two"))
  expect_equal(plan$formats, c("pdf", "html"))
})

test_that(".parse_command_line() accepts all as the only selection", {
  plan = .parse_command_line(
    book = "all",
    format = "all"
  )

  expect_equal(plan$books, "all")
  expect_equal(plan$formats, "all")
})

test_that(".parse_command_line() rejects all combined with another book", {
  expect_error(
    .parse_command_line(
      book = c("all", "book-one")
    ),
    '`book = "all"` cannot be combined with other values.',
    fixed = TRUE
  )
})

test_that(".parse_command_line() rejects all combined with another format", {
  expect_error(
    .parse_command_line(
      format = c("all", "html")
    ),
    '`format = "all"` cannot be combined with other values.',
    fixed = TRUE
  )
})

test_that(".parse_command_line() rejects empty book selections", {
  expect_error(
    .parse_command_line(book = character()),
    "`book` must contain at least one non-empty value.",
    fixed = TRUE
  )

  expect_error(
    .parse_command_line(book = ""),
    "`book` must contain at least one non-empty value.",
    fixed = TRUE
  )

  expect_error(
    .parse_command_line(
      book = c("book-one", "")
    ),
    "`book` must contain at least one non-empty value.",
    fixed = TRUE
  )
})

test_that(".parse_command_line() rejects empty format selections", {
  expect_error(
    .parse_command_line(format = character()),
    "`format` must contain at least one non-empty value.",
    fixed = TRUE
  )

  expect_error(
    .parse_command_line(format = ""),
    "`format` must contain at least one non-empty value.",
    fixed = TRUE
  )

  expect_error(
    .parse_command_line(
      format = c("html", "")
    ),
    "`format` must contain at least one non-empty value.",
    fixed = TRUE
  )
})

test_that(".parse_command_line() rejects unknown formats", {
  expect_error(
    .parse_command_line(format = "epub"),
    paste0(
      '`format` contains invalid value: "epub". ',
      'Valid values are: "all", "html", "pdf".'
    ),
    fixed = TRUE
  )
})

test_that(".parse_command_line() reports all unknown formats", {
  expect_error(
    .parse_command_line(
      format = c("epub", "docx")
    ),
    paste0(
      '`format` contains invalid values: "epub", "docx". ',
      'Valid values are: "all", "html", "pdf".'
    ),
    fixed = TRUE
  )
})

test_that(".parse_command_line() does not restrict book names", {
  plan = .parse_command_line(
    book = "future-publication",
    format = "pdf"
  )

  expect_equal(plan$books, "future-publication")
  expect_equal(plan$formats, "pdf")
})