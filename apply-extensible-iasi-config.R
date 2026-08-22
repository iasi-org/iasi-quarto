engine_path = file.path("R", "engine_check.R")
test_path = file.path("tests", "testthat", "test-check.R")

if (!file.exists(engine_path)) {
  stop(sprintf("File not found: %s", engine_path), call. = FALSE)
}

if (!file.exists(test_path)) {
  stop(sprintf("File not found: %s", test_path), call. = FALSE)
}

engine = paste(readLines(engine_path, warn = FALSE), collapse = "\n")
tests = paste(readLines(test_path, warn = FALSE), collapse = "\n")

publication_block = paste(
  c(
    '  if (is.list(publication)) {',
    '    allowed_keys = c(',
    '      "strategy",',
    '      "content-dir",',
    '      "numbered",',
    '      "html"',
    '    )',
    '',
    '    unknown_keys = setdiff(',
    '      names(publication),',
    '      allowed_keys',
    '    )',
    '',
    '    if (length(unknown_keys)) {',
    '      errors = c(',
    '        errors,',
    '        sprintf(',
    '          "Unknown publication keys in _iasi.yml: %s.",',
    '          paste(',
    '            unknown_keys,',
    '            collapse = ", "',
    '          )',
    '        )',
    '      )',
    '    }',
    '  }',
    ''
  ),
  collapse = "\n"
)

html_block = paste(
  c(
    '  if (is.list(html)) {',
    '    allowed_html_keys = "landing-page"',
    '    unknown_html_keys = setdiff(',
    '      names(html),',
    '      allowed_html_keys',
    '    )',
    '',
    '    if (length(unknown_html_keys)) {',
    '      errors = c(',
    '        errors,',
    '        sprintf(',
    '          "Unknown publication.html keys in _iasi.yml: %s.",',
    '          paste(',
    '            unknown_html_keys,',
    '            collapse = ", "',
    '          )',
    '        )',
    '      )',
    '    }',
    '  }',
    ''
  ),
  collapse = "\n"
)

old_test = paste(
  c(
    'test_that("check detects unknown IASI publication keys", {',
    '  root = testthat::test_path(',
    '    "fixtures",',
    '    "check",',
    '    "unknown-key"',
    '  )',
    '',
    '  plan = check(',
    '    discover(',
    '      validate(root)',
    '    )',
    '  )',
    '',
    '  expect_false(plan$valid)',
    '',
    '  expect_match(',
    '    plan$projects[[1L]]$errors,',
    '    "stratgey"',
    '  )',
    '})'
  ),
  collapse = "\n"
)

new_test = paste(
  c(
    'test_that("check tolerates unknown IASI publication keys", {',
    '  root = testthat::test_path(',
    '    "fixtures",',
    '    "check",',
    '    "unknown-key"',
    '  )',
    '',
    '  plan = check(',
    '    discover(',
    '      validate(root)',
    '    )',
    '  )',
    '',
    '  expect_true(plan$valid)',
    '  expect_length(',
    '    plan$projects[[1L]]$errors,',
    '    0L',
    '  )',
    '})',
    '',
    'test_that("check accepts extension publication keys such as version", {',
    '  root = .copy_test_fixture(',
    '    "check",',
    '    "regular-without-index"',
    '  )',
    '',
    '  iasi_file = file.path(root, "_iasi.yml")',
    '  iasi = readLines(iasi_file, warn = FALSE)',
    '',
    '  writeLines(',
    '    c(',
    '      iasi,',
    '      \'  version: "0.1.0"\'',
    '    ),',
    '    iasi_file',
    '  )',
    '',
    '  plan = check(',
    '    discover(',
    '      validate(root)',
    '    )',
    '  )',
    '',
    '  expect_true(plan$valid)',
    '  expect_length(',
    '    plan$projects[[1L]]$errors,',
    '    0L',
    '  )',
    '})'
  ),
  collapse = "\n"
)

missing = character()

if (!grepl(publication_block, engine, fixed = TRUE)) {
  missing = c(missing, "publication unknown-key validation block")
}

if (!grepl(html_block, engine, fixed = TRUE)) {
  missing = c(missing, "publication.html unknown-key validation block")
}

if (!grepl(old_test, tests, fixed = TRUE)) {
  missing = c(missing, "unknown-key test block")
}

if (length(missing)) {
  stop(
    paste0(
      "Expected source blocks were not found:\n- ",
      paste(missing, collapse = "\n- "),
      "\nNo files were changed."
    ),
    call. = FALSE
  )
}

engine = sub(publication_block, "", engine, fixed = TRUE)
engine = sub(html_block, "", engine, fixed = TRUE)
tests = sub(old_test, new_test, tests, fixed = TRUE)

writeLines(strsplit(engine, "\n", fixed = TRUE)[[1L]], engine_path)
writeLines(strsplit(tests, "\n", fixed = TRUE)[[1L]], test_path)

message("Updated:")
message("- ", engine_path)
message("- ", test_path)
message("")
message("Unknown _iasi.yml publication keys are now tolerated.")
message("Known IASI fields continue to be validated.")
message("Run devtools::test() next.")
