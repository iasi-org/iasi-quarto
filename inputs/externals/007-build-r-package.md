# R package builder

Source: working conversations with Javier G. Grandez.

A second private builder, conceptually `.build_package()`, builds R packages using `devtools`.

The public `build()` function supports these package formats:

- `r`: build source and binary packages.
- `r-source`: build only the source package.
- `r-binary`: build only the binary package.

An R package is identified by the presence of a `DESCRIPTION` file in the supplied `path`.
