[🇪🇸 Español](README.md) | [🇬🇧 **English**](README.en.md)

# iasi.quarto

> Document-engineering infrastructure on top of Quarto.

`iasi.quarto` is part of the IASI ecosystem. It coordinates Quarto publications without replacing Quarto or hiding its configuration.

Its responsibility is to keep four concerns separate:

```text
sources
→ preparation
→ build
→ publication
```

## What it does

- Discovers individual and multiproject IASI Quarto publications.
- Validates their configuration and structure.
- Materialises derived artifacts such as `_book-structure.yml`.
- Builds configured Quarto profiles.
- Prepares a `_publish/` tree independently from build artifacts.
- Keeps local state so `deploy()` can avoid unnecessary work.

## Public operations

```r
validate()
prepare()
build()
publish()
deploy()
```

`prepare()` materialises derived artifacts without rendering. `build()` produces the configured formats. `publish()` assembles `_publish/` from existing build results without rendering again. `deploy()` incrementally decides what needs to be rebuilt and republished.

## Generated directories

Unless another build location is configured, each publication uses:

```text
publication/
└── _outputs/
    ├── html/
    ├── pdf/
    └── ...
```

`_outputs/` contains regenerable build artifacts.

A standalone publication uses:

```text
publication/
├── _outputs/
└── _publish/
```

A multiproject workspace uses one shared publication root:

```text
workspace/
├── docs/
│   ├── 01-user-guide/
│   │   └── _outputs/
│   └── 02-reference/
│       └── _outputs/
└── _publish/
    ├── user-guide/
    └── reference/
```

Leading numeric prefixes in publication folder names are not carried into `_publish/`.

Release creation, external deployment, and later freezing/distribution steps are intentionally outside `publish()`.

## Installation

```r
remotes::install_github("iasi-org/iasi-quarto")
```

## Example

```r
library(iasi.quarto)

prepare()
build()
publish()
```

During editing, a single format can be built explicitly:

```r
build(format = "html")
```

## Status

`iasi.quarto` is under active development. The public API may evolve as the IASI publication model matures.

## License

MIT License.
