[🇪🇸 **Castellano**](README.md) | [🇬🇧 English](README.en.md)

# iasi.quarto

> Infraestructura de ingeniería documental sobre Quarto.

`iasi.quarto` forma parte del ecosistema IASI y coordina publicaciones Quarto sin sustituir a Quarto ni ocultar su configuración.

Su responsabilidad es separar con claridad cuatro cosas distintas:

```text
fuentes
→ preparación
→ construcción
→ publicación
```

## Qué hace

- Descubre publicaciones IASI Quarto individuales o multiproyecto.
- Valida su configuración y estructura.
- Materializa artefactos derivados como `_book-structure.yml`.
- Construye perfiles Quarto en formatos HTML, PDF, EPUB, Single HTML, DOCX/ODT, GitBook y otros perfiles compatibles.
- Prepara un árbol `_publish/` independiente de los artefactos de build.
- Mantiene estado local para evitar trabajo innecesario durante `deploy()`.

## Operaciones públicas

```r
validate()
prepare()
build()
publish()
deploy()
```

`prepare()` materializa artefactos derivados sin renderizar. `build()` produce los formatos configurados. `publish()` toma esos resultados y construye `_publish/` sin volver a renderizar. `deploy()` decide incrementalmente qué necesita reconstruirse y republicarse.

## Directorios generados

Cuando no se configura otra ubicación:

```text
publication/
└── _outputs/
    ├── html/
    ├── pdf/
    └── ...
```

`_outputs/` pertenece a cada publicación y contiene artefactos regenerables.

En una publicación individual:

```text
publication/
├── _outputs/
└── _publish/
```

En un multiproyecto:

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

Solo existe un `_publish/` por workspace seleccionado. Los prefijos numéricos de las carpetas de publicación no forman parte del nombre publicado.

La creación de releases, despliegues externos o congelaciones posteriores queda fuera de `publish()`.

## Instalación

```r
remotes::install_github("iasi-org/iasi-quarto")
```

## Ejemplo

```r
library(iasi.quarto)

prepare()
build()
publish()
```

Durante la edición puede construirse un único formato:

```r
build(format = "html")
```

## Estado

`iasi.quarto` está en desarrollo activo. La API pública puede evolucionar mientras se consolida el modelo de publicación de IASI.

## Licencia

MIT License.
