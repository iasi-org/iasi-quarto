

## Trabajar con varias publicaciones

Un multiproyecto permite mantener varias publicaciones IASI Quarto bajo una misma raíz.

Ejemplo:

``` text
workspace/
├── 01-user-guide/
│   ├── _quarto.yml
│   └── _iasi.yml
├── 02-developer-guide/
│   ├── _quarto.yml
│   └── _iasi.yml
└── 03-reference/
    ├── _quarto.yml
    └── _iasi.yml
```

Desde la raíz puede operar sobre el conjunto:

``` r
build(path = ".")
publish(path = ".")
deploy(path = ".")
```

## Seleccionar una publicación

Puede seleccionar una publicación con `book`:

``` r
build(book = "user-guide")
```

También puede combinar selección y formato:

``` r
build(
  book = "user-guide",
  format = "html"
)
```

La selección puede utilizar el nombre de la publicación, su directorio completo o su prefijo numérico cuando corresponda.

## Publicación común

Cuando se publica un multiproyecto, IASI Quarto ensambla las publicaciones bajo el `publish/` común del workspace respetando la organización del conjunto.

Utilice un multiproyecto cuando las publicaciones forman parte del mismo espacio de trabajo pero siguen siendo unidades de construcción independientes.
