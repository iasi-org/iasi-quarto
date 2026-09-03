

## Operaciones principales

El uso habitual de `iasi.quarto` gira alrededor de cuatro operaciones:

``` r
validate()
build()
publish()
deploy()
```

### Validar

``` r
validate()
```

Utilice `validate()` para comprobar que la ruta corresponde a una publicación o espacio de trabajo IASI Quarto reconocido.

No construye ni publica.

### Construir

``` r
build()
```

Construye los formatos seleccionados. Por defecto, los resultados se escriben en `_outputs/`; la ubicación puede configurarse.

Durante la edición suele ser cómodo construir únicamente HTML:

``` r
build(format = "html")
```

### Publicar

``` r
publish()
```

Toma los resultados construidos previamente y prepara el árbol final `publish/`. Por defecto, esos resultados se encuentran en `_outputs/`.

No vuelve a renderizar los fuentes.

En una publicación con HTML, el contenido HTML ocupa la raíz de `publish/` y los demás formatos permanecen en sus subdirectorios:

``` text
publish/
├── index.html
├── chapters/
├── pdf/
│   └── libro.pdf
└── epub/
    └── libro.epub
```

### Desplegar

``` r
deploy()
```

`deploy()` mantiene la publicación al día de forma incremental.

Para cada publicación comprueba si necesita construir y, después, si necesita publicar. Si no hay cambios relevantes, no repite trabajo innecesario.

## Forzar una operación

Cuando quiera ejecutar una operación completa aunque el estado indique que no es necesaria, utilice `force = TRUE`:

``` r
deploy(force = TRUE)
```

El modo forzado significa:

``` text
validar
→ ejecutar siempre
```

No elimina las comprobaciones de validez ni ignora errores. Simplemente no utiliza el estado previo para decidir si debe omitir build o publish.

También puede propagarse en:

``` r
build(force = TRUE)
publish(force = TRUE)
```

`build()` y `publish()` ya ejecutan directamente la operación solicitada; el argumento permite mantener una política común cuando son invocados desde otros flujos.
