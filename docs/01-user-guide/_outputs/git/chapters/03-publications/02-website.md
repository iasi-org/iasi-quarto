

## Crear un Website

Un Website utiliza el tipo de proyecto `website` de Quarto y la estrategia `regular` de IASI Quarto.

En `_quarto.yml`:

``` yaml
project:
  type: website
```

En `_iasi.yml` puede utilizar:

``` yaml
publication:
  strategy: regular
```

Para un Website, IASI Quarto no introduce las estrategias editoriales propias de los libros. La estructura y navegación continúan siendo las del proyecto Website de Quarto.

Para construirlo:

``` r
build(format = "html")
```
