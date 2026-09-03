

## Formatos disponibles

IASI Quarto reconoce actualmente los siguientes formatos de construcción:

<table>
<thead>
<tr>
<th style="text-align: left;">Formato</th>
<th style="text-align: left;">Resultado habitual</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">html</td>
<td style="text-align: left;">Publicación HTML</td>
</tr>
<tr>
<td style="text-align: left;">pdf</td>
<td style="text-align: left;">PDF generado por Quarto</td>
</tr>
<tr>
<td style="text-align: left;">typst</td>
<td style="text-align: left;">PDF mediante Typst</td>
</tr>
<tr>
<td style="text-align: left;">epub</td>
<td style="text-align: left;">Libro EPUB</td>
</tr>
<tr>
<td style="text-align: left;">doc</td>
<td style="text-align: left;">Documento ODT presentado como formato DOC de IASI Quarto</td>
</tr>
<tr>
<td style="text-align: left;">odt</td>
<td style="text-align: left;">Documento ODT</td>
</tr>
<tr>
<td style="text-align: left;">git</td>
<td style="text-align: left;">GitBook</td>
</tr>
</tbody>
</table>

Un Website utiliza HTML. Un Book puede utilizar cualquiera de los formatos soportados que tenga configurados.

El formato `git` representa una publicación **GitBook**. `gfm` no es un formato soportado por IASI Quarto.

## Configurar un formato

Cada formato se configura mediante su perfil Quarto correspondiente, por ejemplo:

``` text
_quarto-html.yml
_quarto-pdf.yml
_quarto-epub.yml
```

IASI Quarto no sustituye la configuración propia de Quarto para cada formato.

## Construir un formato

Para construir solo HTML:

``` r
build(format = "html")
```

Para construir solo PDF:

``` r
build(format = "pdf")
```

Puede solicitar varios formatos:

``` r
build(format = c("html", "pdf"))
```

## Construcción predeterminada

Si no indica `format`, IASI Quarto utiliza su conjunto predeterminado:

``` text
html, pdf, epub, doc, git
```

Solo se construyen los formatos que estén disponibles en la publicación mediante sus perfiles correspondientes.
