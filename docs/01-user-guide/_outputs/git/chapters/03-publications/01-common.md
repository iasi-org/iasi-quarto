

## Estrategias soportadas

IASI Quarto soporta tres estrategias de publicación. La estrategia define cómo se interpreta la organización de los documentos fuente.

<table>
<colgroup>
<col style="width: 4%" />
<col style="width: 6%" />
<col style="width: 45%" />
<col style="width: 42%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">Estrategia</th>
<th style="text-align: left;">Disponible para</th>
<th style="text-align: left;">Organización editorial</th>
<th style="text-align: left;">Cuándo utilizarla</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">regular</td>
<td style="text-align: left;">Website y Book</td>
<td style="text-align: left;">En un Book, cada carpeta de contenido forma una unidad y sus archivos .qmd se combinan.</td>
<td style="text-align: left;">Cuando varios documentos fuente forman una única unidad editorial. Es la estrategia predeterminada.</td>
</tr>
<tr>
<td style="text-align: left;">structured</td>
<td style="text-align: left;">Book</td>
<td style="text-align: left;">Cada carpeta representa una Parte; su index.qmd presenta la Parte y los demás archivos .qmd son Capítulos.</td>
<td style="text-align: left;">Cuando la estructura Parte → Capítulo debe estar representada explícitamente.</td>
</tr>
<tr>
<td style="text-align: left;">direct</td>
<td style="text-align: left;">Book</td>
<td style="text-align: left;">Cada archivo .qmd se publica directamente como Capítulo; las carpetas solo organizan los fuentes.</td>
<td style="text-align: left;">Cuando cada documento ya es una unidad editorial completa.</td>
</tr>
</tbody>
</table>

Para un Website, la estrategia soportada es `regular`. Las estrategias `structured` y `direct` son propias de publicaciones de tipo Book.

## Configuración de una publicación

Una publicación IASI Quarto combina la configuración de Quarto con `_iasi.yml`.

`_quarto.yml` continúa describiendo el proyecto Quarto. `_iasi.yml` indica cómo debe interpretar IASI Quarto su organización.

Una configuración mínima de libro puede ser:

``` yaml
publication:
  strategy: regular
  content-dir: chapters
  numbered: true
```

Los valores predeterminados para un libro son:

``` text
strategy    = regular
content-dir = chapters
numbered    = true
```

`content-dir` indica dónde se encuentra el contenido organizado de la publicación.

Cuando `numbered: true`, los documentos y carpetas de contenido que participan en el orden editorial utilizan prefijos numéricos, por ejemplo:

``` text
chapters/
├── 01-intro/
├── 02-installation/
└── 03-usage/
```
