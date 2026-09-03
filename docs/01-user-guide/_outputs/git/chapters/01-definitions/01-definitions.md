

## Definiciones y conceptos

En este documento se utilizan los siguientes términos y conceptos. Las definiciones se presentan en orden alfabético para facilitar su consulta.

<table>
<colgroup>
<col style="width: 7%" />
<col style="width: 92%" />
</colgroup>
<thead>
<tr>
<th style="text-align: left;">Término</th>
<th style="text-align: left;">Definición</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align: left;">Build</td>
<td style="text-align: left;">Operación que construye uno o varios formatos de una publicación a partir de sus fuentes. Por defecto, los resultados se escriben en _outputs/; la ubicación puede configurarse. Véase: Formato.</td>
</tr>
<tr>
<td style="text-align: left;">Capítulo</td>
<td style="text-align: left;">Unidad principal de contenido de un libro. Puede estar organizado en secciones.</td>
</tr>
<tr>
<td style="text-align: left;">Direct</td>
<td style="text-align: left;">Estrategia de libro en la que cada documento .qmd del contenido se publica como capítulo y las carpetas se utilizan únicamente para organizar físicamente los archivos. Véase: Estrategia.</td>
</tr>
<tr>
<td style="text-align: left;">Deploy</td>
<td style="text-align: left;">Operación que mantiene una publicación al día: construye cuando es necesario y publica cuando el resultado construido ha cambiado. Puede ejecutarse en modo forzado. Véase: Force y Publish.</td>
</tr>
<tr>
<td style="text-align: left;">Estrategia</td>
<td style="text-align: left;">Regla que determina cómo interpreta IASI Quarto la organización física de un libro. Las estrategias de libro son regular, structured y direct.</td>
</tr>
<tr>
<td style="text-align: left;">Force</td>
<td style="text-align: left;">Modo explícito que ordena ejecutar la operación completa sin consultar si el estado previo indica que es necesaria. No evita las validaciones ni convierte operaciones Git en operaciones forzadas.</td>
</tr>
<tr>
<td style="text-align: left;">Formato</td>
<td style="text-align: left;">Tipo de salida que puede generar una publicación, por ejemplo HTML, PDF o EPUB.</td>
</tr>
<tr>
<td style="text-align: left;">GitBook</td>
<td style="text-align: left;">Formato de publicación de IASI Quarto para generar un libro destinado a GitBook. Se selecciona mediante el formato <code>git</code>. Véase: Formato.</td>
</tr>
<tr>
<td style="text-align: left;">Libro</td>
<td style="text-align: left;">Publicación Quarto de tipo book. IASI Quarto admite las estrategias regular, structured y direct para libros. Véase: Publicación.</td>
</tr>
<tr>
<td style="text-align: left;">Multiproyecto</td>
<td style="text-align: left;">Espacio de trabajo que contiene varias publicaciones IASI Quarto y permite operar sobre todas o seleccionar una de ellas. Véase: Publicación.</td>
</tr>
<tr>
<td style="text-align: left;">Parte</td>
<td style="text-align: left;">División de alto nivel de un libro utilizada para agrupar capítulos relacionados. En la estrategia structured, una carpeta de contenido representa una Parte. Véase: Structured.</td>
</tr>
<tr>
<td style="text-align: left;">Proyecto Quarto</td>
<td style="text-align: left;">Proyecto reconocido por Quarto mediante su configuración habitual, incluida _quarto.yml.</td>
</tr>
<tr>
<td style="text-align: left;">Publicación</td>
<td style="text-align: left;">Proyecto Quarto gestionado por IASI Quarto mediante _iasi.yml. Puede ser un Website o un Libro. Véase: Website.</td>
</tr>
<tr>
<td style="text-align: left;">Publish</td>
<td style="text-align: left;">Operación que toma los resultados ya construidos y crea el árbol final publish/ preparado para desplegarse. No vuelve a renderizar los fuentes.</td>
</tr>
<tr>
<td style="text-align: left;">Quarto</td>
<td style="text-align: left;">Motor de publicación que procesa los documentos .qmd y genera los formatos finales. IASI Quarto organiza y coordina el proyecto, pero Quarto continúa siendo el motor de renderizado.</td>
</tr>
<tr>
<td style="text-align: left;">Regular</td>
<td style="text-align: left;">Estrategia de libro en la que cada carpeta de contenido se convierte en una unidad o capítulo y los documentos .qmd que contiene se combinan en el index.qmd generado de esa carpeta. Véase: Estrategia.</td>
</tr>
<tr>
<td style="text-align: left;">Sección</td>
<td style="text-align: left;">División principal dentro de un capítulo.</td>
</tr>
<tr>
<td style="text-align: left;">Structured</td>
<td style="text-align: left;">Estrategia de libro en la que cada carpeta de contenido representa una Parte, su index.qmd presenta esa Parte y los demás documentos .qmd de la carpeta son Capítulos. Véase: Estrategia.</td>
</tr>
<tr>
<td style="text-align: left;">Website</td>
<td style="text-align: left;">Publicación Quarto de tipo website. En IASI Quarto utiliza la estrategia regular.</td>
</tr>
</tbody>
</table>
