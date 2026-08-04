[🇪🇸 **Castellano**](README.md) | [🇬🇧 English](README.en.md)

# iasi.quarto

> Framework de ingeniería documental basado en Quarto.

`iasi.quarto` forma parte del ecosistema **IASI (Ingeniería Asistida por Sistemas Inteligentes)** y proporciona una infraestructura reutilizable para la generación de libros, documentación técnica y publicaciones profesionales basadas en Quarto.

Su objetivo es separar el contenido de la infraestructura necesaria para construirlo.

---

# ¿Por qué existe?

Quarto es una excelente plataforma de publicación.

Sin embargo, cuando un proyecto documental crece, aparecen necesidades que trascienden el propio contenido:

- Descubrimiento automático de capítulos.
- Construcción de la estructura del libro.
- Generación de índices.
- Convenciones comunes.
- Automatización del proceso de renderizado.
- Componentes reutilizables.

`iasi.quarto` encapsula esa infraestructura para que el autor pueda concentrarse únicamente en escribir.

---

# Filosofía

`iasi.quarto` no pretende sustituir a Quarto.

Tampoco pretende ocultarlo.

Su objetivo es construir una capa de ingeniería sobre Quarto que permita desarrollar proyectos documentales complejos de forma sencilla, consistente y reutilizable.

Creemos que el mejor framework es aquel que desaparece detrás del contenido.

---

# Características

- Descubrimiento automático de la estructura del proyecto.
- Generación automática de la estructura del libro.
- Renderizado HTML y PDF.
- API pública sencilla.
- Arquitectura modular basada en *engines* internos.
- Componentes reutilizables.
- Cobertura mediante pruebas automatizadas.

---

# Instalación

```r
remotes::install_github("iasi-org/iasi-quarto")
```

---

# Primer uso

```r
library(iasi.quarto)

render()
```

Eso es todo.

`iasi.quarto` descubrirá automáticamente la estructura del proyecto, generará los archivos auxiliares necesarios y construirá la documentación en todos los formatos configurados.

También es posible generar un único formato:

```r
render("html")

render("pdf")
```

---

# Estado del proyecto

`iasi.quarto` se encuentra actualmente en desarrollo activo.

La API pública continuará evolucionando conforme madure el ecosistema IASI.

---

# Ecosistema IASI

Este proyecto forma parte del ecosistema **IASI**.

- 📖 **iasi-book** — Libro y marco conceptual.
- ⚙️ **iasi.lua** — Biblioteca de filtros Lua reutilizables.
- 🎨 **iasi.render** — Componentes relacionados con el proceso de renderizado.

Más información:

https://github.com/iasi-org

---

# Licencia

MIT License.