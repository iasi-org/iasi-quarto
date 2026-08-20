[🇪🇸 Español](README.md) | [🇬🇧 **English**](README.en.md)

> **Language note**
>
> `iasi.quarto` is developed primarily in Spanish as part of the **IASI (Engineering Assisted by Intelligent Systems)** ecosystem.
>
> English documentation is provided to make the project accessible to the international community.
> If you need any document that is only available in Spanish, just let us know: **we will translate it for you.**

# iasi.quarto

> **A document engineering framework built on top of Quarto.**

`iasi.quarto` is part of the **IASI (Engineering Assisted by Intelligent Systems)** ecosystem.

It provides a reusable infrastructure for building books, technical documentation and professional publications based on Quarto.

Its goal is simple:

**Separate content from the infrastructure required to publish it.**

---

# Why does it exist?

Quarto is an excellent publishing platform.

However, as documentation projects grow, new engineering challenges emerge:

- Automatic chapter discovery.
- Book structure generation.
- Index generation.
- Shared project conventions.
- Rendering automation.
- Reusable components.

`iasi.quarto` encapsulates that infrastructure so authors can focus on writing instead of maintaining project mechanics.

---

# Philosophy

`iasi.quarto` is not intended to replace Quarto.

Neither is it intended to hide it.

Instead, it builds an engineering layer on top of Quarto, making complex documentation projects easier to develop, maintain and reuse.

We believe that **the best framework is the one that disappears behind the content.**

---

# Features

- Automatic project discovery.
- Automatic book structure generation.
- HTML, PDF, Typst, EPUB, ODT/DOC, and GFM/Git rendering.
- Simple public API.
- Modular architecture based on internal engines.
- Reusable components.
- Automated test coverage.

---

# Installation

```r
remotes::install_github("iasi-org/iasi-quarto")
```

---

# Getting Started

```r
library(iasi.quarto)

render()
```

That's it.

`iasi.quarto` automatically discovers the project structure, generates the required auxiliary files and renders the documentation in every configured format.

You can also render a single format:

```r
render("html")

render("pdf")
```

---

# Project Status

`iasi.quarto` is currently under active development.

The public API may continue to evolve as the IASI ecosystem matures.

---

# The IASI Ecosystem

This project is part of the **IASI** ecosystem.

- 📖 **iasi-book** – Book and conceptual framework.
- ⚙️ **iasi.lua** – Reusable Lua filters and extensions.
- 🎨 **iasi.render** – Rendering-related components.

Learn more at:

https://github.com/iasi-org

---

# License

MIT License.

---

> **Write content. We'll take care of the infrastructure.**