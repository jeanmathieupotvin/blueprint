# Blueprint

<!-- badges: start -->
![](https://img.shields.io/badge/lifecycle-experimental-orange?style=for-the-badge)
![](https://img.shields.io/badge/version-9002-green?style=for-the-badge)
![](https://img.shields.io/badge/R%20BUILD-passing-green?style=for-the-badge)
![](https://img.shields.io/badge/R%20CMD%20CHECK-passing-green?style=for-the-badge)
![](https://img.shields.io/badge/covr-100%25-green?style=for-the-badge)
<!--
[![R-CMD-check](https://github.com/jeanmathieupotvin/blueprint/workflows/R-CMD-check/badge.svg)](https://github.com/jeanmathieupotvin/blueprint/actions) -->
<!-- badges: end -->

Blueprint provides `R6` classes that generate convenient data schemas. It is
a tidy unopiniotated interface that aims to work with any kind of R objects.

---

*Yo, listen up here's a story*

*About a little guy*

*That lives in a blue world*

*And all day and all night*

*And everything he sees is just blue*

*Like him inside and outside*

\- Wise words from a true 90's classic [(Eiffel 65, *Blue*)](https://www.youtube.com/watch?v=zA52uNzx7Y4).

---

# What are data schemas?

A *schema* is a concise description of how an object (such a `list`, a
`data.frame` or a `vector`) is structured. It dictates what it *should*
contain and how it *should* be structured. Hence, the name of the package:
`blueprint`. Schemas behave a little bit like *promises*: you declare bindings
that will live in the future and provide a thorough description of them. In
other words, they are *future bindings with super-powers*. But blueprints are
more than that, they are also firm handshakes between developers and scientists
that work together.

```r
# Create an Atomic blueprint for a double vector
# of length 1000 to be bound to `rain` later.
bp_rain <- blueprint::Atomic$new(double(), "rain", 1000)$print()

# Alternatively, you could have used the Blueprinter operator.
bp_rain <- rain %bp% double(1000L)

> <Atomic blueprint [0.0.0.9002]>
>   <name:rain type:double length:1000>

# Write a convenient YAML representation of it.
bp_rain$as_yaml(
    file    = "bp_rain.yaml",
    headers = list(author = "JM Potvin", desc = "rain levels (mm) in Montreal"))

# Bind in the current environment.
bp_rain$bind()

# Compare candidates against a blueprint.
bp_rain$compare(integer(10L))  # FALSE
bp_rain$compare(double(1000L)) # TRUE
```

```yaml
# bp_rain.yaml
source: R[v3.6.3]::blueprint[v0.0.0.9002]::Atomic$as_yaml()
author: JM Potvin
desc: rain levels (mm) in Montreal
Atomic:
  name: rain
  type: double
  length: 1000
  prototype: .na.real
```

# Blueprint or blueprint?

Upper cases can be annoying. Grammar can be too. When referring to the package,
use whatever you prefer between `Blueprint` or `blueprint`. Just keep in mind
that `blueprint` is the namespace's true name and that `Blueprint` is its core
super-class.

# Installation

Blueprint is still at an early stage of its development cycle. Best is to not
install and/or use it for now. Developers can explore development versions and
test their features by installing from Github.

```r
# If you want to test the development version here.
if (!require("remotes")) install.packages("remotes")
remotes::install_github("jeanmathieupotvin/blueprint")
```

Source and binary files of specific versions are also [avalaible on Github](https://github.com/jeanmathieupotvin/blueprint/releases).

# Contributions

Blueprint is a personal project. Once it reaches a more mature state, I will
gladly open the development process to everyone! You can definitely send me your
ideas if you are interested in seeing this package grow.

# Questions

Ask me any questions related to the package by sending an email to
<info@blueprint.potvin.xyz>, by opening a
[public discussion](https://github.com/jeanmathieupotvin/blueprint/discussions/new)
or by [opening an issue](https://github.com/jeanmathieupotvin/blueprint/issues/new).
Issues should be reserved for bugs and suggestions (including feedback).
