# anytime: anytime behavior analysis and visualization 

[GitHub](https://github.com/TLDart/nondLib)

**Maintainer:**
    [Alexandre D. Jesus](https://adbjesus.com)

**Contributors:**
    [Alexandre D. Jesus](https://adbjesus.com),
    [Luís Paquete](https://www.uc.pt/go/paquete/),
    [Arnaud Liefooghe](https://sites.google.com/site/arnaudliefooghe),
    [Bilel Derbel](https://sites.google.com/site/bilelderbelpro/).

----------------------------​

This library provides various functions to help study the anytime
behavior of algorithms. In particular, it implements some
`geom_anytime_*` objects to extend `ggplot2`, and some `gganytime_*`
plotting functions that use `ggplot2`. It also implements a scalar
measure of performance for anytime algorithms that was introduced in:

A. D. Jesus, A. Liefooghe, B. Derbel, L. Paquete. Algorithm Selection of
Anytime Algorithms. Proceedings of the 2020 Genetic and Evolutionary
Computation Conference (GECCO 2020), 850-858, 2020.
[DOI](https://doi.org/10.1145/3377930.3390185)

This library was first presented in:

A. D. Jesus, L. Paquete, A. Liefooghe, B. Derbel. Techniques to analyze
the anytime behavior of algorithms for multi-objective optimization.
31st European Conference on Operational Research (EURO 2021).

**Note:** this is an early version of the library, and as such there may
be bugs and/or breaking changes. Once it reaches a more mature state, it
will be submitted to CRAN.

## Usage

To install the library you can use `devtools` for the moment

``` R
install.packages("devtools") # if it is not yet installed
devtools::install_github("adbjesus/anytime")
```

For usage examples, you can check the documentation of the functions.
