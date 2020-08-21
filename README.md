RhoVec
======

Work in progress. Note that [semantics.md](semantics.md) may be slightly
outdated compared to the implementation.

This is a formalization of vectors and subsetting in R (see Advanced R, [chapter
3](https://adv-r.hadley.nz/vectors-chap.html) and [chapter
4](https://adv-r.hadley.nz/subsetting.html)), a (subset of)
[dplyr](https://dplyr.tidyverse.org/), plus some related
[tidyverse](https://www.tidyverse.org/packages/) packages, such as
[tibble](https://tibble.tidyverse.org/) and
[magrittr](https://magrittr.tidyverse.org/).


Getting Started
---------------

Install [opam](https://opam.ocaml.org/). Then run the following:

```
opam install ocaml.4.10.0
git clone https://github.com/reactorlabs/RhoVec.git
cd RhoVec
make deps # install dependencies
make
```
