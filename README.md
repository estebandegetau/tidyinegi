# tidyinegi

`tidyinegi` is an open-source R package that allows users to
programatically load analysis-ready [INEGI](https://www.inegi.org.mx/)
data sets into R and in other formats (.dta, .xlsx).

Why tidy? While other packages such as
[`importINEGI`](https://github.com/crenteriam/importinegi) and
[`inegiR`](https://github.com/Eflores89/inegiR) provide similar
functionality, `tidyinegi` is built on top of the `tidyverse` and
`tidyselect` packages, which allows for a more consistent and intuitive
user experience.

Why analysis-ready? Frequent users of INEGI data sets ought to know how
painful it is to get raw data from INEGI into a working state, with
variable and value labels. This package aims to bridge that gap,
providing users with a consistent and intuitive way to load INEGI data
sets into R.

## Development

`tidyinegi` is at an early stage of development. Current work is focused
on [ENIGH](https://www.inegi.org.mx/programas/enigh/nc/2022/). No
development version is available just yet.
