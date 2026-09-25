## Submission

This is an update of doudpackage (2.1.0 -> 2.2.0).

It is mostly a bug fix release. The most important change is that a statistical
test which cannot be computed (a constant variable, a factor with an empty
level, an all-NA column...) no longer aborts the whole table: the p value is NA,
a warning names the variable, and the table is still produced.

It also implements `normality = "assess"` and repairs the `anaBiv()` method for
data.frame, both of which previously raised an error for every input. The value
`normality = "manual"` has been removed; it had never done anything but raise an
error.

See NEWS.md for the complete list.

## R CMD check results

Local check with `devtools::check(args = "--as-cran")` on macOS (R 4.6.0):
0 ERRORs, 0 WARNINGs, 0 NOTEs.

## Tests

The package now ships a testthat suite; it passes with no failure and no
warning.

## Reverse dependencies

There are no reverse dependencies.
