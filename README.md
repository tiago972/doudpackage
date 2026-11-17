
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doudpackage

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/doudpackage)](https://CRAN.R-project.org/package=doudpackage)
<!-- badges: end -->

doudpackage builds the “table one” of a bio-medical paper: one row per
variable, one column per sub-group, the right statistical test picked
for you, and an HTML or LaTeX table ready to paste into the manuscript.

You give it a `data.frame` and the name of the grouping variable. It
works out which variables are quantitative and which are qualitative,
describes each of them, compares them across the groups, and renders the
result with [kableExtra](https://CRAN.R-project.org/package=kableExtra).

## Installation

``` r
install.packages("doudpackage")
```

Development version:

``` r
# install.packages("devtools")
devtools::install_github("tiago972/doudpackage")
```

## Getting started

Two functions do the work: `descTab()` computes the table,
`parseClassFun()` renders it.

``` r
library(doudpackage)

data(iris)
iris$treated <- factor(sample(c("no", "yes"), 150, replace = TRUE))
iris$dose <- runif(150, min = 0, max = 100)
iris$dose[sample(1:150, 5)] <- NA

tab <- descTab(iris, group = "Species")
tab["table"]
#>                      var      setosa  versicolor virginica       Total pvalue
#> 1         dose mean (SD) 56.3 (24.6) 58.4 (27.9) 51.5 (31) 55.4 (27.9)  0.464
#> 2 Petal.Length mean (SD)   1.5 (0.2)   4.3 (0.5) 5.6 (0.6)   3.8 (1.8)  0.000
#> 3  Petal.Width mean (SD)   0.2 (0.1)   1.3 (0.2)   2 (0.3)   1.2 (0.8)  0.000
#> 4 Sepal.Length mean (SD)     5 (0.4)   5.9 (0.5) 6.6 (0.6)   5.8 (0.8)  0.000
#> 5  Sepal.Width mean (SD)   3.4 (0.4)   2.8 (0.3)   3 (0.3)   3.1 (0.4)  0.000
#> 6            treated, no     23 (46)     26 (52)   28 (56)   77 (51.3)  0.602
#> 7           treated, yes     27 (54)     24 (48)   22 (44)   73 (48.7)  0.602
```

`descTab()` returns an S4 object; the raw data frame is under
`["table"]` and `parseClassFun()` turns it into the formatted table:

``` r
parseClassFun(tab)
```

## What goes into a row

| Variable | Described as | Compared with |
|----|----|----|
| numeric / integer, normal | mean (SD) | t test (2 groups), ANOVA (3+) |
| numeric / integer, non normal | median (IQR) | Wilcoxon (2 groups), Kruskal-Wallis (3+) |
| factor / ordered factor | n (%) per level | Chi-squared, Fisher when the approximation is unreliable |

Variables of any other type (character, `Date`, logical) are ignored
with a warning. Set `normality` to choose how quantitative variables are
handled:

``` r
descTab(iris, group = "Species", normality = "normal")      # mean (SD) everywhere
descTab(iris, group = "Species", normality = "non normal")  # median (IQR) everywhere
descTab(iris, group = "Species", normality = "assess")      # Shapiro-Wilk, variable by variable
```

## Shaping the output

``` r
tab <- descTab(iris, group = "Species", na.print = TRUE)

parseClassFun(
  tab,
  # only one line for a binary variable, and choose which one
  levels_to_keep = list("treated" = "yes"),
  # regroup rows under labels, in this order
  group_rows_labels = list("Petal" = c("Petal.Length", "Petal.Width"),
                           "Sepal" = c("Sepal.Length", "Sepal.Width")),
  # column order
  col.order = c("Total", "setosa", "versicolor", "virginica"),
  font = "Times New Roman"
)
```

## Weighted populations: IPTW and matching

After inverse probability of treatment weighting (IPTW) or matching,
give the weights to `descTab()`. Every statistic is then computed on the
weighted population, and balance is shown with standardized mean
differences (SMD) instead of p values.

``` r
set.seed(1)
n <- 500
cohort <- data.frame(
  age      = round(rnorm(n, mean = 65, sd = 10)),
  sex      = factor(sample(c("Female", "Male"), n, replace = TRUE)),
  diabetes = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.7, 0.3)))
)
# Older and diabetic patients are treated more often: the groups are unbalanced
p.treat <- plogis(-4 + 0.05 * cohort$age + 0.8 * (cohort$diabetes == "Yes"))
cohort$treated <- factor(ifelse(runif(n) < p.treat, "Treated", "Control"))

# Crude table, with the SMD to show the imbalance
descTab(cohort, group = "treated", smd = TRUE)["table"]
#>             var    Control     Treated       Total pvalue   SMD
#> 1 age mean (SD) 62.9 (9.4) 68.7 (10.2) 65.2 (10.1)  0.000 0.581
#> 2  diabetes, No 229 (76.3)  115 (57.5)  344 (68.8)  0.000 0.409
#> 3 diabetes, Yes  71 (23.7)   85 (42.5)  156 (31.2)  0.000 0.409
#> 4   sex, Female 142 (47.3)     98 (49)    240 (48)  0.715 0.033
#> 5     sex, Male 158 (52.7)    102 (51)    260 (52)  0.715 0.033

# Propensity score and ATE weights
ps <- glm(treated ~ age + sex + diabetes, data = cohort,
          family = binomial)$fitted.values
cohort$iptw <- ifelse(cohort$treated == "Treated", 1 / ps, 1 / (1 - ps))

# Weighted table: the SMD are now close to 0
tab.w <- descTab(cohort, group = "treated", weights = "iptw")
tab.w["table"]
#>             var      Control      Treated        Total   SMD
#> 1 age mean (SD)   64.8 (9.5)  65.1 (10.5)    64.9 (10) 0.028
#> 2  diabetes, No 343.5 (69.6) 343.9 (68.4)   687.4 (69) 0.025
#> 3 diabetes, Yes 150.2 (30.4) 158.6 (31.6)   308.8 (31) 0.025
#> 4   sex, Female 235.6 (47.7) 256.7 (51.1) 492.4 (49.4) 0.067
#> 5     sex, Male 258.1 (52.3) 245.8 (48.9) 503.9 (50.6) 0.067
```

The weights column (`iptw`) is not described. Counts are sums of
weights, also in the header of `parseClassFun(tab.w)`. An SMD below 0.1
is the usual mark of a good balance.

`weights` also takes a numeric vector, or directly a `matchit` (MatchIt)
or `weightit` (WeightIt) object. Give the full data the model was fitted
on, not `match.data()`: unmatched patients have a weight of 0 and are
left out.

``` r
m <- MatchIt::matchit(treated ~ age + sex + diabetes, data = cohort)
descTab(cohort, group = "treated", weights = m)

w <- WeightIt::weightit(treated ~ age + sex + diabetes, data = cohort, estimand = "ATE")
descTab(cohort, group = "treated", weights = w)
```

P values are off by default with weights. `pvalue = TRUE` gives design
based tests from the [survey](https://CRAN.R-project.org/package=survey)
package.

Useful `descTab()` arguments:

| Argument | Effect |
|----|----|
| `group` | the factor defining the columns; omit it for a single Total column |
| `na.print` | add a “Missing values” row under each variable |
| `pvalue` | drop the p value column |
| `weights` | IPTW or matching weights: a column name, a vector, or a `matchit`/`weightit` object |
| `smd` | add a standardized mean difference column (default with `weights`) |
| `quanti`, `quali` | restrict the table to one kind of variable |
| `digits.p`, `digits.qt`, `digits.ql` | rounding for p values, mean/SD, proportions |
| `parallel`, `mc.cores` | compute the analyses with `parallel::mclapply()` |

## Notes on missing and degenerate data

- Rows with a missing value in `group` are dropped, with a warning.
- Unused levels of `group` (typically left behind by a subset) are
  dropped, with a warning, rather than producing an empty column.
- When a test cannot be computed — a constant variable, an empty level,
  too few observations — the p value is `NA` and a warning names the
  variable. The rest of the table is still produced.

## Bugs

Please report them at <https://github.com/tiago972/doudpackage/issues>.
