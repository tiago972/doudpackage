
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doudpackage

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/doudpackage)](https://CRAN.R-project.org/package=doudpackage)
<!-- badges: end -->

The goal of doudpackage is to Creates the “table one” of biomedical
papers. Fill it with your data and the name of the variable which you’ll
make the group(s) out of and it will make univariate, bivariate analysis
and parse it into HTML.

## Installation

You can install the development version of doudpackage from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tiago972/doudpackage")
```

## Example

``` r
library(doudpackage)
## basic example code
data(iris)
library(stringi)
iris$fact_1<-as.factor(as.character(sample(1:5, 150, replace = TRUE)))
n_na<-sample(1:150, 30)
iris[n_na, "fact_1"]<-NA
iris$fact_2<-as.factor(as.character(stri_rand_strings(150, 1, '[A-B]')))
iris$num<-runif(150, min = 0, max = 100)
n_na<-sample(1:150, 5)
iris[n_na, "num"]<-NA
iris_test<-descTab(iris, group = "Species", na.print = TRUE)
testParse<-parseClassFun(iris_test, levels_to_keep = list("fact_2" =  "A"),
group_rows_labels = list("Size" = c("Petal.Length", "Petal.Width"),
"My_f" = c("num", "fact_2")))
testParse
```

<table class=" lightable-paper" style="font-family: arial; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="empty-cells: hide;" colspan="1">
</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #00000020; padding-bottom: 5px; ">

setosa

</div>

</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #00000020; padding-bottom: 5px; ">

versicolor

</div>

</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #00000020; padding-bottom: 5px; ">

virginica

</div>

</th>
<th style="padding-bottom:0; padding-left:3px;padding-right:3px;text-align: center; " colspan="1">

<div style="border-bottom: 1px solid #00000020; padding-bottom: 5px; ">

Total

</div>

</th>
<th style="empty-cells: hide;" colspan="1">
</th>
</tr>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
n = 50 (33.3)
</th>
<th style="text-align:left;">
n = 50 (33.3)
</th>
<th style="text-align:left;">
n = 50 (33.3)
</th>
<th style="text-align:left;">
n = 150
</th>
<th style="text-align:left;">
pvalue
</th>
</tr>
</thead>
<tbody>
<tr grouplength="2">
<td colspan="6" style="border-bottom: 1px solid #00000020;">
<strong>Size</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Petal.Length
</td>
<td style="text-align:left;">
1.5 (0.2)
</td>
<td style="text-align:left;">
4.3 (0.5)
</td>
<td style="text-align:left;">
5.6 (0.6)
</td>
<td style="text-align:left;">
3.8 (1.8)
</td>
<td style="text-align:left;">
\< 0.001
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Petal.Width
</td>
<td style="text-align:left;">
0.2 (0.1)
</td>
<td style="text-align:left;">
1.3 (0.2)
</td>
<td style="text-align:left;">
2 (0.3)
</td>
<td style="text-align:left;">
1.2 (0.8)
</td>
<td style="text-align:left;">
\< 0.001
</td>
</tr>
<tr grouplength="3">
<td colspan="6" style="border-bottom: 1px solid #00000020;">
<strong>My_f</strong>
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
fact_2, A
</td>
<td style="text-align:left;">
27 (54)
</td>
<td style="text-align:left;">
25 (50)
</td>
<td style="text-align:left;">
21 (42)
</td>
<td style="text-align:left;">
73 (48.7)
</td>
<td style="text-align:left;">
0.474
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
num
</td>
<td style="text-align:left;">
45.8 (30.3)
</td>
<td style="text-align:left;">
46.7 (27.5)
</td>
<td style="text-align:left;">
47.7 (29.9)
</td>
<td style="text-align:left;">
46.7 (29.1)
</td>
<td style="text-align:left;">
0.948
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 4em;" indentlevel="2">
Missing values
</td>
<td style="text-align:left;">
0 (0)
</td>
<td style="text-align:left;">
4 (8)
</td>
<td style="text-align:left;">
1 (2)
</td>
<td style="text-align:left;">
5 (3)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
fact_1, 1
</td>
<td style="text-align:left;">
9 (18)
</td>
<td style="text-align:left;">
7 (14)
</td>
<td style="text-align:left;">
11 (22)
</td>
<td style="text-align:left;">
27 (18)
</td>
<td style="text-align:left;">
0.502
</td>
</tr>
<tr>
<td style="text-align:left;">
fact_1, 2
</td>
<td style="text-align:left;">
8 (16)
</td>
<td style="text-align:left;">
6 (12)
</td>
<td style="text-align:left;">
6 (12)
</td>
<td style="text-align:left;">
20 (13.3)
</td>
<td style="text-align:left;">
0.502
</td>
</tr>
<tr>
<td style="text-align:left;">
fact_1, 3
</td>
<td style="text-align:left;">
8 (16)
</td>
<td style="text-align:left;">
11 (22)
</td>
<td style="text-align:left;">
7 (14)
</td>
<td style="text-align:left;">
26 (17.3)
</td>
<td style="text-align:left;">
0.502
</td>
</tr>
<tr>
<td style="text-align:left;">
fact_1, 4
</td>
<td style="text-align:left;">
7 (14)
</td>
<td style="text-align:left;">
11 (22)
</td>
<td style="text-align:left;">
10 (20)
</td>
<td style="text-align:left;">
28 (18.7)
</td>
<td style="text-align:left;">
0.502
</td>
</tr>
<tr>
<td style="text-align:left;">
fact_1, 5
</td>
<td style="text-align:left;">
3 (6)
</td>
<td style="text-align:left;">
5 (10)
</td>
<td style="text-align:left;">
11 (22)
</td>
<td style="text-align:left;">
19 (12.7)
</td>
<td style="text-align:left;">
0.502
</td>
</tr>
<tr>
<td style="text-align:left;padding-left: 2em;" indentlevel="1">
Missing values
</td>
<td style="text-align:left;">
15 (30)
</td>
<td style="text-align:left;">
10 (20)
</td>
<td style="text-align:left;">
5 (10)
</td>
<td style="text-align:left;">
30 (20)
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
Sepal.Length
</td>
<td style="text-align:left;">
5 (0.4)
</td>
<td style="text-align:left;">
5.9 (0.5)
</td>
<td style="text-align:left;">
6.6 (0.6)
</td>
<td style="text-align:left;">
5.8 (0.8)
</td>
<td style="text-align:left;">
\< 0.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Sepal.Width
</td>
<td style="text-align:left;">
3.4 (0.4)
</td>
<td style="text-align:left;">
2.8 (0.3)
</td>
<td style="text-align:left;">
3 (0.3)
</td>
<td style="text-align:left;">
3.1 (0.4)
</td>
<td style="text-align:left;">
\< 0.001
</td>
</tr>
</tbody>
</table>
