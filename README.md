
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doudpackage

<!-- badges: start -->
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
iris$fact_2<-as.factor(as.character(sample(1:2, 150, replace = TRUE)))
n_na<-sample(1:150, 10)
iris[n_na, "fact_2"]<-NA
iris$fact_3<-as.factor(as.character(stri_rand_strings(150, 1, '[A-B]')))
iris$num<-runif(150, min = 0, max = 100)
n_na<-sample(1:150, 5)
iris[n_na, "num"]<-NA
iris_test<-descTab(iris, group = "Species", na.print = TRUE)
iris_test
#> An object of class "parseClass"
#> Slot "table":
#>                            var      setosa  versicolor   virginica     Total
#> 1                    fact_1, 1     10 (20)      5 (10)      9 (18)   24 (16)
#> 2                    fact_1, 2     11 (22)     11 (22)      8 (16)   30 (20)
#> 3                    fact_1, 3      6 (12)      6 (12)      8 (16) 20 (13.3)
#> 4                    fact_1, 4      6 (12)     12 (24)      8 (16) 26 (17.3)
#> 5                    fact_1, 5      8 (16)      5 (10)      7 (14) 20 (13.3)
#> 6        fact_1.Missing values      9 (18)     11 (22)     10 (20)   30 (20)
#> 7                    fact_2, 1     27 (54)     18 (36)     26 (52) 71 (47.3)
#> 8                    fact_2, 2     20 (40)     27 (54)     22 (44)   69 (46)
#> 9        fact_2.Missing values       3 (6)      5 (10)       2 (4)  10 (6.7)
#> 10                   fact_3, A     23 (46)     19 (38)     19 (38) 61 (40.7)
#> 11                   fact_3, B     27 (54)     31 (62)     31 (62) 89 (59.3)
#> 12       fact_3.Missing values       0 (0)       0 (0)       0 (0)     0 (0)
#> 13                         num 50.1 (27.2) 48.1 (26.9) 51.9 (28.3) 50 (27.4)
#> 14          num.Missing values       3 (6)       2 (4)       0 (0)     5 (3)
#> 15                Petal.Length   1.5 (0.2)   4.3 (0.5)   5.6 (0.6) 3.8 (1.8)
#> 16 Petal.Length.Missing values       0 (0)       0 (0)       0 (0)     0 (0)
#> 17                 Petal.Width   0.2 (0.1)   1.3 (0.2)     2 (0.3) 1.2 (0.8)
#> 18  Petal.Width.Missing values       0 (0)       0 (0)       0 (0)     0 (0)
#> 19                Sepal.Length     5 (0.4)   5.9 (0.5)   6.6 (0.6) 5.8 (0.8)
#> 20 Sepal.Length.Missing values       0 (0)       0 (0)       0 (0)     0 (0)
#> 21                 Sepal.Width   3.4 (0.4)   2.8 (0.3)     3 (0.3) 3.1 (0.4)
#> 22  Sepal.Width.Missing values       0 (0)       0 (0)       0 (0)     0 (0)
#>    pvalue
#> 1   0.693
#> 2   0.693
#> 3   0.693
#> 4   0.693
#> 5   0.693
#> 6      NA
#> 7   0.207
#> 8   0.207
#> 9      NA
#> 10  0.643
#> 11  0.643
#> 12     NA
#> 13  0.801
#> 14     NA
#> 15  0.000
#> 16     NA
#> 17  0.000
#> 18     NA
#> 19  0.000
#> 20     NA
#> 21  0.000
#> 22     NA
#> 
#> Slot "group":
#> [1] "Species"
#> 
#> Slot "pvalue":
#> [1] TRUE
#> 
#> Slot "na.print":
#> [1] TRUE
#> 
#> Slot "quanti":
#> [1] TRUE
#> 
#> Slot "quali":
#> [1] TRUE
#> 
#> Slot "var_list":
#> An object of class "listVar"
#> Slot "List":
#> [[1]]
#> An object of class "Var"
#> Slot "name":
#> [1] "fact_1"
#> 
#> Slot "type":
#> [1] "factor"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[2]]
#> An object of class "Var"
#> Slot "name":
#> [1] "fact_2"
#> 
#> Slot "type":
#> [1] "factor"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[3]]
#> An object of class "Var"
#> Slot "name":
#> [1] "fact_3"
#> 
#> Slot "type":
#> [1] "factor"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[4]]
#> An object of class "Var"
#> Slot "name":
#> [1] "num"
#> 
#> Slot "type":
#> [1] "numeric"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[5]]
#> An object of class "Var"
#> Slot "name":
#> [1] "Petal.Length"
#> 
#> Slot "type":
#> [1] "numeric"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[6]]
#> An object of class "Var"
#> Slot "name":
#> [1] "Petal.Width"
#> 
#> Slot "type":
#> [1] "numeric"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[7]]
#> An object of class "Var"
#> Slot "name":
#> [1] "Sepal.Length"
#> 
#> Slot "type":
#> [1] "numeric"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[8]]
#> An object of class "Var"
#> Slot "name":
#> [1] "Sepal.Width"
#> 
#> Slot "type":
#> [1] "numeric"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> [[9]]
#> An object of class "Var"
#> Slot "name":
#> [1] "Species"
#> 
#> Slot "type":
#> [1] "factor"
#> 
#> Slot "normal":
#> [1] TRUE
#> 
#> 
#> 
#> 
#> Slot "data":
#>     Sepal.Length Sepal.Width Petal.Length Petal.Width    Species fact_1 fact_2
#> 1            5.1         3.5          1.4         0.2     setosa   <NA>      1
#> 2            4.9         3.0          1.4         0.2     setosa      2      1
#> 3            4.7         3.2          1.3         0.2     setosa      4      1
#> 4            4.6         3.1          1.5         0.2     setosa      2      1
#> 5            5.0         3.6          1.4         0.2     setosa      5      2
#> 6            5.4         3.9          1.7         0.4     setosa   <NA>      2
#> 7            4.6         3.4          1.4         0.3     setosa   <NA>   <NA>
#> 8            5.0         3.4          1.5         0.2     setosa      1      1
#> 9            4.4         2.9          1.4         0.2     setosa      5      1
#> 10           4.9         3.1          1.5         0.1     setosa      1      2
#> 11           5.4         3.7          1.5         0.2     setosa      1      2
#> 12           4.8         3.4          1.6         0.2     setosa      3      2
#> 13           4.8         3.0          1.4         0.1     setosa      3      2
#> 14           4.3         3.0          1.1         0.1     setosa   <NA>      2
#> 15           5.8         4.0          1.2         0.2     setosa      1      1
#> 16           5.7         4.4          1.5         0.4     setosa      2      2
#> 17           5.4         3.9          1.3         0.4     setosa      3      2
#> 18           5.1         3.5          1.4         0.3     setosa      3      1
#> 19           5.7         3.8          1.7         0.3     setosa      1      1
#> 20           5.1         3.8          1.5         0.3     setosa      4      1
#> 21           5.4         3.4          1.7         0.2     setosa      5      2
#> 22           5.1         3.7          1.5         0.4     setosa      1      1
#> 23           4.6         3.6          1.0         0.2     setosa      1      2
#> 24           5.1         3.3          1.7         0.5     setosa      3      1
#> 25           4.8         3.4          1.9         0.2     setosa   <NA>      1
#> 26           5.0         3.0          1.6         0.2     setosa      2      1
#> 27           5.0         3.4          1.6         0.4     setosa      5      1
#> 28           5.2         3.5          1.5         0.2     setosa      2      1
#> 29           5.2         3.4          1.4         0.2     setosa      5      1
#> 30           4.7         3.2          1.6         0.2     setosa      2      1
#> 31           4.8         3.1          1.6         0.2     setosa      4      1
#> 32           5.4         3.4          1.5         0.4     setosa      4      1
#> 33           5.2         4.1          1.5         0.1     setosa      2      2
#> 34           5.5         4.2          1.4         0.2     setosa   <NA>      2
#> 35           4.9         3.1          1.5         0.2     setosa      3   <NA>
#> 36           5.0         3.2          1.2         0.2     setosa   <NA>      1
#> 37           5.5         3.5          1.3         0.2     setosa      5      1
#> 38           4.9         3.6          1.4         0.1     setosa      1      2
#> 39           4.4         3.0          1.3         0.2     setosa      2   <NA>
#> 40           5.1         3.4          1.5         0.2     setosa   <NA>      2
#> 41           5.0         3.5          1.3         0.3     setosa      5      2
#> 42           4.5         2.3          1.3         0.3     setosa      5      2
#> 43           4.4         3.2          1.3         0.2     setosa   <NA>      1
#> 44           5.0         3.5          1.6         0.6     setosa      1      1
#> 45           5.1         3.8          1.9         0.4     setosa      4      2
#> 46           4.8         3.0          1.4         0.3     setosa      2      2
#> 47           5.1         3.8          1.6         0.2     setosa      2      2
#> 48           4.6         3.2          1.4         0.2     setosa      4      1
#> 49           5.3         3.7          1.5         0.2     setosa      1      1
#> 50           5.0         3.3          1.4         0.2     setosa      2      1
#> 51           7.0         3.2          4.7         1.4 versicolor   <NA>      2
#> 52           6.4         3.2          4.5         1.5 versicolor      4      2
#> 53           6.9         3.1          4.9         1.5 versicolor      1      2
#> 54           5.5         2.3          4.0         1.3 versicolor   <NA>      1
#> 55           6.5         2.8          4.6         1.5 versicolor      4      1
#> 56           5.7         2.8          4.5         1.3 versicolor   <NA>      2
#> 57           6.3         3.3          4.7         1.6 versicolor   <NA>      1
#> 58           4.9         2.4          3.3         1.0 versicolor   <NA>      1
#> 59           6.6         2.9          4.6         1.3 versicolor      2      1
#> 60           5.2         2.7          3.9         1.4 versicolor      1      2
#> 61           5.0         2.0          3.5         1.0 versicolor      4      2
#> 62           5.9         3.0          4.2         1.5 versicolor      2      2
#> 63           6.0         2.2          4.0         1.0 versicolor   <NA>   <NA>
#> 64           6.1         2.9          4.7         1.4 versicolor      2      1
#> 65           5.6         2.9          3.6         1.3 versicolor   <NA>      2
#> 66           6.7         3.1          4.4         1.4 versicolor      3      1
#> 67           5.6         3.0          4.5         1.5 versicolor      3      2
#> 68           5.8         2.7          4.1         1.0 versicolor      4      2
#> 69           6.2         2.2          4.5         1.5 versicolor      1      2
#> 70           5.6         2.5          3.9         1.1 versicolor      5      1
#> 71           5.9         3.2          4.8         1.8 versicolor      2      1
#> 72           6.1         2.8          4.0         1.3 versicolor      5      1
#> 73           6.3         2.5          4.9         1.5 versicolor      3      1
#> 74           6.1         2.8          4.7         1.2 versicolor      4      2
#> 75           6.4         2.9          4.3         1.3 versicolor      4      2
#> 76           6.6         3.0          4.4         1.4 versicolor      1      2
#> 77           6.8         2.8          4.8         1.4 versicolor      4      2
#> 78           6.7         3.0          5.0         1.7 versicolor      2      2
#> 79           6.0         2.9          4.5         1.5 versicolor      2      1
#> 80           5.7         2.6          3.5         1.0 versicolor      2      1
#> 81           5.5         2.4          3.8         1.1 versicolor      3      1
#> 82           5.5         2.4          3.7         1.0 versicolor      2      2
#> 83           5.8         2.7          3.9         1.2 versicolor      3      1
#> 84           6.0         2.7          5.1         1.6 versicolor      2      2
#> 85           5.4         3.0          4.5         1.5 versicolor      5      2
#> 86           6.0         3.4          4.5         1.6 versicolor      1      2
#> 87           6.7         3.1          4.7         1.5 versicolor      2      2
#> 88           6.3         2.3          4.4         1.3 versicolor      4      2
#> 89           5.6         3.0          4.1         1.3 versicolor      2      1
#> 90           5.5         2.5          4.0         1.3 versicolor   <NA>      2
#> 91           5.5         2.6          4.4         1.2 versicolor   <NA>      2
#> 92           6.1         3.0          4.6         1.4 versicolor   <NA>      2
#> 93           5.8         2.6          4.0         1.2 versicolor      4   <NA>
#> 94           5.0         2.3          3.3         1.0 versicolor      4      1
#> 95           5.6         2.7          4.2         1.3 versicolor      4      2
#> 96           5.7         3.0          4.2         1.2 versicolor      5   <NA>
#> 97           5.7         2.9          4.2         1.3 versicolor      3      2
#> 98           6.2         2.9          4.3         1.3 versicolor      5   <NA>
#> 99           5.1         2.5          3.0         1.1 versicolor      4      1
#> 100          5.7         2.8          4.1         1.3 versicolor   <NA>   <NA>
#> 101          6.3         3.3          6.0         2.5  virginica      1      1
#> 102          5.8         2.7          5.1         1.9  virginica      5      2
#> 103          7.1         3.0          5.9         2.1  virginica      3      2
#> 104          6.3         2.9          5.6         1.8  virginica      3      2
#> 105          6.5         3.0          5.8         2.2  virginica   <NA>      1
#> 106          7.6         3.0          6.6         2.1  virginica      2      1
#> 107          4.9         2.5          4.5         1.7  virginica      3   <NA>
#> 108          7.3         2.9          6.3         1.8  virginica      3   <NA>
#> 109          6.7         2.5          5.8         1.8  virginica      5      1
#> 110          7.2         3.6          6.1         2.5  virginica   <NA>      1
#> 111          6.5         3.2          5.1         2.0  virginica   <NA>      1
#> 112          6.4         2.7          5.3         1.9  virginica      2      1
#> 113          6.8         3.0          5.5         2.1  virginica      2      2
#> 114          5.7         2.5          5.0         2.0  virginica      4      2
#> 115          5.8         2.8          5.1         2.4  virginica      4      2
#> 116          6.4         3.2          5.3         2.3  virginica      1      2
#> 117          6.5         3.0          5.5         1.8  virginica      3      1
#> 118          7.7         3.8          6.7         2.2  virginica      5      2
#> 119          7.7         2.6          6.9         2.3  virginica      4      2
#> 120          6.0         2.2          5.0         1.5  virginica      1      2
#> 121          6.9         3.2          5.7         2.3  virginica      4      1
#> 122          5.6         2.8          4.9         2.0  virginica      1      1
#> 123          7.7         2.8          6.7         2.0  virginica      4      1
#> 124          6.3         2.7          4.9         1.8  virginica      3      1
#> 125          6.7         3.3          5.7         2.1  virginica      2      1
#> 126          7.2         3.2          6.0         1.8  virginica      2      1
#> 127          6.2         2.8          4.8         1.8  virginica      1      1
#> 128          6.1         3.0          4.9         1.8  virginica      5      1
#> 129          6.4         2.8          5.6         2.1  virginica      2      2
#> 130          7.2         3.0          5.8         1.6  virginica      3      2
#> 131          7.4         2.8          6.1         1.9  virginica      2      1
#> 132          7.9         3.8          6.4         2.0  virginica   <NA>      2
#> 133          6.4         2.8          5.6         2.2  virginica   <NA>      2
#> 134          6.3         2.8          5.1         1.5  virginica      1      2
#> 135          6.1         2.6          5.6         1.4  virginica      4      2
#> 136          7.7         3.0          6.1         2.3  virginica      5      2
#> 137          6.3         3.4          5.6         2.4  virginica      1      1
#> 138          6.4         3.1          5.5         1.8  virginica      1      1
#> 139          6.0         3.0          4.8         1.8  virginica   <NA>      1
#> 140          6.9         3.1          5.4         2.1  virginica      3      2
#> 141          6.7         3.1          5.6         2.4  virginica      4      1
#> 142          6.9         3.1          5.1         2.3  virginica   <NA>      1
#> 143          5.8         2.7          5.1         1.9  virginica      4      2
#> 144          6.8         3.2          5.9         2.3  virginica      5      1
#> 145          6.7         3.3          5.7         2.5  virginica      1      2
#> 146          6.7         3.0          5.2         2.3  virginica   <NA>      1
#> 147          6.3         2.5          5.0         1.9  virginica   <NA>      1
#> 148          6.5         3.0          5.2         2.0  virginica      5      2
#> 149          6.2         3.4          5.4         2.3  virginica   <NA>      2
#> 150          5.9         3.0          5.1         1.8  virginica      2      1
#>     fact_3        num
#> 1        B  3.1717167
#> 2        A         NA
#> 3        B 44.9529903
#> 4        A 81.6434591
#> 5        B         NA
#> 6        A 50.3649662
#> 7        A 11.1263213
#> 8        A  3.7403557
#> 9        B 46.7472134
#> 10       B 51.9327765
#> 11       B 75.9403215
#> 12       A 61.1977288
#> 13       A  1.1450282
#> 14       B 45.0252073
#> 15       B 54.3468488
#> 16       A 39.8465445
#> 17       B 67.4682370
#> 18       B 53.3991854
#> 19       B 82.0563930
#> 20       B 69.2982938
#> 21       B 13.6008987
#> 22       A 11.4675236
#> 23       A         NA
#> 24       B 83.2519779
#> 25       B 41.0701061
#> 26       B 87.4644254
#> 27       B 60.0492418
#> 28       B 90.8197017
#> 29       A 93.9595681
#> 30       B 31.9131322
#> 31       A 85.4850599
#> 32       A 24.8060275
#> 33       B 62.6830727
#> 34       A 52.8324767
#> 35       A 40.8598437
#> 36       A 50.0266344
#> 37       A 80.0462805
#> 38       B  7.6858412
#> 39       A 84.8332686
#> 40       A 37.1300384
#> 41       A 39.4321325
#> 42       A 44.9044774
#> 43       B 12.9890096
#> 44       B  5.6664215
#> 45       A 32.8074185
#> 46       B 88.2498574
#> 47       B 64.2934205
#> 48       B 76.5860959
#> 49       B 65.9117179
#> 50       A 38.9627071
#> 51       B 19.1408303
#> 52       B 57.5219100
#> 53       B         NA
#> 54       B 27.3056834
#> 55       A 95.3357422
#> 56       B 63.4957057
#> 57       A 66.5872692
#> 58       B 95.2774906
#> 59       A 10.7043567
#> 60       B  4.7796691
#> 61       A 32.9740928
#> 62       A 41.1501455
#> 63       B  4.2095233
#> 64       A 23.5696293
#> 65       A 18.0740789
#> 66       B 19.1704448
#> 67       B 27.5915451
#> 68       A 29.5304337
#> 69       A 32.3227639
#> 70       B 53.5387004
#> 71       A 64.0824615
#> 72       A 31.0960064
#> 73       A 46.7481547
#> 74       A 16.7860122
#> 75       A 67.1699407
#> 76       B 50.2590120
#> 77       A 27.2360890
#> 78       B 41.6003887
#> 79       B 75.8475623
#> 80       B 52.5921551
#> 81       B 72.7250239
#> 82       B 25.5964008
#> 83       A 31.7585155
#> 84       B 98.5648339
#> 85       B 91.5568275
#> 86       B 34.4082221
#> 87       B 87.1667733
#> 88       B 33.6512246
#> 89       A 44.0087063
#> 90       B 78.7996605
#> 91       B 32.1441312
#> 92       A 82.1734726
#> 93       B         NA
#> 94       B 87.9095407
#> 95       B 50.1208780
#> 96       B 39.7526410
#> 97       B 15.1456370
#> 98       B 95.9687139
#> 99       B 69.5423287
#> 100      A 44.1190344
#> 101      A  5.0722205
#> 102      B 17.4807121
#> 103      A 58.5275220
#> 104      B 71.4015882
#> 105      A 42.4191506
#> 106      B 12.9888219
#> 107      B 67.5762224
#> 108      B 98.2841725
#> 109      B 40.1220595
#> 110      B 34.3481727
#> 111      B 12.3327930
#> 112      A 59.5145461
#> 113      A 26.6361858
#> 114      B 94.5730463
#> 115      B 84.6639831
#> 116      B 49.2549533
#> 117      B 63.0293332
#> 118      A  0.8497997
#> 119      A 37.6243459
#> 120      A 62.7224390
#> 121      B 69.1074228
#> 122      B 22.6246895
#> 123      B 73.2553372
#> 124      B 71.4735125
#> 125      A 28.3683399
#> 126      B  8.8723030
#> 127      B 67.4737833
#> 128      B 60.9562948
#> 129      B 94.5126268
#> 130      A 60.7674346
#> 131      B 60.2700291
#> 132      A 96.7778970
#> 133      A 45.4086109
#> 134      A 72.9262048
#> 135      B 43.0900064
#> 136      A 71.5860241
#> 137      A  5.5448262
#> 138      B 90.4807321
#> 139      B 87.1088252
#> 140      A 94.8159818
#> 141      B 77.6664159
#> 142      A 26.3304858
#> 143      A 51.4023354
#> 144      B 21.8872877
#> 145      B 46.4603576
#> 146      B 83.7515399
#> 147      B 30.2143595
#> 148      B 14.0340603
#> 149      B 59.5654426
#> 150      A 16.4448044
#> 
#> Slot "digits.qt":
#> [1] 1
#> 
#> Slot "digits.ql":
#> [1] 1
```
