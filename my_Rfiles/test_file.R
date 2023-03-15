library(devtools)
library(stringi)
data("iris")
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

summary(iris)
load_all(".")

iris_test<-descTab(iris, group = "Species", na.print = TRUE, parallel = TRUE)
iris_test_table<-iris_test@table
testParse<-parseClassFun(iris_test, levels_to_keep = list("fact_2" =  "1"),
                         group_rows_labels = list("Size" = c("Petal.Length", "Petal.Width"),
                        "My_f" = c("num", "fact_2", "fact_3")))


iris_test_table2<-filter(iris_test_table, !(Total %in% "0 (0)" & grepl(".*Missing values", iris_test_table$var)))

