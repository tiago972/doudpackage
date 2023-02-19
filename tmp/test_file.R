data("iris")
iris$fact_1<-as.factor(as.character(sample(1:5, 150, replace = TRUE)))
n_na<-sample(1:150, 30)
iris[n_na, "fact_1"]<-NA

iris$fact_2<-as.factor(as.character(sample(1:2, 150, replace = TRUE)))
n_na<-sample(1:150, 10)
iris[n_na, "fact_2"]<-NA

summary(iris)

iris_test<-descTab(iris, group = "Species")
test<-makeTable(iris_test, group = "Species")
rm(tmp_df)


