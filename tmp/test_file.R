data("iris")
iris$fact_1<-as.factor(as.character(sample(1:5, 150, replace = TRUE)))
n_na<-sample(1:150, 30)
iris[n_na, "fact_1"]<-NA

iris$fact_2<-as.factor(as.character(sample(1:2, 150, replace = TRUE)))
n_na<-sample(1:150, 10)
iris[n_na, "fact_2"]<-NA

iris$fact_3<-as.factor(as.character(stri_rand_strings(150, 1, '[A-B]')))

summary(iris)

iris_test<-descTab(iris, group = "Species")
ana.tmp<-anaUniv(iris_test, group = "Species", data = iris, digits.qt = 1, digits.ql = 1)


test<-makeTable(iris_test, group = "Species")
rm(tmp_df)


