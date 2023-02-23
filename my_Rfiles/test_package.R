set.seed(42)
nrow = 1000
ncol = 250
test_df<-matrix(data = NA, nrow = nrow, ncol = ncol)
value<-sapply(1:ncol(test_df), function(x){
  if (x %% 2 == 0){
    n = sample(1:5, size = 1)
    pat = c("[A-E]", "[F-J]", "[K-O]", "[P-T]", "[U-Y]")
    p = sample(1:5, size = 1)
    set.seed(42)
    return(stringi::stri_rand_strings(n = nrow(test_df), length = n , pattern = pat[p]))
  }
  else
  {
    i = sample(1:100, size = 1)
    j = sample(i:1000, size = 1)
    return(sample(i:j, size = nrow(test_df), replace = TRUE))
  }
})
test_df<-matrix(data = value, nrow = nrow, ncol = ncol)
test_df<-as.data.frame(test_df)
colnames(test_df)<-1:ncol(test_df)
for (i in 1:ncol(test_df)){
  k = sample(i:ncol(test_df), size = 1)
  if (i %% k == 0){
    n = sample(1:nrow(test_df), 1)
    r = sample(1:nrow(test_df), n)
    test_df[r,i]<-NA
  }
}
test_df[,"10"]<-stringi::stri_rand_strings(n = nrow(test_df), length = 1 , pattern = "[A-B]")
n_col<-as.list(1:ncol(test_df))
env = environment()
test_df2<-test_df
r<-sapply(n_col, function(x, test_df){
  tryCatch({
    env$test_df2[,x]<-as.numeric(test_df[,x])
  }, 
  warning=function(w){
    env$test_df2[,x]<-as.factor(test_df[,x])
  })
  return(env$test_df2[,x])
}, test_df)
rm(r, env, n_col, value)
# 
for (i in 1:ncol(test_df2)){
  if (is.factor(test_df2[,i]) && nlevels(test_df2[,i]) == 2)
    print(i)
}
rm(i, k, n)
Sys.time()
library(doudpackage)
setwd("/Users/tiago2/BF/doudpackage/old/R")
files.sources = list.files()
sapply(files.sources, source)
library(tictoc)
tic("old")
old<-ft_desc_tab(test_df2, group = "10")
toc("end of old")
Sys.sleep(3)
tic("new")
new<-descTab(test_df2, group = "10")
toc("end of new")