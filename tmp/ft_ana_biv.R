#' Bivariate analysis
#'
#' Function that allows you to compute a p value according to a binary variable; default tests are Wilcoxon for quantitative variables, chisq or fisher wether chisq.test returns a warning
#' @param data A database, dataframe object
#' @param group Variable from which sub-groups are made to compute p values
#' @param signi Significance levels (def = 0.20)
#' @import stats
#' @import lubridate
#' @return Return a dataframe with univariate analaysis
#' @export
ft_ana_biv<-function(data, group, signi=0.20){
  tmp<-as.data.frame(matrix(NA, ncol(data)-1, 4))
  colnames(tmp)<-c("var", "test", "p", "signi")
  options(warn=0)
  my_env<-environment()
  for (i in 1:ncol(data)){
    if (colnames(data)[i]==group)
      next;
    if((is.numeric(data[,i])||is.integer(data[,i])) &&
       (min(data[,i], na.rm = TRUE) != max(data[,i], na.rm = TRUE))){
      tmp[i,"var"]<-colnames(data)[i]
      tmp[i,"test"]<-"t.test"
      tmp[i,"p"]<-round(t.test(data[,i]~data[,group])$p.value, digits = 7)
      tmp[i, "signi"]<-ifelse(tmp[i,"p"]<signi,"OUI","non")
    }else if (is.factor(data[,i]) && !is.Date(data[,i]) && nlevels(data[,i]) > 1){
      tryCatch(
        {
          c<-c(chisq.test(data[,i], data[,group], correct=FALSE), test_name = "chi2")
        },
        warning=function(w){
          my_env$c<-c(fisher.test(data[,i], data[,group], simulate.p.value = TRUE), test_name = "fisher")
        },
        finally = {
          tmp[i,"var"]<-colnames(data)[i]
          tmp[i,"test"]<-my_env$c$test_name
          tmp[i,"p"]<-round(c$p.value,digits =7)
          tmp[i, "signi"]<-ifelse(c$p.value<signi,"OUI","non")
        })
    } else
    {
      tmp[i,"var"]<-colnames(data)[i]
      tmp[i,"test"]<-"not conformed to be tested"
      tmp[i,"p"]<-NA
      tmp[i, "signi"]<-NA
    }
  }
  tmp<-tmp[complete.cases(tmp$var),]
  return (tmp)
}
