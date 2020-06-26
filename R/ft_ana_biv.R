#' Bivariate analysis
#'
#' Function that allows you to compute a p value according to a binary variable; default tests are Wilcoxon for quantitative variables, chisq or fisher wether chisq.test returns a warning
#' @param data A database, dataframe object
#' @param var Variable from which groups are made to compute p values
#' @param signi Significance levels (def = 0.20)
#' @import stats
#' @import lubridate
#' @return Return a dataframe with univariate analaysis
#' @export
## peut etre clairement opti ##
ft_ana_biv<-function(data, var, signi=0.20){
  options(warn=0)
  my_env<-environment()
  for (i in 1:ncol(data)){
    if (colnames(data)[i]==var)
      next;
    if((is.numeric(data[,i])||is.integer(data[,i])) &&
       (min(data[,i], na.rm = TRUE) != max(data[,i], na.rm = TRUE))){
      nom<-colnames(data)[i]
      w<-wilcox.test(data[,i]~data[,var])
      signi<-ifelse(w$p.value<signi,"OUI","non")
      analyse<-c(nom=nom, test='wilcoxon',p=round(w$p.value, digits =7), signi=signi)
      nomtest<-paste("analyse",i,sep="_")
      assign(nomtest,analyse, envir = my_env)
    }else if (is.factor(data[,i]) && !is.Date(data[,i]) && nlevels(data[,i]) > 1){
      nom<-colnames(data)[i]
      tryCatch(
        {
          c<-c(chisq.test(data[,i], data[,var], correct=FALSE), test_name = "chi2")
        },
        warning=function(w){
          my_env$c<-c(fisher.test(data[,i], data[,var], simulate.p.value = TRUE), test_name = "fisher")
        },
        finally = {
          signi<-ifelse(c$p.value<signi,"OUI","non")
          analyse<-c(nom=nom, test=my_env$c$test_name,p=round(c$p.value,digits =7), signi=signi)
          nomtest<-paste("analyse",i,sep="_")
          assign(nomtest,analyse, envir = my_env)
        })
    } else
    {
      analyse<-(paste(colnames(data)[i], 'no test', sep=" "))
      nomtest<-paste("analyse",i,sep="_")
      assign(nomtest,analyse)
    }
  }
  analyse<-data.frame(mget(ls(pattern = "analyse_", envir = my_env), envir = my_env))
  analyse<-as.data.frame(t(analyse))
  return (analyse)
}
