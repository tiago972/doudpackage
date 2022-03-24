#' Bivariate analysis
#'
#' Function that allows you to compute a p value according to a binary variable; default tests are Wilcoxon for quantitative variables, chisq or fisher wether chisq.test returns a warning
#' @param data A database, dataframe object
#' @param group Variable from which sub-groups are made to compute p values
#' @param signi Significance levels (def = 0.20)
#' @param nonnormal Treat all quantitative variable as non normal variables: will compute p.value according to Wilcox.test. Default is 0
#' @import stats
#' @import lubridate
#' @return Return a dataframe with univariate analaysis
#' @export
#### Achecker impérativement: erreur quand la comparaison n'est pas possible
ft_ana_biv<-function(data, group, signi=3, nonnormal = 0){
  tmp<-as.data.frame(matrix(NA, ncol(data)-1, 3))
  colnames(tmp)<-c("var", "test", "p")
  options(warn=0)
  my_env<-environment()
  for (i in 1:ncol(data)){
    if (colnames(data)[i]==group)
      next;
    if((is.numeric(data[,i])||is.integer(data[,i])) &&
       (min(data[,i], na.rm = TRUE) != max(data[,i], na.rm = TRUE))){
        tmp[i,"var"]<-colnames(data)[i]
        if (nonnormal == 0){
          tmp[i,"test"]<-"t.test"
          tryCatch({
            t<-t.test(data[,i]~data[,group])$p.value
          },
            error=function(e){
              warning(paste(e), colnames(data)[i])
            })
        }else {
          tmp[i,"test"]<-"wilcox.test"
          tryCatch({
            t<-wilcox.test(data[,i]~data[,group])$p.value
            },
          warning=function(w){
            my_env$t<-wilcox.test(data[,i]~data[,group], exact = F)$p.value
          })
        }
        if (t < 0.001)
          tmp[i,"p"]<-"< .001"
        else
          tmp[i,"p"]<-round(t, digits = signi)
    }else if (is.factor(data[,i]) && !lubridate::is.Date(data[,i]) && nlevels(data[,i]) > 1){
      tryCatch(
        {
          c<-c(chisq.test(data[,i], data[,group], correct=FALSE), test_name = "chi2")
        },
        error=function(e){
          warning(paste(e), colnames(data)[i])
        },
        warning=function(w){
          my_env$c<-c(fisher.test(data[,i], data[,group], simulate.p.value = TRUE), test_name = "fisher")
        },
        finally = {
          tmp[i,"var"]<-colnames(data)[i]
          tmp[i,"test"]<-my_env$c$test_name
          if (c$p.value < 0.001)
            tmp[i,"p"]<-"< .001"
          else
            tmp[i,"p"]<-round(c$p.value,digits = signi)
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
