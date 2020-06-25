#' Graphical distribution of variables
#'
#' Generic function to display graphical reprensentation of all data in a dataset
#' @param data An object of dataframe type
#' @import graphics
#' @export
#'
#' @examples library(doudpackage)
#' @examples par(mfrow=c(3,3))
#' @examples ft_distrib(cars)
ft_distrib<-function (data){
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i]))
      hist(data[,i],  main = NULL, xlab=colnames(data)[i])
    else if (is.factor(data[,i]))
      plot(data[,i], main=colnames(data)[i])
  }
}
