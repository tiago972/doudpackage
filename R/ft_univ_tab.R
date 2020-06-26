##### Il faut rajouter en fait une option pour afficher ou non ou les Nas dans le tableau
#### Il faut changer le nom de l option complete en parsed et rajouter une option pour la p-value
### L'option complete devra en faite retourner les tableau concatener


#' Generic function to create a table of descriptive analysis of you dataset
#'
#' This function allows you to display all together all univariate analysis (min-max; median; IQR; proportions) and bivariates analysis (wilcoxon, chisq or fisher). The univariate analysis can be grouped by a viariable of interest (binary viarble)
#' @param data your dataset, needs to be a dataframe object
#' @param group your variable of interest for you univariate analylisis to be splited
#' @param complete wether or not you want your table to be parsed into a kable object and p value displayed; default = TRUE
#' @param min.max wether or not youd want to display min and max value for quantitative variables; default is false
#' @param digits.opt How many numbers after the "." you'd like for the proportions of qualitative variables; default is 1
#' @param quanti To be used only if complete is FALSE. If TRUE, returns only the univariate analysis for quantitative variables
#' @param quali To be used only if complete is FALSE. If TRUE returns only the univariate analysis for qualitative variables
#' @import knitr
#' @import kableExtra
#' @return the object returned depends on the complete option:either a dataframe or a kable oject
#' @export
ft_univ_tab<-function(data, group=NULL, complete=TRUE, min.max=FALSE, digits.opt=1, quanti=FALSE, quali=FALSE){
  if (!is.null(group) && (!is.factor(data[,group]) || nlevels(data[,group]) > 3))
  {
    write("Grouping error dude, check if the variable is a binary factor", stderr())
    return (-1)
  }
  if (isTRUE(complete) || isTRUE(quanti))
    quanti_tab<-ft_quanti.group_false(data, group, complete, min.max)
  if (isTRUE(complete) || isTRUE(quali))
    quali_tab<-ft_quali.group_false(data, group, complete, digits.opt)
  if (!isTRUE(complete) && isTRUE(quanti))
    return(quanti_tab)
  else if (!isTRUE(complete) && isTRUE(quali))
    return(quali_tab)
  res<-merge(quanti_tab, quali_tab, all=TRUE)
  return (res)
}
