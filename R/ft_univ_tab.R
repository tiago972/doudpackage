#### Il rajouter l option parsed
### Il faut refaire quali avec le nouvelles options et en plus opti
### Il faut opti la biv

#' Generic function to create a table of descriptive analysis of a dataset
#'
#' This function allows you to display all together all univariate analysis (min-max; median; IQR; proportions) and bivariates analysis (wilcoxon, chisq or fisher). The univariate analysis can be sub-grouped by a viariable of interest (binary variable)
#' @param data A dataset, needs to be a dataframe object
#' @param group The variable of interest for you univariate analylisis to sub-grouped by.
#' @param complete Wether to print quantitative and qualitative variables; default = TRUE
#' @param quanti To be used only if complete is FALSE. If TRUE, returns only the univariate analysis for quantitative variables
#' @param quali To be used only if complete is FALSE. If TRUE returns only the univariate analysis for qualitative variables
#' @param na.print Wether to print NAs n(%), default = FALSE
#' @param p.value Print p value. Group needs to be set; default = TRUE
#' @param min.max Display min and max value for quantitative variables; default is false
#' @param digits.opt How many numbers after the "." you'd like for the proportions of qualitative variables; default is 1
#' @return The object returned depends on the "parse" option:either a dataframe or a kable oject
#' @export
ft_univ_tab<-function(data, group=NULL, complete = TRUE, quanti=FALSE, quali=FALSE, na.print = FALSE, p.value=TRUE, min.max=FALSE, digits.opt=1){
  if (!is.null(group) && (!is.factor(data[,group]) || nlevels(data[,group]) > 3))
  {
    write("Grouping error dude, check if the variable is a binary factor", stderr())
    return (-1)
  }
  if (isTRUE(quanti)||isTRUE(quali))
    complete=FALSE
  if (isTRUE(complete) || isTRUE(quanti))
    quanti_tab<-ft_quanti(data, group, p.value, min.max, na.print)
  # if (isTRUE(complete) || isTRUE(quali))
  #   quali_tab<-ft_quali.group_false(data, p.value, complete, digits.opt)
  if (!isTRUE(complete) && isTRUE(quanti))
    return(quanti_tab)
  else if (!isTRUE(complete) && isTRUE(quali))
    return(quali_tab)
  res<-merge(quanti_tab, quali_tab, all=TRUE)
  return (res)
}
