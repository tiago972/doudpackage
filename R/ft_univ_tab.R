#### Il rajouter l option parsed
### Mettre le n(%) de chaque groupe dans le titre
## Il faut rajouter la possibilite d'avoir ou SD ou l'IQR
## Il faut changer l'affichage en mettant les Nas en dessous si il y en a et lvl dans le nom de var
## Il faut rajouter les anova si groupe à > 2 classes; ça implique de revoir les remplissage des tableaux selon la modalité de groupe

#' Generic function to create a table of descriptive analysis of a dataset
#'
#' This function allows you to display all together all univariate analysis (min-max; median; IQR; proportions) and bivariates analysis (wilcoxon, chisq or fisher). The univariate analysis can be sub-grouped by a viariable of interest (binary variable)
#' @param data A dataset, needs to be a dataframe object
#' @param group The variable of interest for you univariate analysis to sub-grouped by.
#' @param complete Wether to print quantitative and qualitative variables; default = TRUE
#' @param quanti To be used only if complete is FALSE. If TRUE, returns only the univariate analysis for quantitative variables
#' @param quali To be used only if complete is FALSE. If TRUE returns only the univariate analysis for qualitative variables
#' @param na.print Wether to print NAs n(%), default = FALSE. Note that if true, "Total" will also be printed. This will be change in a futur version
#' @param p.value Print p value. Group needs to be set; default = TRUE. If TRUE, "Total" will also be printed
#' @param min.max Display min and max value for quantitative variables; default is false
#' @param digits.opt How many numbers after the "." you'd like for the proportions of qualitative variables; default is 0
#' @return The object returned depends on the "parse" option:either a dataframe or a kable oject
#' @export
ft_desc_tab<-function(data, group=NULL, complete = TRUE, quanti=FALSE, quali=FALSE, na.print = FALSE, p.value=TRUE, min.max=FALSE, digits.opt=1)
{
  if ((ft_error(data, group, complete, quanti, quali)) == -1)
    return(-1)
  if (!is.null(group) && table(data[,group], useNA = "always")[nlevels(data[,group]) + 1] != 0)
  {
    warning(paste(table(data[,group], useNA = "always")[nlevels(data[,group]) + 1], " rows have been deleted due to missing values in the defined group" ,sep = ""))
    data<-data[!is.na(data[,group]),]
  }
  if (isTRUE(quanti)||isTRUE(quali))
    complete=FALSE
  if (isTRUE(complete) || isTRUE(quanti))
  {
    quanti_tab<-ft_parse_quanti_opt(ft_quanti(data, group, p.value, min.max, na.print, digits.opt), min.max, na.print, p.value, group)
    print(colnames(quanti_tab))
  }
  if (isTRUE(complete) || isTRUE(quali))
  {
     quali_tab<-ft_parse_quali_opt(ft_quali(data, group, p.value, na.print, digits.opt), na.print, p.value, group)
     print(colnames(quali_tab))
  }
  if (!isTRUE(complete) && isTRUE(quanti))
    return(quanti_tab)
  else if (!isTRUE(complete) && isTRUE(quali))
    return(quali_tab)
  res<-rbind(quanti_tab, quali_tab)
  return (res)
}
