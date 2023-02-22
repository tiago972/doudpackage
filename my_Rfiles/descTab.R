# Function to get var types in a custom object list
varType<-function(data, normality){
  list_names<-colnames(data)
  list_names<-list_names[order(list_names)]
  list_names<-as.list(list_names)
  type_list<-lapply(data, class)
  type_list<-type_list[order(names(type_list))]
  if (normality %in% c("manual", "assess"))
    stop(sprintf("Not ready yet for %s", normality))
  n<-ifelse(normality == "normal", TRUE, FALSE)

  list_res<-mapply(Var,list_names, type_list, normal = n)
  list_res<-new("listVar", List = list_res)
  return(list_res)
}

library(tidyverse)

descTab<-function(data, group=NULL, quanti=TRUE, quali=TRUE, na.print = FALSE,
                      pvalue=TRUE, digits.p=3L, digits.qt = 1L,
                  digits.ql = 1L, normality = "normal")
{
  checkVarDescTab(data, group, quanti, quali, na.print, pvalue, digits.p, digits.qt,
           digits.ql, normality)
  if (!is.null(group) && !is.factor(data[, group]))
    stop(sprintf("group needs to be a factor, %s is %s", group, class(data[, group])))
  var_list<-varType(data, normality)
  print("ok")
  ana.biv_list<-anaBiv(var_list, data = data, group = group, digits.p = digits.p)
  if (is.null(ana.biv_list))
    ana.biv_list<-lapply(var_list@List, as, Class = "VarGroup")
  ana.biv_list<-new("listVar", List = ana.biv_list)
  print("b4")
  print(head(ana.biv_list))
  ana.univ_list<-anaUniv(ana.biv_list, group = group, data = data, digits.qt = digits.qt,
                         digits.ql = digits.ql, quali = quali, quanti = quanti)
  print("after")
  print(head(ana.biv_list))
  return<-makeTable(ana.univ_list, group, pvalue, na.print)

  return.table<-parseClass(table = return$df, group = group, quanti = quanti,
                           quali = quali, na.print = na.print,
                           pvalue = return$pvalue,
                           var_list = var_list, data = data,
                           digits.qt = digits.qt, digits.ql = digits.ql)
  return(return.table)
}
