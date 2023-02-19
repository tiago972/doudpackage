varType<-function(data, normality){
  list_names<-colnames(data)
  list_names<-list_names[order(list_names)]
  list_names<-as.list(list_names)
  type_list<-lapply(data, class)
  type_list<-type_list[order(names(type_list))]
  # If normality == "assess", assess normality
  # If normality == "manuel_assess"
  n<-ifelse(normality == "normal", TRUE, FALSE)
  
  list_res<-mapply(Var,list_names, type_list, normal = n)
  list_res<-new("listVar", List = list_res)
  return(list_res)
}

library(tidyverse)

descTab<-function(data, group=NULL, quanti=TRUE, quali=TRUE, na.print = FALSE, 
                      p.value=TRUE, min.max=FALSE, digits.p=3, digits.qt = 1, 
                  digits.ql = 1, normality = "normal")
{
  var_list<-varType(data, normality)
  ana.biv_list<-anaBiv(var_list, data = data, group = group, digits.p = digits.p)
  if (is.null(ana.biv_list))
    ana.biv_list<-lapply(var_list@List, as, Class = "VarGroup")
  ana.biv_list<-new("listVar", List = ana.biv_list)
  ana.univ_list<-anaUniv(ana.biv_list, group = group,
                         data = data, digits.qt = digits.qt, digits.ql = digits.ql)
  return(ana.univ_list)
}
