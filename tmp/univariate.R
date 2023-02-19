##### Quali Univ Fun ###
qualiUnivFun<-function(x, data_sub, group, digits.ql){
  for (i in 1:nlevels(data_sub[, x@name])){
    tab = table(data_sub[, x@name], useNA = "always")
    n = tab[i]
    prop = round(prop.table(tab) * 100, digits.ql)[i]
    
    parsed_name = paste(x@name, levels(data_sub[, x@name])[i], sep = ", ")
    value = paste(n, " (", prop, ")", sep = "")
    group_var<-as.character(ifelse(is.null(group), "Total", group))
    
    if (i == nlevels(data_sub[, x@name])){
      n.missing.value = tab[(nlevels(data_sub[, x@name]) + 1)]
      prop.missing.value = round(prop.table(tab) * 100, digits = digits.ql)[(nlevels(data_sub[, x@name]) + 1)]
      missing.value = paste(n.missing.value, " (", prop.missing.value, ")", sep = "")
      missing.value.name = paste(x@name , "Missing values", sep = ".")
    }
    else{
      missing.value = ""
      missing.value.name = ""
    }
    var.group <-VarGroup(x = x,
                       group_var = group_var, pvalue = x@pvalue, 
                       parsed_name = parsed_name, value = value, 
                       missing.value = missing.value, 
                       missing.value.name = missing.value.name)

    if(!exists("var.group_list", inherits = FALSE))
      var.group_list<-var.group
    else
      var.group_list<-c(var.group_list, var.group)
  }
  return(var.group_list)
}

setGeneric("anaUniv", function(var, group, data, 
                               digits.qt, digits.ql) {
  return(standardGeneric("anaUniv"))
})

setMethod("anaUniv", "listVar", function(var, group, data, 
                                         digits.qt, digits.ql){
    numeric_list<-compact(lapply(var@List, function(x){if("numeric" %in% x@type) return(x)}))
    if (!is.null(group))
      factor_list<-compact(lapply(var@List, function(x){if("factor" %in% x@type && 
                                                          x@name != group) return(x)}))
    else
      factor_list<-compact(lapply(var@List, function(x){if("factor" %in% x@type) return(x)}))
    
    numeric_list<-new("listVar", List = numeric_list)
    factor_list<-new("listVar", List = factor_list)
    if (!is.null(group)){
      for (i in 1:nlevels(data[,group])){
        data_sub<-data[data[,group] == levels(data[,group])[i],]
        quali.Univ_list.tmp<-lapply(factor_list@List, qualiUnivFun, data_sub = data_sub,
                                    group = levels(data[,group])[i], digits.ql = digits.ql)
        if (!exists("quali.Univ_list.Group", inherits = FALSE))
          quali.Univ_list.Group<-quali.Univ_list.tmp
        else
          quali.Univ_list.Group<-c(quali.Univ_list.Group, quali.Univ_list.tmp)
        }
      }
   lst_VarGroup.Univ.Total<-lapply(factor_list@List, qualiUnivFun, data_sub = data,
                                   group = NULL, digits.ql = digits.ql)
   if (exists("quali.Univ_list.Group", inherits = FALSE)){
     quali.Univ_list.Global<-unlist(c(quali.Univ_list.Group, lst_VarGroup.Univ.Total))
     return(unlist(quali.Univ_list.Global))
   }
   else
     return(unlist(lst_VarGroup.Univ.Total))
})
