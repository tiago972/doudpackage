digits.opt = 1
nonnormal = 0

ft_tab_quanti<-function(col.var, digits.opt = 1, nonnormal = 0){
  if(!is.numeric(col.var))
    return(NA)
  print(names(col.var))
  "Min"<-min(col.var, na.rm=TRUE)
  "Max"<-max(col.var, na.rm=TRUE)
  if (nonnormal == 0){
    "mean"<-round(mean(col.var, na.rm = T), digits = digits.opt)
    "sd"<-round(sd(col.var, na.rm = T), digits = digits.opt)
    tmp_mat<-c(var = names(col.var),
               "Total"=gsub(" ", "", paste(mean, "(",  sd,")")), "Min-Max"=paste(Min, Max, sep="-"))
  }
  print(tmp_mat)
  return(tmp_mat)
}

ft_runquanti_tab<-function(data, group, digits.opt, nonnormal){
  # if(is.null(group))
  #   return(ft_tab_quanti_null())
  # else{
    res<-sapply(bdd[,'Age'], bdd[,"Sexe"], ft_tab_quanti, digits.opt, nonnormal, simplify = T)

    return(res)
  # }
}
a<-ft_runquanti_tab(bdd, group = "Sexe", digits.opt, nonnormal)
