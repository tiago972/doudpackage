digits.opt = 1
nonnormal = 0

ft_tab_quanti<-function(col.var, digits.opt = 1, nonnormal = 0){
  print(names(col.var))
  if(!is.numeric(col.var))
    return(NA)
  "Min"<-min(col.var, na.rm=TRUE)
  "Max"<-max(col.var, na.rm=TRUE)
  if (nonnormal == 0){
    "mean"<-round(mean(col.var, na.rm = T), digits = digits.opt)
    "sd"<-round(sd(col.var, na.rm = T), digits = digits.opt)
    tmp_mat<-c(var = colnames(data)[i],
               "Total"=gsub(" ", "", paste(mean, "(",  sd,")")), "Min-Max"=paste(Min, Max, sep="-"))
  }
  print(tmp_mat)
  return(tmp_mat)
}

ft_runquanti_tab<-function(data, group, digits.opt, nonnormal){
  # if(is.null(group))
  #   return(ft_tab_quanti_null())
  # else{
  res<-data.frame(unlist(by(data[,"Age"], data[,group], ft_tab_quanti, simplify = F), use.names = F))
    return(res)
  # }
}
a<-ft_runquanti_tab(bdd, group = "Sexe", digits.opt, nonnormal)
b<-ft_runquanti_tab(bdd, group = "Sexe", digits.opt, nonnormal)

test<-split(bdd, bdd$Sexe)
