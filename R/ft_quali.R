### Function ton repeate p value k times for lines ##
ft_parse_p_quali<-function(biv, total)
{
  i = 0;
  tmp<-c()

  while (i < nrow(biv))
  {
    i = i + 1;
    patt = paste("^", biv[i,"var"], ",.*",sep = "")
    k = table(grepl(pattern = patt, total$var))[2]
    if (!is.na(k))
    {
      tmp2<-data.frame("p" = rep(biv[i, "p"], k), "var" = rep(biv[i, "var"], k))
      tmp3<-data.frame("p" = rep(NA, 3), "var" = rep(biv[i, "var"], 3))
      tmp2<-rbind(tmp2, tmp3)
      tmp<-rbind(tmp, tmp2)
    }
  }
  if (!is.null(tmp))
    tmp<-tmp[order(tmp$var),]
  return(tmp)
}

ft_quali.pvalue<-function(data, res, group, na.print, digits.opt){
  res_tot<-ft_quali(data, NULL, NULL, na.print, digits.opt)
  res_tot$Group="Total"
  remove_group = min(grep(pattern = paste(group, ",.*", sep = ""), res_tot$var))
  res_tot<-res_tot[-c(remove_group:(remove_group + 1)),]
  remove_group = grep(pattern = paste(group, ".Missing*", sep = ""), res_tot$var)
  res_tot<-res_tot[-c(remove_group:(remove_group)),]
  total<-merge(res, res_tot, all = T)
  biv<-ft_ana_biv(data, group, 3)
  biv<-ft_parse_p_quali(biv, total)
  total<-total[order(total$var),]
  total$p<-biv[,"p"]
  return(total)
}

#' @import tidyr
ft_quali_grouped<-function(data, group, p.value, na.print, digits.opt)
{
  my_env<-environment()
  for (i in 1:nlevels(data[,group]))
  {
    tmp<-data[which(data[,group]==levels(data[,group])[i]),]
    tmp<-tmp[,-which(colnames(tmp)==group)]
    tmp<-ft_quali(tmp, group=NULL, p.value, na.print, digits.opt)
    tmp$Group=levels(data[,group])[i]
    nametab<-paste("tmp", i,sep="_")
    assign(nametab,tmp, envir = my_env)
  }
  res<-merge(tmp_1, tmp_2)
  return (ft_quali.pvalue(data, res, group, na.print, digits.opt))
}

ft_quali<-function(data, group=NULL, p.value, na.print, digits.opt){
  if (is.null(group))
  {
    j <- 0;
    tmp<-data.frame("var" = NA, "Total" = NA)
    for (i in 1:ncol(data))
    {
      if (!is.factor(data[,i]) || nlevels(data[,i]==1))
        next;
      for (k in 1:nlevels(data[,i]))
      {
          tmp[(j + k), "var"]<-paste(colnames(data)[i], levels(data[,i])[k], sep = ", ")
          tmp[(j + k), "Total"]<-paste(table(data[,i])[k], " (",
                                     round(prop.table(table(data[,i], useNA = "always"))[k] * 100, digits = digits.opt),
                                     ")", sep ="")
          if (k == nlevels(data[,i]))
            j = j + k
      }
      j = j + 1
      tmp[j, "var"]<-paste(colnames(data)[i], "Missing values, n(%)", sep = ".")
      tmp[j, "Total"]<-paste(table(data[,i], useNA = "always")[nlevels(data[,i]) + 1], " (",
                        round(prop.table(table(data[,i], useNA = "always"))[nlevels(data[,i]) + 1] * 100, digits = digits.opt),
                        ")", sep = "")
    }
    return (tmp)
  }
    else
      return (ft_quali_grouped(data, group, p.value, na.print, digits.opt))
}
