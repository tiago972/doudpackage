ft_parse_quali_opt<-function(tmp, opt, v_group=NULL, na.print)
{
  if (opt == 1)
  {
    tmp[,"n(%)"]<-paste(tmp$n, "(", tmp[,"%"], ")", sep="")
    tmp[,"NAs(%)"]<-paste(tmp[,"NAs"], "(", tmp[,"%_NAs"], ")", sep = "")
    tmp<-tmp[,!names(tmp) %in% c("n", '%', "NAs", "%_NAs")]
    if (isFALSE(na.print))
    {
      tmp<-tmp[,!names(tmp) %in% "NAs(%)"]
      for (i in 1:nrow(tmp))
        tmp[i,1]<-paste(tmp[i,1], "n(%)", sep = " ")
    }
    else
    {
      for (i in 1:nrow(tmp))
        tmp[i,1]<-paste(tmp[i,1], "(n(%);NAs(%))", sep = " ")
      tmp[,"n(%)"]<-paste(tmp[,"n(%)"], tmp[,"NAs(%)"], sep=";")
      tmp<-tmp[,!names(tmp) %in% "NAs(%)"]
    }
  }
  else if (opt == 2)
    colnames(tmp)[colnames(tmp)=="n(%)"]<-"Total"
  else if (opt == 3)
  {
    if (isFALSE(na.print))
    {
      tmp<-tmp[,!names(tmp) %in% "NAs(%)"]
      for (i in 1:nrow(tmp))
        tmp[i,1]<-paste(tmp[i,1], "n(%)", sep = " ")
    }
    else
    {
      for (i in 1:nrow(tmp))
        tmp[i,1]<-paste(tmp[i,1], "(n(%);NAs(%))", sep = " ")
    }
  }
  return (tmp)
}

ft_quali.pvalue<-function(data, res, group, na.print, digits.opt){
  v_group<-c(levels(data[,group]))
  res_tot<-ft_parse_quali_opt(ft_quali(data, NULL, NULL, na.print, digits.opt), 2, NULL, na.print)
  res<-merge(res, res_tot, all=TRUE)
  p<-ft_parse_quali_opt(ft_ana_biv(data, group), 3, NULL, na.print)
  p<-p[,!names(p) %in% c("signi", "test")]
  p$p<-ifelse(as.numeric(p$p) < 0.001, '<.001', round(as.numeric(p$p), digits=3))
  res<-merge(res, p, all.x=TRUE)
  return(res)
}

#' @import tidyr
ft_quali_grouped<-function(data, group, p.value, na.print, digits.opt)
{
  tmp_1<-data[which(data[,group]==levels(data[,group])[1]),]
  tmp_1<-tmp_1[,-which(colnames(tmp_1)==group)]
  tmp_1<-ft_quali(tmp_1, group=NULL, p.value, na.print, digits.opt)
  tmp_1$Group=levels(data[,group])[1]
  tmp_2<-data[which(data[,group]==levels(data[,group])[2]),]
  tmp_2<-tmp_2[,-which(colnames(tmp_2)==group)]
  tmp_2<-ft_quali(tmp_2, group=NULL, p.value, na.print, digits.opt)
  tmp_2$Group=levels(data[,group])[2]
  res<-merge(tmp_1, tmp_2, all=TRUE)
  res<-pivot_wider(res, names_from = "Group", values_from =  "n(%)")
  if (isFALSE(p.value))
    return(res)
  else
    return (ft_quali.pvalue(data, res, group, na.print, digits.opt))
}

ft_quali<-function(data, group=NULL, p.value, na.print, digits.opt){
  if (is.null(group))
  {
    j <- 0;
    tmp<-as.data.frame(matrix(NA, 1, 6))
    colnames(tmp)<-c("var", "level", "n", "%", "NAs", "%_NAs")
    for (i in 1:ncol(data))
    {
      if (!is.factor(data[,i]) || nlevels(data[,i]==1))
        next;
      for (k in 1:nlevels(data[,i]))
      {
          tmp[j + k, "var"]<-colnames(data)[i]
          tmp[j + k, "level"]<-levels(data[,i])[k]
          tmp[j + k, "n"]<-table(data[,i])[k]
          tmp[j + k, "%"]<-round(prop.table(table(data[,i], useNA = "always"))[k] * 100, digits = digits.opt)
          tmp[j + k, "NAs"]<-table(data[,i], useNA = "always")[nlevels(data[,i]) + 1]
          tmp[j + k, "%_NAs"]<-round(prop.table(table(data[,i], useNA = "always"))[nlevels(data[,i]) + 1] * 100, digits = digits.opt)
          if (k == nlevels(data[,i]))
            j = j + k
      }
    }
    tmp = ft_parse_quali_opt(tmp, 1, group, na.print)
    return (tmp)
  }
    else
      return (ft_quali_grouped(data, group, p.value, na.print, digits.opt))
}
