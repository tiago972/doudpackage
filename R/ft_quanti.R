ft_tab_quanti<-function(data, i, group=NULL, group_level=NULL)
{
  if (!is.null(group))
    subset<-data[which(data[,group]==group_level),]
  else
    subset<-data
  "Min"<-round(min(subset[,i], na.rm=TRUE), 1)
  "Max"<-round(max(subset[,i], na.rm=TRUE), 1)
  "mediane"<-round(quantile(subset[,i], probs = seq(0,1,0.5),na.rm=TRUE)[2], 1)
  "first_quartile"<-round(quantile(subset[,i], probs = seq(0,1,0.25),na.rm=TRUE)[2], 1)
  "third_quartile"<-round(quantile(subset[,i], probs = seq(0,1,0.25),na.rm=TRUE)[4], 1)
  "prop_NAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0, round(prop.table(table(is.na(subset[,i])))[2] * 100, digits = 2))
  "NNAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0,table(is.na(subset[,i]))[2])
  tmp_mat<-c(var = colnames(data)[i], "Min-Max"=paste(Min, Max, sep="-"),
             "median(IQR)"=gsub(" ", "", paste(mediane, "(", first_quartile, "-", third_quartile, ")")),
             NAs=gsub(" ", "", paste(NNAs, "(", prop_NAs, ")")))
  return(tmp_mat)
}

ft_parse_quanti_opt<-function(data, min.max, na.print, group)
{
  if (!isTRUE(min.max))
    data<-data[,!names(data) %in% "Min-Max"]
  if (!isTRUE(na.print))
  {
    data<-data[,!names(data) %in% "NAs"]
    for (i in 1:nrow(data))
      data[i,1]<-paste(data[i,1], "(median(IQR))", sep = " ")
  }
  else if (!isTRUE(na.print) && !is.null(group))
  {
    data[,'median(IQR)']<-paste(data[,'median(IQR)'], data[,'NAs'], sep = "; ")
    data<-data[,!names(data) %in% "NAs"]
    for (i in 1:nrow(data))
      data[i,1]<-paste(data[i,1], "(median(IQR); NAs(%))", sep = " ")
  }
  return(data)
}

#' @import tidyr
ft_univ_quanti_p.value<-function(data, group, min.max, na.print,tab_tmp)
{
  dicho<-ft_parse_quanti_opt(tab_tmp, min.max, na.print, group)
  total<-ft_quanti(data, NULL, NULL, min.max, na.print)
  biv<-ft_ana_biv(data, group)
  total$Group <- "Total"
  total<-merge(total, dicho, all=TRUE)
  biv<-ft_parse_quanti_opt(biv, min.max, na.print, group)
  total<-merge(total, biv, all.x=TRUE, by.x="var", by.y="nom")
  total<-total[,!names(total) %in% c("test", "signi")]
  Group=NULL
  if (isTRUE(min.max) && isTRUE(na.print))
    total<-pivot_wider(total, names_from = Group, values_from = c("median(IQR)", "Min-Max", "NAs"))
  else if (isTRUE(min.max) && !isTRUE(na.print))
    total<-pivot_wider(total, names_from = Group, values_from = c("median(IQR)", "Min-Max"))
  else if (!isTRUE(min.max) && isTRUE(na.print))
    total<-pivot_wider(total, names_from = Group, values_from = c("median(IQR)", "NAs"))
  else if (!isTRUE(min.max) && !isTRUE(na.print))
    total<-pivot_wider(total, names_from = Group, values_from = c("median(IQR)"))
  total$p<-ifelse(as.numeric(total$p) < 0.001, "< .001", round(as.numeric(total$p), digits = 3))
  return(total)
}

ft_univ_quanti_2<-function(data, group, p.value, min.max, na.print){
  tab_1<-data.frame("var"=NA, "Min-Max"=NA, "median(IQR)"=NA, "NAs"=NA)
  colnames(tab_1)=c("var", "Min-Max", "median(IQR)", "NAs")
  tab_2<-data.frame("var"=NA, "Min-Max"=NA, "median(IQR)"=NA, "NAs"=NA)
  colnames(tab_2)=c("var", "Min-Max", "median(IQR)", "NAs")
  j = 0;
  for (i in 1:ncol(data))
  {
    if (colnames(data)[i]==group || !is.numeric(data[,i]))
      next ;
    j = j + 1
    tmp_1<-ft_tab_quanti(data, i, group, levels(data[,group])[1])
    tmp_2<-ft_tab_quanti(data, i, group, levels(data[,group])[2])
    for (k in 1:4)
      tab_1[j,k]<-tmp_1[k]
    for (k in 1:4)
      tab_2[j,k]<-tmp_2[k]
  }
  tab_1$Group=levels(data[,group])[1]
  tab_2$Group=levels(data[,group])[2]
  tmp<-merge(tab_1, tab_2, all=TRUE)
  if (!isTRUE(p.value))
  {
    tmp<-ft_parse_quanti_opt(tmp, min.max, na.print, group)
    return (tmp)
  }
  else
    return (ft_univ_quanti_p.value(data, group, min.max, na.print,tmp))
}

ft_quanti<-function(data, group=NULL, p.value, min.max, na.print){
  if (is.null(group))
  {
    tab<-data.frame("var"=NA, "Min-Max"=NA, "median(IQR)"=NA, "NAs"=NA)
    colnames(tab)=c("var", "Min-Max", "median(IQR)", "NAs")
    j = 0
    for (i in 1:ncol(data))
    {
      if (!is.numeric(data[,i]))
        next;
      j = j + 1
      tmp<-ft_tab_quanti(data,i)
      tab[j,"var"]<-tmp[1]
      tab[j,"Min-Max"]<-tmp[2]
      tab[j,"median(IQR)"]<-tmp[3]
      tab[j,"NAs"]<-tmp[4]
    }
    tab<-ft_parse_quanti_opt(tab, min.max, na.print)
    return(tab)
  }
  else
    return(ft_univ_quanti_2(data, group, p.value, min.max, na.print))
}
