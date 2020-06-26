###  A optimiser avant de repasser aux quali

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

ft_parse_quanti_opt<-function(data, min.max, na.print)
{
  if (!isTRUE(min.max))
    data<-data[,!names(data) %in% "Min-Max"]
  if (!isTRUE(na.print))
  {
    data<-data[,!names(data) %in% "NAs"]
    for (i in 1:nrow(data))
      data[i,1]<-paste(data[i,1], "(median(IQR))", sep = " ")
  }
  else
  {
    data[,'median(IQR)']<-paste(data[,'median(IQR)'], data[,'NAs'], sep = "; ")
    data<-data[,!names(data) %in% "NAs"]
    for (i in 1:nrow(data))
      data[i,1]<-paste(data[i,1], "(median(IQR); NAs(%))", sep = " ")
  }
  return(data)
}

ft_univ_quanti_p.value<-function(data, group, min.max, na.print,tab_tmp)
{
  dicho<-tab_tmp
  total<-ft_quanti(data, NULL, NULL, min.max, na.print)
  biv<-ft_ana_biv(data, group)
  total$Group <- "Total"
  total<-merge(total, dicho, all=TRUE)
  total<-ft_parse_quanti_opt(total, min.max, na.print)
  total<-merge(total, biv, all.x=TRUE, by.x="var", by.y="nom")
  total<-total[,!names(total) %in% c("test", "signi")]
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
  for (i in 1:ncol(data))
  {
    if (colnames(data)[i]==group || !is.numeric(data[,i]))
      next ;
    for (j in 1:2)
    {
      tab<-ft_tab_quanti(data, i, group, levels(data[,group])[j])
      nomtest<-paste("tab", levels(data[,group])[j], i,sep="_")
      assign(nomtest, tab)
    }
  }
  for (j in 1:2)
  {
    analyse<-data.frame(mget(ls(pattern = paste("tab", levels(data[,group])[j], sep = "_"))))
    analyse<-as.data.frame(t(analyse))
    rownames(analyse) <- c()
    analyse<-analyse %>% mutate(Group = levels(data[,group])[j])
    if (j == 1)
      tmp<-analyse
    else
      tmp<-merge(tmp, analyse, all=TRUE)
  }
  if (!isTRUE(p.value))
  {
    tmp<-ft_parse_quanti_opt(tmp, min.max, na.print)
    return (tmp)
  }
  else
    return (ft_univ_quanti_p.value(data, group, min.max, na.print,tmp))
}

ft_quanti<-function(data, group=NULL, p.value, min.max, na.print){
  if (is.null(group))
  {
    for (i in 1:ncol(data))
    {
      if (!is.numeric(data[,i]))
        next;
      tab<-ft_tab_quanti(data,i)
      nomtest<-paste("tab",i,sep="_")
      assign(nomtest,tab)
    }
    analyse<-data.frame(mget(ls(pattern = "tab_")))
    analyse<-as.data.frame(t(analyse))
    rownames(analyse) <- c()
    analyse<-ft_parse_quanti_opt(as.data.frame(analyse), min.max, na.print)
    return(analyse)
  }
  else
    return(ft_univ_quanti_2(data, group, p.value, min.max, na.print))
}
