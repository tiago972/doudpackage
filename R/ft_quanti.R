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

ft_univ_quanti_complete_true<-function(data, group, min.max=FALSE, tab_tmp)
{
  dicho<-tab_tmp
  total<-ft_quanti.group_false(data)
  biv<-ft_ana_biv(data, group)
  total$Group <- "Total"
  total<-merge(total, dicho, all=TRUE)
  if (min.max==FALSE)
    total<-select(total, -"Min-Max")
  total<-mutate(total, 'median(IQR)' = paste(total$`median(IQR)`, total$NAs, sep = "; ")) %>%
    select(-NAs)
  total<-merge(total, biv, all.x=TRUE, by.x="var", by.y="nom") %>%
    select(-test, -signi) %>%
    pivot_wider(names_from = Group, values_from = `median(IQR)`)
  total$p<-ifelse(as.numeric(total$p) < 0.001, "< .001", round(as.numeric(total$p), digits = 3))
  for (i in 1:nrow(total))
    total[i,1]<-paste(total[i,1], "(median(IQR); NAs(%))", sep = " ")
  return(total)
}

ft_univ_quanti_complete_false<-function(data, group, complete, min.max=FALSE){
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
  if (!isTRUE(complete))
    return (tmp)
  else
    return (ft_univ_quanti_complete_true(data, group, min.max, tmp))
}

ft_quanti.group_false<-function(data, group=NULL, complete=TRUE, min.max=FALSE){
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
    return(as.data.frame(analyse))
  }
  else
    return(ft_univ_quanti_complete_false(data, group, complete))
}
