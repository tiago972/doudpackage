#### Fonction annexe pour renommer la var contene dans biv comme celles du dataframe de sortie ###
ft_rename_quanti_biv<-function(biv, na.print)
{
  if (!isTRUE(na.print))
  {
    for (i in 1:nrow(biv))
      biv[i,1]<-paste(biv[i,1], "(median(IQR))", sep = " ")
  }
  else if (isTRUE(na.print))
  {
    for (i in 1:nrow(biv))
      biv[i,1]<-paste(biv[i,1], "(median(IQR); NAs(%))", sep = " ")
  }
  return(biv)
}

#### Fonction principale pour les differents elements de l analyse univariee ####
ft_tab_quanti<-function(data, i, group=NULL, group_level=NULL, digits.opt)
{
  if (!is.null(group))
    subset<-data[which(data[,group]==group_level),]
  else
    subset<-data
  "Min"<-min(subset[,i], na.rm=TRUE)
  "Max"<-max(subset[,i], na.rm=TRUE)
  "mediane"<-round(mean(subset[,i], na.rm = T), digits = digits.opt)
  "sd"<-round(sd(subset[,i], na.rm = T), digits = digits.opt)
  "prop_NAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0, round(prop.table(table(is.na(subset[,i])))[2] * 100, digits = digits.opt))
  "NNAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0,table(is.na(subset[,i]))[2])
  tmp_mat<-c(var = colnames(data)[i], "Min-Max"=paste(Min, Max, sep="-"),
             "median(IQR)"=gsub(" ", "", paste(mediane, "(",  sd,")")),
             NAs=gsub(" ", "", paste(NNAs, "(", prop_NAs, ")")))
  return(tmp_mat)
}

#### Fonction pour le filtrage des elements selon les options choisies ####
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
  else
  {
    ### Pour les Nas a la ligne c est ici qu il faudra agir
    data[,'Total']<-paste(data[,'Total'], data[,'NAs'], sep = "; ")
    data<-data[,!names(data) %in% "NAs"]
    print(head(data, 2))

    for (i in 1:nrow(data))
      data[i,1]<-paste(data[i,1], "(median(IQR); NAs(%))", sep = " ")
  }
  return(data)
}

#### si pvalue est true #####
#' @import tidyr
ft_univ_quanti_p.value<-function(data, group, min.max, na.print,tab_tmp, digits.opt)
{
  dicho<-ft_parse_quanti_opt(tab_tmp, min.max, na.print, group)
  total<-ft_quanti(data, NULL, NULL, min.max, na.print, digits.opt)
  biv<-ft_ana_biv(data, group)
  total$Group <- "Total"
  total<-merge(total, dicho, all=TRUE)
  biv<-ft_rename_quanti_biv(biv, na.print)
  total<-merge(total, biv, all.x=TRUE)
  total<-total[,!names(total) %in% c("test", "signi")]
  if (isTRUE(min.max))
    total<-pivot_wider(total, names_from = "Group", values_from = c("Total", "Min-Max"))
  else
    total<-pivot_wider(total, names_from = "Group", values_from = c("Total"))
  total$p<-ifelse(as.numeric(total$p) < 0.001, "< .001", round(as.numeric(total$p), digits = 3))
  return(total)
}

### simple fonction coupee ####
ft_univ_quanti_2<-function(data, group, p.value, min.max, na.print, digits.opt){
  tab_1<-data.frame("var"=NA, "Min-Max"=NA, "Total"=NA, "NAs"=NA)
  colnames(tab_1)=c("var", "Min-Max", "Total", "NAs")
  tab_2<-data.frame("var"=NA, "Min-Max"=NA, "Total"=NA, "NAs"=NA)
  colnames(tab_2)=c("var", "Min-Max", "Total", "NAs")
  j = 0;
  for (i in 1:ncol(data))
  {
    if (colnames(data)[i]==group || !is.numeric(data[,i]))
      next ;
    j = j + 1
    tmp_1<-ft_tab_quanti(data, i, group, levels(data[,group])[1], digits.opt)
    tmp_2<-ft_tab_quanti(data, i, group, levels(data[,group])[2], digits.opt)
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
    if (!isTRUE(min.max))
      tmp<-pivot_wider(tmp, names_from = "Group", values_from = "Total")
    else
      tmp<-pivot_wider(tmp, names_from = "Group", values_from = c("Total", "Min-Max"))
    print("debug 5")
    return (tmp)
  }
  else
    return (ft_univ_quanti_p.value(data, group, min.max, na.print,tmp, digits.opt))
}

ft_quanti<-function(data, group=NULL, p.value, min.max, na.print, digits.opt){
  if (is.null(group))
  {
    tab<-data.frame("var"=NA, "Min-Max"=NA, "Total"=NA, "NAs"=NA)
    colnames(tab)=c("var", "Min-Max", "Total", "NAs")
    j = 0
    for (i in 1:ncol(data))
    {
      if (!is.numeric(data[,i]))
        next;
      j = j + 1
      tmp<-ft_tab_quanti(data,i, NULL, NULL, digits.opt)
      for (k in 1:4)
        tab[j,k]<-tmp[k]
    }
    tab<-ft_parse_quanti_opt(tab, min.max, na.print, NULL)
    return(tab)
  }
  else
    return(ft_univ_quanti_2(data, group, p.value, min.max, na.print, digits.opt))
}
