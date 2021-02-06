#### Fonction principale pour les differents elements de l analyse univariee ####
ft_tab_quanti<-function(data, i, group=NULL, group_level=NULL, digits.opt)
{
  if (!is.null(group))
    subset<-data[which(data[,group]==group_level),]
  else
    subset<-data
  "Min"<-min(subset[,i], na.rm=TRUE)
  "Max"<-max(subset[,i], na.rm=TRUE)
  "mean"<-round(mean(subset[,i], na.rm = T), digits = digits.opt)
  "sd"<-round(sd(subset[,i], na.rm = T), digits = digits.opt)
  "prop_NAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0, round(prop.table(table(is.na(subset[,i])))[2] * 100, digits = digits.opt))
  "NNAs"<-ifelse(is.na(table(is.na(subset[,i]))[2]), 0,table(is.na(subset[,i]))[2])
  if (NNAs == 0)
  {
    NNAs = NA;
    prop_NAs = NA;
  }
  tmp_mat<-c(var = colnames(data)[i],
             "mean(SD)"=gsub(" ", "", paste(mean, "(",  sd,")")), "Min-Max"=paste(Min, Max, sep="-"))
  if (is.null(group))
  {
    tmp_NA <-c("Missing values (%)", gsub(" ", "", paste(NNAs, "(", prop_NAs, ")")),
             gsub(" ", "", paste(NNAs, "(", prop_NAs, ")")))
    tmp_mat<-rbind(tmp_mat, tmp_NA)
    rownames(tmp_mat)<-NULL
  }
  return(tmp_mat)
}

#### Fonction pour le filtrage des elements selon les options choisies ####
ft_parse_quanti_opt<-function(data, min.max, na.print, group, biv.opt = F)
{
  i = 1;
  while (i <= nrow(data))
  {
    data[i,1]<-paste(data[i,1], ", mean(SD)", sep = "")
    if (is.null(group) && isFALSE(biv.opt))
    {
      i = i + 2; ## Syntax bug, do not change the {}
    }
    else
      i = i + 1;
  }
  if (isTRUE(biv.opt))
    return(data)
  if (!isTRUE(min.max))
    data<-data[,!names(data) %in% "Min-Max"]
  if (!isTRUE(na.print))
    data<-data[!grepl("Missing Values n(%)", data[,"var"], fixed = T),]
  else
    data<-data[!grepl("NA(NA)", data[,"Total"], fixed = T),]
  return(data)
}

#### si pvalue est true #####
#' @import tidyr
#' @import plyr
ft_univ_quanti_p.value<-function(data, group, min.max, na.print,tab_tmp, digits.opt)
{
  dicho<-ft_parse_quanti_opt(tab_tmp, min.max, na.print, group)
  total<-ft_quanti(data, NULL, NULL, min.max, na.print, digits.opt)
  biv<-ft_ana_biv(data, group)
  total$Group <- "Total"
  total<-ft_merge_tot(dicho, total)
  biv<-ft_parse_quanti_opt(biv, min.max, na.print, group=NULL, biv.opt = T)
  biv<-biv[biv$var %in% total$var,]
  biv<-biv[,!names(biv) %in% c("test", "signi")]
  total<-plyr::join(total, biv, by = "var")
  if (isTRUE(min.max))
    total<-pivot_wider(total, names_from = "Group", values_from = c("Total", "Min-Max"))
  else
    total<-pivot_wider(total, names_from = "Group", values_from = c("Total"))
  return(total)
}

### simple fonction coupee ####
ft_univ_quanti_2<-function(data, group, p.value, min.max, na.print, digits.opt){
  tab_1<-data.frame("var"=NA, "Total"=NA, "Min-Max"=NA)
  colnames(tab_1)=c("var", "Total", "Min-Max")
  tab_2<-data.frame("var"=NA, "Total"=NA, "Min-Max"=NA)
  colnames(tab_2)=c("var", "Total", "Min-Max")
  j = 1;
  for (i in 1:ncol(data))
  {
    if (colnames(data)[i]==group || !is.numeric(data[,i]))
      next ;
    tmp_1<-ft_tab_quanti(data, i, group, levels(data[,group])[1], digits.opt)
    tmp_2<-ft_tab_quanti(data, i, group, levels(data[,group])[2], digits.opt)
    for (k in 1:3)
    {
      tab_1[j,k]<-tmp_1[k]
      tab_2[j,k]<-tmp_2[k]
    }
    j = j + 1
  }
  tab_1$Group=levels(data[,group])[1]
  tab_2$Group=levels(data[,group])[2]
  # tab_2<-tab_2[c(rep(T, 1),F),]
  tmp<-ft_merge(tab_1, tab_2)
  if (!isTRUE(p.value))
  {
    tmp<-ft_parse_quanti_opt(tmp, min.max, na.print, group)
    if (!isTRUE(min.max))
      tmp<-pivot_wider(tmp, names_from = "Group", values_from = "Total")
    else
      tmp<-pivot_wider(tmp, names_from = "Group", values_from = c("Total", "Min-Max"))
    return (tmp)
  }
  else
    return (ft_univ_quanti_p.value(data, group, min.max, na.print,tmp, digits.opt))
}

ft_quanti<-function(data, group=NULL, p.value, min.max, na.print, digits.opt){
  if (is.null(group))
  {
    tab<-data.frame("var"=NA, "Total"=NA, "Min-Max"=NA)
    colnames(tab)=c("var", "Total", "Min-Max")
    j = 1
    for (i in 1:ncol(data))
    {
      if (!is.numeric(data[,i]))
        next;
      tmp<-ft_tab_quanti(data,i, NULL, NULL, digits.opt)
      for (k in 1:3)
      {
        tab[j,k]<-tmp[1,k]
        tab[j+1,k]<-tmp[2,k]
      }
      j = j + 2
    }
    tab<-ft_parse_quanti_opt(tab, min.max, na.print, NULL)
    return(tab)
  }
  else
    return(ft_univ_quanti_2(data, group, p.value, min.max, na.print, digits.opt))
}
